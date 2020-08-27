/*
 UploadLeaderboardData.re has the responsibilities of uploading the
 information needed for the Leaderboard webpage to a Google Sheets tab.

 This information is seperate from points as it's data that is only
 useful for the Leaderboard webpage.

  */

module StringMap = Map.Make(String);
open Sheets;
open Sheets.Core;

/*
   Upload "Genesis Members", "Block Count", and "Participants" to the Data tab
 */
let uploadData = (spreadsheetId, totalBlocks) => {
  let client = createClient();

  getRange(
    client,
    initSheetsQuery(
      spreadsheetId,
      "All-Time Leaderboard!C4:G",
      "FORMATTED_VALUE",
    ),
    result => {
    switch (result) {
    | Ok(sheetsData) =>
      let data = sheetsData |> decodeGoogleSheets;

      let columnHeaders = [|
        "Genesis Members",
        "Block Count",
        "Participants",
        "Last Updated",
      |];

      let statisticsData = [|
        data->Belt.Array.keep(row => {
          /* The 4th column indicates whether the user is a genesis member */
          switch (Belt.Array.get(row, 3)) {
          | Some(genesisMember) =>
            String.length(Belt.Option.getExn(genesisMember)) == 0
              ? false : true
          | None => false
          }
        })
        |> Array.length
        |> string_of_int,
        totalBlocks,
        data |> Array.length |> string_of_int,
      |];

      updateRange(
        client,
        initSheetsUpdate(
          spreadsheetId,
          "Data!A1:C",
          "USER_ENTERED",
          Array.append([|columnHeaders|], [|statisticsData|]),
        ),
        result => {
        switch (result) {
        | Ok(_) => Js.log({j|Uploaded to Data spreadsheet|j})
        | Error(error) => Js.log(error)
        }
      });
    | Error(error) => Js.log(error)
    }
  });
};

let computeMapping = (usernameIndex, users, propertyMap) => {
  users
  |> Array.map(userRow => {
       let username = Belt.Option.getExn(userRow[usernameIndex]);
       if (StringMap.mem(username, propertyMap)) {
         let property = StringMap.find(username, propertyMap);
         Belt.Array.concat(userRow, [|Some(property)|]);
       } else {
         Belt.Array.concat(userRow, [|None|]);
       };
     });
};

let computeProperty = (propertyIndex, userIndex, pointsData, users) => {
  pointsData
  |> Array.fold_left(
       (map, userRow) => {
         switch (
           Belt.Array.get(userRow, userIndex),
           Belt.Array.get(userRow, propertyIndex),
         ) {
         | (Some(usernameOption), Some(propertyOption)) =>
           switch (usernameOption, propertyOption) {
           | (Some(username), Some(property)) =>
             StringMap.add(username, property, map)
           | (_, _) => map
           }
         | (_, _) => map
         }
       },
       StringMap.empty,
     )
  |> computeMapping(0, users);
};

let computeUsers = (userIndex, userData) => {
  userData
  |> Array.fold_left(
       (a, row) => {
         switch (Belt.Array.get(row, userIndex)) {
         | Some(usernameOption) =>
           switch (usernameOption) {
           | Some(username) => Array.append(a, [|[|Some(username)|]|])
           | None => a
           }
         | None => a
         }
       },
       [||],
     );
};

let computeMemberProfileData = (allTimeData, phaseData, releaseData) => {
  let allTimeUserIndex = 4; /* usernames are located in the 5th column */
  let phaseUserIndex = 2; /* usernames are located in the 3rd column */
  let releaseUserIndex = 1; /* usernames are located in the 2nd column */

  /* compute users */
  computeUsers(allTimeUserIndex, allTimeData)
  /* compute genesis */
  |> computeProperty(3, allTimeUserIndex, allTimeData)
  /* compute all time points */
  |> computeProperty(5, allTimeUserIndex, allTimeData)
  /* compute phase points */
  |> computeProperty(3, phaseUserIndex, phaseData)
  /* compute release points */
  |> computeProperty(6, phaseUserIndex, phaseData)
  /* compute all time rank */
  |> computeProperty(0, allTimeUserIndex, allTimeData)
  /* compute phase rank */
  |> computeProperty(0, phaseUserIndex, phaseData)
  /* compute release rank */
  |> computeProperty(0, releaseUserIndex, releaseData);
};

let uploadUserProfileData = spreadsheetId => {
  let client = createClient();
  /* Fetch all-time leaderboard data */
  getRange(
    client,
    initSheetsQuery(
      spreadsheetId,
      "All-Time Leaderboard!C4:H",
      "FORMATTED_VALUE",
    ),
    result => {
    switch (result) {
    | Ok(allTimeResult) =>
      let allTimeData = allTimeResult |> decodeGoogleSheets;
      /* Fetch current phase leaderboard data */
      getRange(
        client,
        initSheetsQuery(
          spreadsheetId,
          "Phase 3 Leaderboard!B4:Z",
          "FORMATTED_VALUE",
        ),
        result => {
        switch (result) {
        | Ok(phaseResult) =>
          let phaseData = phaseResult |> decodeGoogleSheets;
          /* Fetch current release leaderboard data */
          getRange(
            client,
            initSheetsQuery(spreadsheetId, "3.2b!A4:B", "FORMATTED_VALUE"),
            result => {
            switch (result) {
            | Ok(releaseResult) =>
              let releaseData = releaseResult |> decodeGoogleSheets;
              let data =
                computeMemberProfileData(allTimeData, phaseData, releaseData);

              updateRange(
                client,
                initSheetsUpdate(
                  spreadsheetId,
                  "Member_Profile_Data!A2:Z",
                  "USER_ENTERED",
                  encodeGoogleSheets(data),
                ),
                result => {
                switch (result) {
                | Ok(_) => Js.log({j|Uploaded member data|j})
                | Error(error) => Js.log(error)
                }
              });
            | Error(error) => Js.log(error)
            }
          });
        | Error(error) => Js.log(error)
        }
      });
    | Error(error) => Js.log(error)
    }
  });
};
