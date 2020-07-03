/*
 UploadLeaderboardData.re has the responsibilities of uploading the
 information needed for the Leaderboard webpage to a Google Sheets tab.

 This information is seperate from points as it's data that is only
 useful for the Leaderboard webpage.

  */

module StringMap = Map.Make(String);
open Sheets.Bindings;
open Sheets.Core;

/*
   Upload the total block count to the "Data" sheet
 */
let uploadTotalBlocks = (spreadsheetId, totalBlocks) => {
  let client = createClient();
  getRange(
    client, initSheetsQuery(spreadsheetId, "Data!A1:B", "FORMULA"), result => {
    switch (result) {
    | Ok(sheetsData) =>
      let newSheetsData = sheetsData |> decodeGoogleSheets;
      newSheetsData[0][1] = totalBlocks;
      updateRange(
        client,
        initSheetsUpdate(
          spreadsheetId,
          "Data!A1:B",
          "USER_ENTERED",
          newSheetsData,
        ),
        result => {
        switch (result) {
        | Ok(_) => Js.log({j|Uploaded total blocks|j})
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
         userRow;
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

let computeRank = (pointIndex, userIndex, pointData, users) => {
  Belt.SortArray.stableSortBy(pointData, (firstUser, secondUser) => {
    switch (
      Belt.Array.get(firstUser, pointIndex),
      Belt.Array.get(secondUser, pointIndex),
    ) {
    | (Some(firstUserPoints), Some(secondUserPoints)) =>
      compare(
        int_of_string(Belt.Option.getExn(secondUserPoints)),
        int_of_string(Belt.Option.getExn(firstUserPoints)),
      )
    | (_, _) => 0
    }
  })
  |> Array.mapi((place, userRow) => {
       let username = Belt.Option.getExn(userRow[userIndex]);
       (username, string_of_int(place + 1));
     })
  |> Array.fold_left(
       (map, user) => {
         let (name, place) = user;
         StringMap.add(name, place, map);
       },
       StringMap.empty,
     )
  |> computeMapping(0, users);
};

let computeMemberProfileData = (allTimeData, phaseData) => {
  let allTimeUserIndex = 4; /* usernames are located in the 4th column */
  let phaseUserIndex = 2; /* usernames are located in the 2nd column */

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
  |> computeRank(6, phaseUserIndex, phaseData);
};

let uploadUserProfileData = spreadsheetId => {
  let client = createClient();
  /* Fetch All-Time leaderboard data */
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
      /* Fetch current Phase leaderboard data */
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

          let data = computeMemberProfileData(allTimeData, phaseData);

          updateRange(
            client,
            initSheetsUpdate(
              spreadsheetId,
              "Member_Profile_Data!A2:Z",
              "USER_ENTERED",
              data,
            ),
            result => {
            switch (result) {
            | Ok(_) => Js.log({j|Uploaded member data|j})
            | Error(error) => Js.log(error)
            }
          });
          ();
        | Error(error) => Js.log(error)
        }
      });
    | Error(error) => Js.log(error)
    }
  });
};
