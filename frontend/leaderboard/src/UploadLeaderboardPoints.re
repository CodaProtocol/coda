/*
 UploadLeaderboardPoints.re has the responsibilities of managing
 calculation of points from challenges and uploading the corresponding
 points to each user.

 The entry point to upload points is uploadChallengePoints(). The fetched
 Google Sheets data is in a form of an array of rows. Each entry
 in the array is another array that represents cell values for a particular user.

 The script first fetches from "Users!A2:B" to compute a mapping of usernames and
 public keys. The username/pk mapping is then used to assign points to users for
 completing challenges. Finally, the new points data is uploaded to the spreadsheet.
  */

module StringMap = Map.Make(String);
open Sheets;
open Sheets.Core;

let createPublickeyUsernameMap = sheetsData => {
  sheetsData
  |> Array.fold_left(
       (map, user) => {
         switch (user[0], user[1]) {
         | (Some(publickey), Some(username)) =>
           StringMap.add(publickey, username, map)
         | (_, _) => map
         }
       },
       StringMap.empty,
     );
};

let createUsernamePointsMap = (pointsMap, userMap) => {
  StringMap.fold(
    (username, pk, map) => {
      StringMap.mem(pk, pointsMap)
        ? StringMap.add(username, StringMap.find(pk, pointsMap), map) : map
    },
    userMap,
    StringMap.empty,
  );
};

let assignPointsToUsers =
    (pointIndex, userNameIndex, usernamePointsMap, sheetsData) => {
  /* Iterate through all the rows in the fetched Google Sheets data */
  Belt.Array.slice(sheetsData, ~offset=1, ~len=Array.length(sheetsData))
  |> Array.iter(row => {
       let username =
         switch (row[userNameIndex]) {
         | Some(username) => username
         | None => ""
         };

       if (StringMap.mem(username, usernamePointsMap)) {
         /* Found a username, get the points to update */
         let pointsBinding = StringMap.find(username, usernamePointsMap);
         /* Add points from challenge */
         row[pointIndex] = Some(string_of_int(pointsBinding));
       } else {
         /* If username has no points for the challenge, wipe cell */
         row[pointIndex] =
           None;
       };
     });
};

/*
  Filter the collected metrics that don't have a corresponding user from the "Users" tab
 */
let filterMetricsBySheetsUsers = (metricsMap, userMap) => {
  let userPublickeys =
    StringMap.bindings(userMap) |> List.map(((_, publickey)) => publickey);

  metricsMap
  |> StringMap.filter((publickey, _) => {
       List.mem(publickey, userPublickeys)
     });
};

let updatePointsByChallenge = (metricsMap, userMap, sheetsData, usernameIndex) => {
  let filteredMetrics = filterMetricsBySheetsUsers(metricsMap, userMap);
  /* Loop through the first row which contains all challenge headers and calculate valid challenges */
  sheetsData[0]
  |> Array.iteri((challengePointIndex, challengeTitle) => {
       switch (challengeTitle) {
       | Some(challengeTitle) =>
         switch (Challenges.calculatePoints(challengeTitle, filteredMetrics)) {
         | Some(pointsMap) =>
           let usernamePointsMap =
             createUsernamePointsMap(pointsMap, userMap);

           assignPointsToUsers(
             challengePointIndex,
             usernameIndex,
             usernamePointsMap,
             sheetsData,
           );
           ();
         | None => ()
         }
       | None => ()
       }
     });
};

let updateUserPoints = (metricsMap, userMap, sheetsData) => {
  switch (getColumnIndex("Name", sheetsData[0])) {
  | Some(usernameIndex) =>
    updatePointsByChallenge(metricsMap, userMap, sheetsData, usernameIndex)
  | None => ()
  };
};

let updateChallengeSheet = (client, spreadsheetId, range, userMap, metricsMap) => {
  getRange(client, initSheetsQuery(spreadsheetId, range, "FORMULA"), result => {
    switch (result) {
    /* leaderboardData is a 2d array that has usernames and points to be given for each column */
    | Ok(leaderboardData) =>
      let sheetsData =
        leaderboardData |> decodeGoogleSheets |> normalizeGoogleSheets;

      updateUserPoints(metricsMap, userMap, sheetsData);

      updateRange(
        client,
        initSheetsUpdate(
          spreadsheetId,
          range,
          "USER_ENTERED",
          encodeGoogleSheets(sheetsData),
        ),
        result => {
        switch (result) {
        | Ok(_) => Js.log({j|Data uploaded points for 3.2b|j})
        | Error(error) => Js.log(error)
        }
      });
    | Error(error) => Js.log(error)
    }
  });
};

let uploadChallengePoints = (spreadsheetId, metricsMap) => {
  let client = createClient();
  getRange(
    client, initSheetsQuery(spreadsheetId, "Users!A2:B", "FORMULA"), result => {
    switch (result) {
    /* userData is a 2d array of usernames and public keys to represent each user */
    | Ok(userData) =>
      let userMap =
        userData |> decodeGoogleSheets |> createPublickeyUsernameMap;

      updateChallengeSheet(
        client,
        spreadsheetId,
        Sheets.getSheet(Sheets.CurrentReleaseLeaderboard).name ++ "!A4:M",
        userMap,
        metricsMap,
      );
    | Error(error) => Js.log(error)
    }
  });
};
