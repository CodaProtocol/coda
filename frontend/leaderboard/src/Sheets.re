open Bindings.GoogleSheets;

let createClient = () => {
  googleAuth({scopes: [|"https://www.googleapis.com/auth/spreadsheets"|]});
};

let getRange = (client, sheetsQuery, cb) => {
  let sheets = sheets({version: "v4", auth: client});
  get(sheets, sheetsQuery, (~error, ~res) => {
    switch (Js.Nullable.toOption(error)) {
    | None => cb(Ok(res.data.values))
    | Some(error) => cb(Error(error))
    }
  });
};

let updateRange = (client, sheetsUpdate, cb) => {
  let sheets = sheets({version: "v4", auth: client});

  update(sheets, sheetsUpdate, (~error, ~res) => {
    switch (Js.Nullable.toOption(error)) {
    | None => cb(Ok(res.data.values))
    | Some(error) => cb(Error(error))
    }
  });
};

/*
   Core is a module that provides some helpers to be used when interacting
   with the Google Sheets API.
 */
module Core = {
  module Sheets = {
    type t = {
      name: string,
      range: string,
    };

    type sheets =
      | Main
      | AllTimeLeaderboard
      | CurrentPhaseLeaderboard
      | CurrentReleaseLeaderboard
      | MemberProfileData
      | Users
      | Data;

    let getSheet = sheet => {
      switch (sheet) {
      | Main => {name: "main", range: "main!A5:I"}
      | AllTimeLeaderboard => {
          name: "All-Time Leaderboard",
          range: "All-Time Leaderboard!C4:H",
        }
      | CurrentPhaseLeaderboard => {
          name: "Phase 3 Leaderboard",
          range: "Phase 3 Leaderboard!B4:E",
        }
      | CurrentReleaseLeaderboard => {name: "3.2b", range: "3.2b!A5:C"}
      | MemberProfileData => {
          name: "Member_Profile_Data",
          range: "Member_Profile_Data!A2:Z",
        }
      | Users => {name: "Users", range: "Users!A2:B"}
      | Data => {name: "Data", range: "Data!A1:D"}
      };
    };
  };

  let getColumnIndex = (columnToFind, data) => {
    Belt.Array.getIndexBy(data, headerName =>
      switch (headerName) {
      | Some(headerName) =>
        String.lowercase_ascii(headerName)
        == String.lowercase_ascii(columnToFind)
      | None => false
      }
    );
  };

  let getCellType = v =>
    switch (Js.Types.classify(v)) {
    | JSNumber(float) => `Float(float)
    | JSString(string) => `String(string)
    | _ => failwith("Sheets can only contain string or number")
    };

  /*
     Googlesheets API returns a typed 2d array of the cell data.
     The data being fetched will either be a number (for points)
     or a string (for a formula).
   */
  let decodeGoogleSheets = sheetsData => {
    sheetsData->Belt.Array.keep(row => Array.length(row) > 0)
    |> Array.map(row => {
         Array.map(
           cell => {
             switch (getCellType(cell)) {
             | `Float(float) => Some(Js.Float.toString(float))
             | `String(string) => Some(string)
             | _ => None
             }
           },
           row,
         )
       });
  };

  let encodeGoogleSheets = sheetsData => {
    sheetsData
    |> Array.map(row => {
         Array.map(
           cell => {
             switch (cell) {
             | Some(cell) => cell
             | None => ""
             }
           },
           row,
         )
       });
  };

  /*
     Googlesheets API will truncate rows that have empty trailing cells.
     Because of this, we make each row the same size from the fetched data.
   */
  let normalizeGoogleSheets = sheetsData => {
    // The first row is assumed to be the column titles of the sheet.
    let headerLength = Array.length(sheetsData[0]);
    sheetsData
    |> Array.map(row => {
         let rowLength = Array.length(row);
         if (rowLength < headerLength) {
           Array.append(
             row,
             ArrayLabels.make(headerLength - rowLength, None),
           );
         } else {
           row;
         };
       });
  };

  let initSheetsQuery = (spreadsheetId, range, valueRenderOption) => {
    {spreadsheetId, range, valueRenderOption};
  };

  let initSheetsUpdate = (spreadsheetId, range, valueInputOption, data) => {
    let resource: sheetsUploadData = {values: data};
    {spreadsheetId, range, valueInputOption, resource};
  };
};
