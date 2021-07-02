package delegation_backend

import (
  sheets "google.golang.org/api/sheets/v4"
  logging "github.com/ipfs/go-log/v2"
  "github.com/btcsuite/btcutil/base58"
)

func RetrieveWhitelist (service *sheets.Service, log *logging.ZapEventLogger) Whitelist {
  wl := make(Whitelist)
  col := DELEGATION_WHITELIST_COLUMN
  readRange := DELEGATION_WHITELIST_LIST + "!" + col + ":" + col
  spId := DELEGATION_WHITELIST_SPREADSHEET_ID
  resp, err := service.Spreadsheets.Values.Get(spId, readRange).Do()
  if err != nil {
    log.Fatalf("Unable to retrieve data from sheet: %v", err)
  }
  for _, row := range resp.Values {
    if len(row) > 0 {
      bs, ver, err := base58.CheckDecode(row[0].(string))
      if err == nil || ver == BASE58CHECK_VERSION_PK || len(bs) != PK_LENGTH {
        var pk Pk
        copy(pk[:], bs)
        wl[pk] = false // we need something to be provided as value
      }
    }
  }
  return wl
}
