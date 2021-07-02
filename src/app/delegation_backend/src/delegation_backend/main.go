package main

import (
  . "delegation_backend"
  logging "github.com/ipfs/go-log/v2"
  "net/http"
  "context"
  "time"
  "cloud.google.com/go/storage"
  "github.com/btcsuite/btcutil/base58"
  sheets "google.golang.org/api/sheets/v4"
  "google.golang.org/api/option"
)

const CLOUD_BUCKET_NAME = "georgeee-o1labs-1"

func retrieveWhitelist (service *sheets.Service, log *logging.ZapEventLogger) Whitelist {
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

func main() {
  logging.SetupLogging(logging.Config{
    Format: logging.JSONOutput,
    Stderr: true,
    Stdout: false,
    Level:  logging.LevelDebug,
    File:   "",
  })
  log := logging.Logger("delegation backend")
  log.Infof("delegation backend has the following logging subsystems active: %v", logging.GetSubsystems())

  app := new(App)
  app.Log = log
  app.Context = context.Background()
  http.Handle("/submit", app.NewSubmitH())
  client, err1 := storage.NewClient(app.Context)
  if err1 != nil {
    log.Fatalf("Error creating Cloud client: %v", err1)
    return
  }
  // TODO check that Bucket and Sheets service are ok to be used concurrently
  app.Bucket = client.Bucket(CLOUD_BUCKET_NAME)
  app.SubmitCounter = NewAttemptCounter(REQUESTS_PER_PK_HOURLY)
  sheetsService, err2 := sheets.NewService(app.Context, option.WithScopes(sheets.SpreadsheetsReadonlyScope))
  if err2 != nil {
    log.Fatalf("Error creating Sheets service: %v", err2)
    return
  }
  initWl := retrieveWhitelist(sheetsService, log)
  wlMvar := new(WhitelistMVar)
  wlMvar.Replace(&initWl)
  app.Whitelist = wlMvar
  go func(){
    for {
      wl := retrieveWhitelist(sheetsService, log)
      wlMvar.Replace(&wl)
      time.Sleep(WHITELIST_REFRESH_INTERVAL)
    }
  }()
  log.Fatal(http.ListenAndServe(DELEGATION_BACKEND_LISTEN_TO, nil))
}
