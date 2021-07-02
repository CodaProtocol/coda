package main

import (
  . "delegation_backend"
  logging "github.com/ipfs/go-log/v2"
  "net/http"
  "context"
  "time"
  "cloud.google.com/go/storage"
  sheets "google.golang.org/api/sheets/v4"
  "google.golang.org/api/option"
)

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
  initWl := RetrieveWhitelist(sheetsService, log)
  wlMvar := new(WhitelistMVar)
  wlMvar.Replace(&initWl)
  app.Whitelist = wlMvar
  go func(){
    for {
      wl := RetrieveWhitelist(sheetsService, log)
      wlMvar.Replace(&wl)
      time.Sleep(WHITELIST_REFRESH_INTERVAL)
    }
  }()
  log.Fatal(http.ListenAndServe(DELEGATION_BACKEND_LISTEN_TO, nil))
}
