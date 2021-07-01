package main

import (
  . "delegation_backend"
  logging "github.com/ipfs/go-log/v2"
  "net/http"
  "context"
  "cloud.google.com/go/storage"
)

const CLOUD_BUCKET_NAME = "georgeee-o1labs-1"

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
  app.Bucket = client.Bucket(CLOUD_BUCKET_NAME)
  app.SubmitCounter = NewAttemptCounter(REQUESTS_PER_PK_HOURLY)
  log.Fatal(http.ListenAndServe(DELEGATION_BACKEND_LISTEN_TO, nil))
}
