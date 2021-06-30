package main

import (
  . "delegation_backend"
  logging "github.com/ipfs/go-log/v2"
  "net/http"
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
  http.Handle("/submit", app.NewSubmitH())

  log.Fatal(http.ListenAndServe(DELEGATION_BACKEND_LISTEN_TO, nil))
}
