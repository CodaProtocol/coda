package main

import (
  . "delegation_backend"
  logging "github.com/ipfs/go-log/v2"
)

func main() {
  logging.SetupLogging(logging.Config{
    Format: logging.JSONOutput,
    Stderr: true,
    Stdout: false,
    Level:  logging.LevelDebug,
    File:   "",
  })
  _ = MAX_SUBMIT_PAYLOAD_SIZE
  helperLog := logging.Logger("top-level")
  helperLog.Infof("delegation backend has the following logging subsystems active: %v", logging.GetSubsystems())
}
