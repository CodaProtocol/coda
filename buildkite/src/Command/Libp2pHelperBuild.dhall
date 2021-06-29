let Prelude = ../External/Prelude.dhall

let Command = ./Base.dhall
let Docker = ./Docker/Type.dhall
let Size = ./Size.dhall

let Cmd = ../Lib/Cmds.dhall

let commands : List Cmd.Type =
  [
    Cmd.run "chmod -R 777 src/app/libp2p_helper",
    Cmd.runInDocker
      Cmd.Docker::{
        image = (../Constants/ContainerImages.dhall).minaToolchain,
        extraEnv = [ "GO=/usr/lib/go/bin/go" ]
      }
      "make libp2p_helper",
    Cmd.run "cp src/app/libp2p_helper/result/bin/libp2p_helper . && buildkite/scripts/buildkite-artifact-helper.sh libp2p_helper"
  ]

in

let cmdConfig =
  Command.build
    Command.Config::{
      commands  = commands,
      label = "Build Libp2p helper",
      key = "libp2p-helper",
      target = Size.Small
    }

in

{ step = cmdConfig }
