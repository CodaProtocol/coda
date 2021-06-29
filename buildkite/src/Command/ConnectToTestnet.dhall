let Prelude = ../External/Prelude.dhall

let Command = ./Base.dhall
let Docker = ./Docker/Type.dhall
let Size = ./Size.dhall

let Cmd = ../Lib/Cmds.dhall in

{ step = \(dependsOn : List Command.TaggedKey.Type) ->
    Command.build
      Command.Config::{
        commands = [
          Cmd.runInDocker
            Cmd.Docker::{
              image = (../Constants/ContainerImages.dhall).ubuntu1804
            }
            "./buildkite/scripts/connect-to-mainnet-on-compatible.sh"
        ],
        label = "Connect to testnet",
        key = "connect-to-mainnet",
        target = Size.Large,
        depends_on = dependsOn
      }
}
