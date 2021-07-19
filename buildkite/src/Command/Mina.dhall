let Prelude = ../External/Prelude.dhall
let List/map = Prelude.List.map
let B = ../External/Buildkite.dhall
let B/Plugins/Partial = B.definitions/commandStep/properties/plugins/Type
let Map = Prelude.Map

let Cmd = ../Lib/Cmds.dhall
let Docker = ./Docker/Type.dhall
let Summon = ./Summon/Type.dhall
let Base = ./Base.dhall

let Size = ./Size.dhall

let dockerImage = (../Constants/ContainerImages.dhall).minaToolchainBuster

let fixPermissionsScript = "sudo chown -R opam ."

let Config = {
  Type = {
    commands : List Cmd.Type,
    label : Text,
    key : Text
  },
  default = {=}
}


let build : Config.Type -> Base.Type = \(c : Config.Type) ->
  Base.build
    Base.Config::{
      commands = [ Cmd.run fixPermissionsScript ] # c.commands,
      label = c.label,
      key = c.key,
      target = Size.Small,
      docker = Some Docker::{ image = dockerImage }
    }

in {
    fixPermissionsCommand = Cmd.runInDocker Cmd.Docker::{ image = dockerImage } fixPermissionsScript,
    Config = Config,
    build = build,
    Type = Base.Type
}

