let Prelude = ../External/Prelude.dhall
let Cmd = ../Lib/Cmds.dhall
let Mina = ../Command/Mina.dhall
let S = ../Lib/SelectFiles.dhall

let r = Cmd.run

let runInToolchainBuster : List Text -> Text -> List Cmd.Type =
  \(environment : List Text) ->
  \(innerScript : Text) ->
    [ Mina.fixPermissionsCommand ] # [
      Cmd.runInDocker
        (Cmd.Docker::{ image = (../Constants/ContainerImages.dhall).minaToolchainBuster, extraEnv = environment })
        (innerScript)
    ]

let runInToolchainStretch : List Text -> Text -> List Cmd.Type =
  \(environment : List Text) ->
  \(innerScript : Text) ->
    [ Mina.fixPermissionsCommand ] # [
      Cmd.runInDocker
        (Cmd.Docker::{ image = (../Constants/ContainerImages.dhall).minaToolchainStretch, extraEnv = environment })
        (innerScript)
    ]

in

{ runInToolchainBuster = runInToolchainBuster
, runInToolchainStretch = runInToolchainStretch
}
