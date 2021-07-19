let Prelude = ../../External/Prelude.dhall

let S = ../../Lib/SelectFiles.dhall
let Cmd = ../../Lib/Cmds.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let RunInToolchain = ../../Command/RunInToolchain.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall

in

Pipeline.build
  Pipeline.Config::{
    spec = 
      JobSpec::{
        dirtyWhen = [
          S.strictlyStart (S.contains "buildkite/src/Jobs/Release/ClientSdk"),
          S.strictlyStart (S.contains "frontend/client_sdk"),
          S.strictlyStart (S.contains "src")
        ],
        path = "Release",
        name = "ClientSdk"
      },
    steps = [
      Command.build
        Command.Config::{
          commands = RunInToolchain.runInToolchainBuster (["NPM_TOKEN"]) "./buildkite/scripts/client-sdk-tool.sh 'publish --non-interactive'"
          , label = "Publish client SDK to npm"
          , key = "publish-client-sdk"
          , target = Size.Medium
          , docker = None Docker.Type
        }
    ]
  }

