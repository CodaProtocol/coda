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
          S.strictlyStart (S.contains "buildkite/src/Jobs/Test/ClientSdk"),
          S.strictlyStart (S.contains "src")
        ],
        path = "Test",
        name = "ClientSdk"
      },
    steps = [
      Command.build
        Command.Config::{
          commands = [
            Cmd.run "chmod -R 777 frontend/client_sdk",
            Cmd.runInDocker
              Cmd.Docker::{image = (../../Constants/ContainerImages.dhall).minaToolchainBuster}
              "cd frontend/client_sdk && yarn install"
          ]
          , label = "Install Yarn dependencies"
          , key = "install-yarn-deps"
          , target = Size.Small
          , docker = None Docker.Type
        },
      Command.build
        Command.Config::{
          commands = RunInToolchain.runInToolchainBuster ([] : List Text) "./scripts/client-sdk-unit-tests.sh"
          , label = "Build client SDK and run unit tests"
          , key = "client-sdk-build-unittests"
          , target = Size.Medium
          , docker = None Docker.Type
        },
      Command.build
        Command.Config::{
          commands = RunInToolchain.runInToolchainBuster ([] : List Text) "./buildkite/scripts/client-sdk-tool.sh 'prepublishOnly'"
          , label = "Prepublish client SDK packages"
          , key = "prepublish-client-sdk"
          , target = Size.Medium
          , docker = None Docker.Type
        }
    ]
  }

