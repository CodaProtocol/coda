let ContainerImages = ../../Constants/ContainerImages.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = [
          S.strictlyStart (S.contains "src/app/delegation_backend"),
          S.exactly "buildkite/src/Jobs/Test/DelegationBackendUnitTest" "dhall"
        ],
        path = "Test",
        name = "DelegationBackendUnitTest"
      },
    steps = [
      Command.build
        Command.Config::{
          commands = [
            Cmd.runInDocker Cmd.Docker::{image = ContainerImages.minaToolchain} "GO=/usr/lib/go/bin/go make -C src/app/delegation_backend test"
          ],
          label = "delegation-backend unit-tests",
          key = "delegation-backend-unit-tests",
          target = Size.Large,
          docker = None Docker.Type
        }
    ]
  }
