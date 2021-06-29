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
      let unitDirtyWhen = [
        S.strictlyStart (S.contains "src/app/libp2p_helper"),
        S.strictlyStart (S.contains "src/lib"),
        S.strictlyStart (S.contains "src/nonconsensus"),
        S.strictly (S.contains "Makefile"),
        S.exactly "buildkite/src/Jobs/Test/DaemonUnitTest" "dhall",
        S.exactly "scripts/link-coredumps" "sh",
        S.exactly "buildkite/scripts/unit-test" "sh"
      ]

      in

      JobSpec::{
        dirtyWhen = [
          S.strictlyStart (S.contains "scr/app/libp2p_helper"),
          S.exactly "Makefile" "",
          S.exactly "buildkite/src/Jobs/Test/Libp2pUnitTest" "dhall"
        ],
        path = "Test",
        name = "Libp2pUnitTest"
      },
    steps = [
      Command.build
        Command.Config::{
          commands = [
            Cmd.runInDocker Cmd.Docker::{image = ContainerImages.minaToolchain} "cd src/app/libp2p_helper/src && /usr/lib/go/bin/go mod download && /usr/lib/go/bin/go test . ./libp2p_helper"
          ],
          label = "libp2p unit-tests",
          key = "libp2p-unit-tests",
          target = Size.Large,
          docker = None Docker.Type
        }
    ]
  }
