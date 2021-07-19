let Prelude = ../../External/Prelude.dhall

let S = ../../Lib/SelectFiles.dhall
let Cmd = ../../Lib/Cmds.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall

let jobDocker = Cmd.Docker::{image = (../../Constants/ContainerImages.dhall).rustToolchain}

in

Pipeline.build
  Pipeline.Config::{
    spec = JobSpec::{
      dirtyWhen = [ S.contains "src/app/trace-tool", S.strictlyStart (S.contains "buildkite/src/Jobs/TraceTool") ],
      path = "Release",
      name = "TraceTool"
    },
    steps = [
      Command.build
        Command.Config::{
          commands = [ Cmd.runInDocker jobDocker "cd src/app/trace-tool && cargo build --frozen" ]
          , label = "Build trace-tool"
          , key = "build-trace-tool"
          , target = Size.Small
          , docker = None Docker.Type
          , artifact_paths = [ S.contains "src/app/trace-tool/target/debug/trace-tool" ]
        }
    ]
  }
