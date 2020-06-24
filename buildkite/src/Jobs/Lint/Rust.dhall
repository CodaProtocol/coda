let Prelude = ../../External/Prelude.dhall

let S = ../../Lib/SelectFiles.dhall
let Cmd = ../../Lib/Cmds.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let Command = ../../Command/Base.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

Pipeline.build
  Pipeline.Config::{
    spec = JobSpec::{
      dirtyWhen = [ S.contains "src/app/trace-tool" ],
      path = "Lint",
      name = "Rust"
    },
    steps = [
    Command.build
      Command.Config::{
        commands = [ Cmd.run "cd src/app/trace-tool ; cargo check --frozen" ],
        label = "Rust lint steps",
        key = "lint",
        target = Size.Small,
        docker = Some Docker::{ image = (../../Constants/ContainerImages.dhall).toolchainBase }
      }
    ]
  }

