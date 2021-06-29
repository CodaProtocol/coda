let Prelude = ../../External/Prelude.dhall

let S = ../../Lib/SelectFiles.dhall
let Cmd = ../../Lib/Cmds.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let Docker = ../../Command/Docker/Type.dhall
let OpamInit = ../../Command/OpamInit.dhall
let Size = ../../Command/Size.dhall


let buildToolCmd : Text -> Size -> Command.Type = \(tool : Text) -> \(cmd_target : Size) ->
  Command.build
    Command.Config::{
      commands = OpamInit.andThenRunInDocker ([ "PATH=/home/opam/.cargo/bin:/usr/lib/go/bin:\\\$PATH", "DUNE_PROFILE=devnet"]) "eval \\\$(opam config env) && make ${tool}",
      label = "Build ${tool} tool",
      key = "archive-redundancy-${tool}",
      target = cmd_target,
      docker = None Docker.Type,
      artifact_paths = [ S.contains "_build/default/src/app/**/*.exe" ]
    }

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = [
            S.strictlyStart (S.contains "src"),
            S.strictlyStart (S.contains "scripts/archive"),
            S.strictlyStart (S.contains "automation"),
            S.strictlyStart (S.contains "buildkite/src/Jobs/Release/ArchiveNodeArtifact"),
            S.strictlyStart (S.contains "buildkite/src/Jobs/Release/ArchiveRedundancyTools")
        ],
        path = "Release",
        name = "ArchiveRedundancyTools"
      },
    steps = [
      buildToolCmd "build_archive_all_sigs" Size.Medium,
      buildToolCmd "extract_blocks" Size.Medium,
      buildToolCmd "missing_blocks_auditor" Size.Medium,
      buildToolCmd "archive_blocks" Size.Medium,
      buildToolCmd "replayer" Size.Medium,
      buildToolCmd "swap_bad_balances" Size.Medium
    ]
  }
