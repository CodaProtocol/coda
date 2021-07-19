let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let Size = ../../Command/Size.dhall

let UploadGitEnv = ../../Command/UploadGitEnv.dhall

let deployEnv = "export-git-env-vars.sh"

in Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = [
          S.strictlyStart (S.contains "buildkite/scripts/export-git-env-vars"),
          S.strictlyStart (S.contains "buildkite/src/Jobs/Release/GitEnvUpload")
        ],
        path = "Release",
        name = "GitEnvUpload"
      },
    steps = [
      UploadGitEnv.step
    ]
  }
