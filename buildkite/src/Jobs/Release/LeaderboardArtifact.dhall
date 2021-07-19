let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall
let D = S.PathPattern

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let Size = ../../Command/Size.dhall
let DockerImage = ../../Command/DockerImage.dhall

let dependsOn = [ { name = "GitEnvUpload", key = "upload-git-env" } ]

let spec = DockerImage.ReleaseSpec::{
    deps=dependsOn,
    service="leaderboard",
    step_key="leaderboard-docker-image"
}

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = [
          S.strictlyStart (S.contains "buildkite/src/Jobs/Release/LeaderboardArtifact"),
          S.strictlyStart (S.contains "frontend/leaderboard")
        ],
        path = "Release",
        name = "LeaderboardArtifact"
      },
    steps = [
      Command.build
        Command.Config::{
          commands = [
              Cmd.run "echo export MINA_VERSION=$(cat frontend/leaderboard/package.json | jq '.version') > LEADERBOARD_DEPLOY_ENV"
          ],
          label = "Setup Leaderboard docker image deploy environment",
          key = "setup-deploy-env",
          target = Size.Small,
          artifact_paths = [ S.contains "frontend/leaderboard/package.json" ]
        },
      DockerImage.generateStep spec
    ]
  }
