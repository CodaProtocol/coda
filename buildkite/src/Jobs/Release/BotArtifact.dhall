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
    service="bot",
    step_key="bot-docker-image"
}

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = [
          S.strictlyStart (S.contains "buildkite/src/Jobs/Release/BotArtifact"),
          S.strictlyStart (S.contains "frontend/bot")
        ],
        path = "Release",
        name = "BotArtifact"
      },
    steps = [
      Command.build
        Command.Config::{
          commands = [
              Cmd.run "echo export MINA_VERSION=$(cat frontend/bot/package.json | jq '.version') > BOT_DEPLOY_ENV"
          ],
          label = "Setup o1bot docker image deploy environment",
          key = "setup-deploy-env",
          target = Size.Small,
          artifact_paths = [ S.contains "frontend/bot/*.json" ]
        },
      DockerImage.generateStep spec
    ]
  }
