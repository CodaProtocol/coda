let Prelude = ../External/Prelude.dhall

let Command = ./Base.dhall
let Size = ./Size.dhall

let Cmd = ../Lib/Cmds.dhall
-- do we need this?
let minaArtifactDockerEnvStep = { name = "MinaArtifact", key = "build-deb-pkg" }

in

{ step = \(dependsOn : List Command.TaggedKey.Type) ->
    Command.build
      Command.Config::{
        commands = [
          -- download docker env artifact, move this to dependsOn
          Cmd.run (
            "if [ ! -f DOCKER_DEPLOY_ENV ]; then " ++
              "buildkite-agent artifact download --build \\\$BUILDKITE_BUILD_ID --include-retried-jobs --step _${minaArtifactDockerEnvStep.name}-${minaArtifactDockerEnvStep.key} DOCKER_DEPLOY_ENV .; " ++
            "fi"
          ),
          Cmd.run (
            "./buildkite/scripts/deploy-nightly.sh"
          )
        ],
        label = "Deploy nightly",
        key = "deploy-nightly",
        target = Size.Large,
        depends_on = dependsOn
      }
}
