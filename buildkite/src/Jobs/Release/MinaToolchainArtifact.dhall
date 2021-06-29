let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let Size = ../../Command/Size.dhall
let UploadGitEnv = ../../Command/UploadGitEnv.dhall
let DockerImage = ../../Command/DockerImage.dhall
let DockerLogin = ../../Command/DockerLogin/Type.dhall


let dependsOn = { name = "MinaToolchainArtifact", key = "upload-git-env" }
let deployEnv = "export-git-env-vars.sh"

let commands : List Cmd.Type =
  [
      -- Setup Git deploy environment
      Cmd.run (
        "if [ ! -f ${deployEnv} ]; then " ++
            "buildkite-agent artifact download --build \\\$BUILDKITE_BUILD_ID --include-retried-jobs --step _${dependsOn.name}-${dependsOn.key} ${deployEnv} .; " ++
        "fi"
      ),
      -- Dockerhub: Build and release toolchain image
      Cmd.run (
        "source ${deployEnv} && cat dockerfiles/Dockerfile-toolchain | docker build --rm --tag codaprotocol/mina-toolchain:\\\$DOCKER_TAG-\\\$GITHASH - && " ++
          "docker push codaprotocol/mina-toolchain:\\\$DOCKER_TAG-\\\$GITHASH"
      ),
      -- GCR: Build and release toolchain image
      Cmd.run (
        "source ${deployEnv} && docker tag codaprotocol/mina-toolchain:\\\$DOCKER_TAG-\\\$GITHASH gcr.io/o1labs-192920/mina-toolchain:\\\$DOCKER_TAG-\\\$GITHASH && " ++
          "docker push gcr.io/o1labs-192920/mina-toolchain:\\\$DOCKER_TAG-\\\$GITHASH"
      )
  ]

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = [
          S.strictlyStart (S.contains "dockerfiles/Dockerfile-toolchain"),
          S.strictlyStart (S.contains "buildkite/src/Jobs/Release/MinaToolchainArtifact")
        ],
        path = "Release",
        name = "MinaToolchainArtifact"
      },
    steps = [
      UploadGitEnv.step,
      Command.build
        Command.Config::{
            commands  = commands,
            label = "Build and release Mina toolchain Docker image",
            key = "mina-toolchain-image",
            target = Size.XLarge,
            docker_login = Some DockerLogin::{=},
            depends_on = [ dependsOn ]
        }
    ]
  }
