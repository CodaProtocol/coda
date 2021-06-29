let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall
let D = S.PathPattern

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let OpamInit = ../../Command/OpamInit.dhall
let Summon = ../../Command/Summon/Type.dhall
let Size = ../../Command/Size.dhall
let Libp2p = ../../Command/Libp2pHelperBuild.dhall
let ConnectToTestnet = ../../Command/ConnectToTestnet.dhall
let UploadGitEnv = ../../Command/UploadGitEnv.dhall
let DockerImage = ../../Command/DockerImage.dhall

let dependsOn = [ { name = "MinaArtifact", key = "build-deb-pkg" } ]
let rosettaDependsOn = [ { name = "MinaArtifact", key = "upload-git-env" } ]

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = OpamInit.dirtyWhen # [
          S.strictlyStart (S.contains "src"),
          S.strictlyStart (S.contains "automation"),
          S.strictly (S.contains "Makefile"),
          S.strictlyStart (S.contains "buildkite/src/Jobs/Release/MinaArtifact"),
          S.exactly "buildkite/scripts/build-artifact" "sh",
          S.exactly "buildkite/scripts/connect-to-testnet-on-develop" "sh",
          S.exactly "dockerfiles/Dockerfile-mina-daemon" "",
          S.exactly "dockerfiles/Dockerfile-mina-daemon-puppeteered" "",
          S.exactly "dockerfiles/puppeteer-context/mina_daemon_puppeteer" "py",
          S.exactly "dockerfiles/scripts/healthcheck-utilities" "sh",
          S.strictlyStart (S.contains "scripts")
        ],
        path = "Release",
        name = "MinaArtifact"
      },
    steps = [
      Libp2p.step,
      UploadGitEnv.step,
      Command.build
        Command.Config::{
          commands = OpamInit.andThenRunInDocker [
            "DUNE_PROFILE=devnet",
            "AWS_ACCESS_KEY_ID",
            "AWS_SECRET_ACCESS_KEY",
            "MINA_BRANCH=$BUILDKITE_BRANCH",
            "MINA_COMMIT_SHA1=$BUILDKITE_COMMIT",
            -- add zexe standardization preprocessing step (see: https://github.com/MinaProtocol/mina/pull/5777)
            "PREPROCESSOR=./scripts/zexe-standardize.sh"
          ] "./buildkite/scripts/build-artifact.sh" # [ Cmd.run "buildkite/scripts/buildkite-artifact-helper.sh ./DOCKER_DEPLOY_ENV" ],
          label = "Build Mina daemon debian package",
          key = "build-deb-pkg",
          target = Size.XLarge,
          artifact_paths = [ S.contains "_build/*.deb" ],
          retries = [ Command.Retry::{ exit_status = +2, limit = Some 2 } ] -- libp2p error
        },

      -- daemon image
      let daemonSpec = DockerImage.ReleaseSpec::{
        deps=dependsOn,
        service="mina-daemon"
      }

      in

      DockerImage.generateStep daemonSpec,

      -- mainnet image
      let mainnetSpec = DockerImage.ReleaseSpec::{
        deps=dependsOn,
        service="mina-daemon",
        version = "\\\${MINA_VERSION}-mainnet",
        step_key="mainnet-docker-image",
        extra_args = "--build-arg deb_version=\\\${MINA_DEB_VERSION} --build-arg deb_release=\\\${MINA_DEB_RELEASE} --build-arg deb_codename=\\\${MINA_DEB_CODENAME} --build-arg network=mainnet --build-arg MINA_VERSION=\\\${MINA_VERSION} --build-arg MINA_BRANCH=\\\${MINA_GIT_BRANCH}"
      }

      in

      DockerImage.generateStep mainnetSpec,

      -- puppeteered image
      let puppeteeredSpec = DockerImage.ReleaseSpec::{
        deps=dependsOn # [{ name = "MinaArtifact", key = "devnet-docker-image" }],
        service="\\\${MINA_SERVICE}-puppeteered",
        extra_args = "--build-arg network=devnet --build-arg MINA_VERSION=\\\${MINA_VERSION}-devnet",
        step_key="puppeteered-docker-image"
      }

      in

      DockerImage.generateStep puppeteeredSpec,

      -- rosetta image
      let rosettaSpec = DockerImage.ReleaseSpec::{
        deps=rosettaDependsOn,
        deploy_env_file="export-git-env-vars.sh",
        service="mina-rosetta",
        version="\\\${DOCKER_TAG}",
        commit = "\\\${GITHASH}",
        extra_args="--build-arg MINA_BRANCH=\\\${BUILDKITE_BRANCH} --build-arg MINA_REPO=\\\${BUILDKITE_PULL_REQUEST_REPO} --cache-from gcr.io/o1labs-192920/mina-rosetta-opam-deps:develop",
        step_key="rosetta-docker-image"
      }

      in

      DockerImage.generateStep rosettaSpec,

      -- rosetta image w/ DUNE_PROFILE=dev
      let rosettaDuneSpec = DockerImage.ReleaseSpec::{
        deps=rosettaDependsOn,
        deploy_env_file="export-git-env-vars.sh",
        service="mina-rosetta",
        version="dev-\\\${DOCKER_TAG}",
        commit = "\\\${GITHASH}",
        extra_args="--build-arg DUNE_PROFILE=dev --build-arg MINA_BRANCH=\\\${BUILDKITE_BRANCH} --build-arg MINA_REPO=\\\${BUILDKITE_PULL_REQUEST_REPO} --cache-from gcr.io/o1labs-192920/mina-rosetta-opam-deps:develop",
        step_key="rosetta-dune-docker-image"
      }

      in

      DockerImage.generateStep rosettaDuneSpec
    ]
  }
