let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall
let D = S.PathPattern

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let RunInToolchain = ../../Command/RunInToolchain.dhall
let Summon = ../../Command/Summon/Type.dhall
let Size = ../../Command/Size.dhall
let Libp2p = ../../Command/Libp2pHelperBuild.dhall
let ConnectToTestnet = ../../Command/ConnectToTestnet.dhall
let DockerImage = ../../Command/DockerImage.dhall

let dependsOn = [ 
  { name = "GitEnvUpload", key = "upload-git-env" },
  { name = "MinaArtifactStretch", key = "build-deb-pkg" }
]

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = [
          S.strictlyStart (S.contains "src"),
          S.strictlyStart (S.contains "automation"),
          S.strictly (S.contains "Makefile"),
          S.strictlyStart (S.contains "buildkite/src/Jobs/Release/MinaArtifactStretch"),
          S.exactly "buildkite/scripts/build-artifact" "sh",
          S.exactly "buildkite/scripts/connect-to-mainnet-on-compatible" "sh",
          S.strictlyStart (S.contains "dockerfiles"),
          S.strictlyStart (S.contains "scripts")
        ],
        path = "Release",
        name = "MinaArtifactStretch"
      },
    steps = [
      Libp2p.step,
      Command.build
        Command.Config::{
          commands = RunInToolchain.runInToolchainStretch [
            "DUNE_PROFILE=devnet",
            "AWS_ACCESS_KEY_ID",
            "AWS_SECRET_ACCESS_KEY",
            "MINA_BRANCH=$BUILDKITE_BRANCH",
            "MINA_COMMIT_SHA1=$BUILDKITE_COMMIT",
            "MINA_DEB_CODENAME=stretch",
            -- add zexe standardization preprocessing step (see: https://github.com/MinaProtocol/mina/pull/5777)
            "PREPROCESSOR=./scripts/zexe-standardize.sh"
          ] "./buildkite/scripts/build-artifact.sh",
          label = "Build Mina daemon package for Debian Stretch",
          key = "build-deb-pkg",
          target = Size.XLarge,
          retries = [ Command.Retry::{ exit_status = +2, limit = Some 2 } ] -- libp2p error
        },

      -- daemon devnet image
      let daemonDevnetSpec = DockerImage.ReleaseSpec::{
        deps=dependsOn,
        service="mina-daemon",
        network="devnet",
        step_key="daemon-devnet-stretch-docker-image"
      }

      in

      DockerImage.generateStep daemonDevnetSpec,

      -- daemon mainnet image
      let daemonMainnetSpec = DockerImage.ReleaseSpec::{
        deps=dependsOn,
        service="mina-daemon",
        network="mainnet",
        step_key="daemon-mainnet-stretch-docker-image"
      }

      in

      DockerImage.generateStep daemonMainnetSpec,

      -- archive image
      let archiveSpec = DockerImage.ReleaseSpec::{
        deps=dependsOn,
        service="mina-archive",
        step_key="archive-stretch-docker-image"
      }

      in

      DockerImage.generateStep archiveSpec

    ]
  }
