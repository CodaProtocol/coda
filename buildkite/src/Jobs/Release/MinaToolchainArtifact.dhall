let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let Size = ../../Command/Size.dhall
let DockerImage = ../../Command/DockerImage.dhall
let DockerLogin = ../../Command/DockerLogin/Type.dhall


let dependsOn = [ { name = "GitEnvUpload", key = "upload-git-env" } ]
let deployEnv = "export-git-env-vars.sh"

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

      -- mina-toolchain Debian Buster image
      let toolchainBusterSpec = DockerImage.ReleaseSpec::{
        deps=dependsOn,
        service="mina-toolchain",
        deb_codename="buster",
        step_key="mina-toolchain-buster-docker-image"
      }

      in

      DockerImage.generateStep toolchainBusterSpec,

      -- mina-toolchain Debian Stretch image
      let toolchainStretchSpec = DockerImage.ReleaseSpec::{
        deps=dependsOn,
        service="mina-toolchain",
        deb_codename="stretch",
        step_key="mina-toolchain-stretch-docker-image"
      }

      in

      DockerImage.generateStep toolchainStretchSpec

    ]
  }
