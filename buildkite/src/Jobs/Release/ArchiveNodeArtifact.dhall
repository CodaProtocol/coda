let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall
let D = S.PathPattern

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let OpamInit = ../../Command/OpamInit.dhall
let Size = ../../Command/Size.dhall
let DockerArtifact = ../../Command/DockerArtifact.dhall

let dependsOn = [ { name = "ArchiveNodeArtifact", key = "archive-artifacts-build" } ]

let spec = DockerArtifact.ReleaseSpec::{
    deps=dependsOn,
    deploy_env_file="ARCHIVE_DOCKER_DEPLOY",
    service="coda-archive",
    step_key="archive-docker-artifact"
}

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = [
          S.strictlyStart (S.contains "buildkite/src/Jobs/Release/ArchiveNodeArtifact"),
          S.strictlyStart (S.contains "scripts/archive")
        ],
        path = "Release",
        name = "ArchiveNodeArtifact"
      },
    steps = [
      Command.build
        Command.Config::{
          commands = OpamInit.andThenRunInDocker [
            "DUNE_PROFILE=testnet_postake_medium_curves",
            "AWS_ACCESS_KEY_ID",
            "AWS_SECRET_ACCESS_KEY",
            "BUILDKITE"
          ] "./buildkite/scripts/ci-archive-release.sh" # [ Cmd.run "BUILDKITE_ARTIFACT_UPLOAD_DESTINATION=gs://buildkite_k8s/coda/shared/\\\${BUILDKITE_JOB_ID} buildkite-agent artifact upload ./${spec.deploy_env_file}" ],
          label = "Build Mina archive-node artifacts",
          key = "archive-artifacts-build",
          target = Size.XLarge,
          artifact_paths = [ S.contains "./*.deb" ]
        },
      DockerArtifact.generateStep spec
    ]
  }
