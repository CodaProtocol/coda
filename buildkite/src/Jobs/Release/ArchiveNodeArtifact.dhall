let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall
let D = S.PathPattern

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let OpamInit = ../../Command/OpamInit.dhall
let Size = ../../Command/Size.dhall
let DockerImage = ../../Command/DockerImage.dhall

let dependsOn = [ { name = "ArchiveNodeArtifact", key = "build-archive-deb-pkg" } ]

let spec = DockerImage.ReleaseSpec::{
    deps=dependsOn,
    deploy_env_file="ARCHIVE_DOCKER_DEPLOY",
    step_key="archive-docker-image"
}

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
        dirtyWhen = [
          S.strictly (S.contains "Makefile"),
          S.strictlyStart (S.contains "src"),
          S.strictlyStart (S.contains "scripts/archive"),
          S.strictlyStart (S.contains "automation"),
          S.strictlyStart (S.contains "buildkite/src/Jobs/Release/ArchiveNodeArtifact")
        ],
        path = "Release",
        name = "ArchiveNodeArtifact"
      },
    steps = [
      Command.build
        Command.Config::{
          commands = [
              Cmd.run "buildkite/scripts/ci-archive-release.sh"
            ]

            #

            OpamInit.andThenRunInDocker [
              "DUNE_PROFILE=devnet",
              "AWS_ACCESS_KEY_ID",
              "AWS_SECRET_ACCESS_KEY",
              "BUILDKITE"
            ] "./scripts/archive/build-release-archives.sh"

            #

            [
              Cmd.run "artifact-cache-helper.sh ./${spec.deploy_env_file} --upload"
            ],
          label = "Build Archive node debian package",
          key = "build-archive-deb-pkg",
          target = Size.XLarge,
          artifact_paths = [ S.contains "./*.deb" ],
          depends_on = [
            { name = "ArchiveRedundancyTools", key = "archive-redundancy-extract_blocks" },
            { name = "ArchiveRedundancyTools", key = "archive-redundancy-build_archive_all_sigs" },
            { name = "ArchiveRedundancyTools", key = "archive-redundancy-archive_blocks" },
            { name = "ArchiveRedundancyTools", key = "archive-redundancy-missing_blocks_auditor" },
            { name = "ArchiveRedundancyTools", key = "archive-redundancy-replayer" },
            { name = "ArchiveRedundancyTools", key = "archive-redundancy-swap_bad_balances" }
          ]
        },
      DockerImage.generateStep spec
    ]
  }
