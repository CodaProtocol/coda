let S = ../../Lib/SelectFiles.dhall

let JobSpec = ../../Pipeline/JobSpec.dhall
let Pipeline = ../../Pipeline/Dsl.dhall

let DeployNightly = ../../Command/DeployNightly.dhall

let dependsOn = [
    { name = "MinaArtifact", key = "mina-docker-image" },
    { name = "ArchiveNodeArtifact", key = "archive-docker-image" }
]

in

Pipeline.build
  Pipeline.Config::{
    spec =
      JobSpec::{
      dirtyWhen = [
        S.exactly "buildkite/nightly" "txt"
      ],
      path = "Test",
      name = "Nightly"
    },
    steps = [
      DeployNightly.step dependsOn
    ]
}
