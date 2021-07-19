let S = ../../Lib/SelectFiles.dhall

let JobSpec = ../../Pipeline/JobSpec.dhall
let Pipeline = ../../Pipeline/Dsl.dhall

let TestExecutive = ../../Command/TestExecutive.dhall

let dependsOn = [
    { name = "TestnetIntegrationTests", key = "build-test-executive" },
    { name = "MinaArtifact", key = "devnet-docker-image" },
    { name = "ArchiveNodeArtifact", key = "archive-docker-image" }
]

in Pipeline.build Pipeline.Config::{
  spec =
    JobSpec::{
    dirtyWhen = [
        S.strictlyStart (S.contains "src"),
        S.strictlyStart (S.contains "dockerfiles"),
        S.strictlyStart (S.contains "buildkite/src/Jobs/Test/TestnetIntegrationTest")
    ],
    path = "Test",
    name = "TestnetIntegrationTests"
  },
  steps = [
    TestExecutive.build "integration_tests",
    TestExecutive.execute "reliability" dependsOn,
    TestExecutive.execute "short-boot" dependsOn,
    TestExecutive.execute "payment" dependsOn,
    TestExecutive.execute "gossip-consis" dependsOn,
    TestExecutive.execute "archive-node" dependsOn

  ]
}
