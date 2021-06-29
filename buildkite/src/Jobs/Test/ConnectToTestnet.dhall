let S = ../../Lib/SelectFiles.dhall

let JobSpec = ../../Pipeline/JobSpec.dhall
let Pipeline = ../../Pipeline/Dsl.dhall

let ConnectToTestnet = ../../Command/ConnectToTestnet.dhall

let dependsOn = [
    { name = "MinaArtifact", key = "mainnet-docker-image" }
]

in Pipeline.build Pipeline.Config::{
  spec =
    JobSpec::{
    dirtyWhen = [
      S.strictlyStart (S.contains "src"),
      S.exactly "buildkite/scripts/connect-to-testnet-on-develop" "sh"
    ],
    path = "Test",
    name = "ConnectToTestnet"
  },
  steps = [
    ConnectToTestnet.step dependsOn
  ]
}
