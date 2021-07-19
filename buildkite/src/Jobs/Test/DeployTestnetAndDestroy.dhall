let S = ../../Lib/SelectFiles.dhall

let JobSpec = ../../Pipeline/JobSpec.dhall
let Pipeline = ../../Pipeline/Dsl.dhall

let DeployTestnet = ../../Command/DeployTestnet.dhall


let spec = DeployTestnet.DeploySpec::{
  deps = [ { name = "MinaArtifactBuster", key = "daemon-devnet-buster-docker-image" } ],
  postDeploy = "sleep 10 && terraform destroy -auto-approve"
}

in

Pipeline.build Pipeline.Config::{
  spec =
    JobSpec::{
    dirtyWhen = [
        S.strictlyStart (S.contains "automation/terraform/modules/kubernetes/testnet"),
        S.strictlyStart (S.contains "buildkite/src/Command/DeployTestnet"),
        S.strictlyStart (S.contains "buildkite/src/Jobs/Test/DeployTestnetAndDestroy")
    ],
    path = "Test",
    name = "DeployTestnetAndDestroy"
  },
  steps = [
    DeployTestnet.step spec
  ]
}
