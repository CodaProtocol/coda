let Prelude = ../External/Prelude.dhall

let Command = ./Base.dhall
let Size = ./Size.dhall

let Cmd = ../Lib/Cmds.dhall


let DeploySpec = {
  Type = {
    testnetLabel: Text,
    deployEnvFile : Text,
    workspace: Text,
    artifactPath: Text,
    postDeploy: Text,
    testnetDir: Text,
    deps : List Command.TaggedKey.Type
  },
  default = {
    testnetLabel = "ci-net",
    deployEnvFile = "export-git-env-vars.sh",
    workspace = "\\\${BUILDKITE_BRANCH//[_\\/]/-}",
    artifactPath = "/tmp/artifacts",
    postDeploy = "echo 'Deployment successfull!'",
    testnetDir = "automation/terraform/testnets/ci-net",
    deps = [] : List Command.TaggedKey.Type
  }
}

in

{ 
  step = \(spec : DeploySpec.Type) ->
    Command.build
      Command.Config::{
        commands = [
          Cmd.run "cd ${spec.testnetDir}",
          Cmd.run "terraform init",
          -- create separate workspace based on build branch to isolate infrastructure states
          Cmd.run "terraform workspace select ${spec.workspace} || terraform workspace new ${spec.workspace}",
          -- download deployment dependencies and ensure artifact DIR exists
          Cmd.run "mkdir -p ${spec.artifactPath}",
          Cmd.run "artifact-cache-helper.sh ${spec.deployEnvFile}",
          -- launch testnet based on deploy ENV and ensure auto-cleanup on `apply` failures
          Cmd.run "source ${spec.deployEnvFile}",
          Cmd.run (
            "terraform apply -auto-approve" ++
              " -var mina_image=gcr.io/o1labs-192920/mina-daemon:\\\$MINA_DOCKER_TAG" ++
              " -var ci_artifact_path=${spec.artifactPath}" ++
              " || (terraform destroy -auto-approve && exit 1)"
          ),
          -- upload/cache testnet genesis_ledger
          Cmd.run "artifact-cache-helper.sh ${spec.artifactPath}/genesis_ledger.json --upload",
          -- execute post-deploy operation
          Cmd.run "${spec.postDeploy}"
        ],
        label = "Deploy ${spec.testnetLabel}",
        key = "deploy-${spec.testnetLabel}",
        target = Size.Large,
        depends_on = spec.deps
      },
  DeploySpec = DeploySpec
}
