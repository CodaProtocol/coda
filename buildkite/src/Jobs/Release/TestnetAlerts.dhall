let Prelude = ../../External/Prelude.dhall
let B = ../../External/Buildkite.dhall

let B/SoftFail = B.definitions/commandStep/properties/soft_fail/Type

let S = ../../Lib/SelectFiles.dhall
let Cmd = ../../Lib/Cmds.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall

in

Pipeline.build
  Pipeline.Config::{
    spec = JobSpec::{
      dirtyWhen = [
        S.strictlyStart (S.contains "automation/terraform/modules/testnet-alerts"),
        S.exactly "automation/terraform/monitoring/o1-testnet-alerts" "tf",
        S.strictlyStart (S.contains "buildkite/src/Jobs/Release/TestnetAlerts")
      ],
      path = "Release",
      name = "TestnetAlerts"
    },
    steps = [
      Command.build
        Command.Config::{
          commands = [
            --- destroy state prior to start to ensure reset
            Cmd.run "cd automation/terraform/monitoring && terraform init && terraform destroy -auto-approve",
            Cmd.run "terraform apply -auto-approve -target module.o1testnet_alerts.docker_container.sync_alert_rules"
          ]
          , label = "Deploy Testnet alert rules"
          , key = "deploy-testnet-alerts"
          , target = Size.Medium
          , depends_on = [ { name = "TestnetAlerts", key = "lint-testnet-alerts" } ]
          , docker = None Docker.Type
          , if = Some "build.branch == 'compatible' || build.env('DEPLOY_ALERTS') == 'true'"
        }
    ]
  }
