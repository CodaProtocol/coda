-- Autogenerates any pre-reqs for monorepo triage execution
-- Keep these rules lean! They have to run unconditionally.

let SelectFiles = ./Lib/SelectFiles.dhall
let Cmd = ./Lib/Cmds.dhall

let Command = ./Command/Base.dhall
let Docker = ./Command/Docker/Type.dhall
let JobSpec = ./Pipeline/JobSpec.dhall
let Pipeline = ./Pipeline/Dsl.dhall
let Size = ./Command/Size.dhall
let triggerCommand = ./Pipeline/TriggerCommand.dhall

let config : Pipeline.Config.Type = Pipeline.Config::{
  spec = JobSpec::{
    name = "prepare",
    -- TODO: Clean up this code so we don't need an unused dirtyWhen here
    dirtyWhen = [ SelectFiles.everything ]
  },
  steps = [
  Command.build
    Command.Config::{
      commands = [
        Cmd.run "bash -c './buildkite/scripts/generate-jobs.sh > buildkite/src/gen/Jobs.dhall'",
        triggerCommand "src/Monorepo.dhall"
      ],
      label = "Prepare monorepo triage",
      key = "monorepo",
      target = Size.Small,
      docker = Some Docker::{
        image = (./Constants/ContainerImages.dhall).toolchainBase,
        environment = ["BUILDKITE_AGENT_ACCESS_TOKEN", "BUILDKITE_PIPELINE_TYPE"]
      }
    }
  ]
}
in (Pipeline.build config).pipeline
