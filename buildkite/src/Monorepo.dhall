let Prelude = ./External/Prelude.dhall
let List/map = Prelude.List.map

let SelectFiles = ./Lib/SelectFiles.dhall
let Cmd = ./Lib/Cmds.dhall

let Command = ./Command/Base.dhall
let Docker = ./Command/Docker/Type.dhall
let JobSpec = ./Pipeline/JobSpec.dhall
let Pipeline = ./Pipeline/Dsl.dhall
let PipelineMode = ./Pipeline/Mode.dhall
let Size = ./Command/Size.dhall
let triggerCommand = ./Pipeline/TriggerCommand.dhall

let jobs : List JobSpec.Type =
  List/map
    Pipeline.CompoundType
    JobSpec.Type
    (\(composite: Pipeline.CompoundType) -> composite.spec)
    ./gen/Jobs.dhall

-- Run a job if we touched a dirty path
let makeCommand : JobSpec.Type -> Cmd.Type = \(job : JobSpec.Type) ->
  let dirtyWhen = SelectFiles.compile job.dirtyWhen
  let trigger = triggerCommand "src/Jobs/${job.path}/${job.name}.dhall"
  let pipelineType : PipelineMode = env:BUILDKITE_PIPELINE_MODE ? PipelineMode.PullRequest
  let pipelineHandlers = {
    PullRequest = ''
      if cat _computed_diff.txt | egrep -q '${dirtyWhen}'; then
        echo "Triggering ${job.name} for reason:"
        cat _computed_diff.txt | egrep '${dirtyWhen}'
        ${Cmd.format trigger}
      fi
    '',
    Stable = ''
      echo "Triggering ${job.name} because this is a stable buildkite run"
      ${Cmd.format trigger}
    ''
  }
  in Cmd.quietly (merge pipelineHandlers pipelineType)

let prefixCommands = [
  Cmd.run "git config --global http.sslCAInfo /etc/ssl/certs/ca-bundle.crt", -- Tell git where to find certs for https connections
  Cmd.run "git fetch origin", -- Freshen the cache
  Cmd.run "./buildkite/scripts/generate-diff.sh > _computed_diff.txt"
]

let commands = Prelude.List.map JobSpec.Type Cmd.Type makeCommand jobs

in Pipeline.build Pipeline.Config::{
  spec = JobSpec::{
    name = "monorepo-triage",
    -- TODO: Clean up this code so we don't need an unused dirtyWhen here
    dirtyWhen = [ SelectFiles.everything ]
  },
  steps = [
  Command.build
    Command.Config::{
      commands = prefixCommands # commands,
      label = "Monorepo triage",
      key = "cmds",
      target = Size.Small,
      docker = Some Docker::{
        image = (./Constants/ContainerImages.dhall).toolchainBase,
        environment = ["BUILDKITE_AGENT_ACCESS_TOKEN", "BUILDKITE_INCREMENTAL"]
      }
    }
  ]
}

