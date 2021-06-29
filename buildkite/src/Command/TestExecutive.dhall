let Prelude = ../External/Prelude.dhall

let Command = ./Base.dhall
let Docker = ./Docker/Type.dhall
let Size = ./Size.dhall

let OpamInit = ../Command/OpamInit.dhall

let Cmd = ../Lib/Cmds.dhall
let SelectFiles = ../Lib/SelectFiles.dhall

let deployEnv = "DOCKER_DEPLOY_ENV"

in

{
  build = \(duneProfile : Text) ->
    Command.build
      Command.Config::{
        commands =
            -- Build test executive binary
            OpamInit.andThenRunInDocker [
              "DUNE_PROFILE=${duneProfile}",
              -- add zexe standardization preprocessing step (see: https://github.com/MinaProtocol/mina/pull/5777)
              "PREPROCESSOR=./scripts/zexe-standardize.sh"
            ] "./buildkite/scripts/build-test-executive.sh"
            
            #
            
            [
              -- Cache test-executive binary
              Cmd.run "artifact-cache-helper.sh test_executive.exe --upload"
            ],
        label = "Build test-executive",
        key = "build-test-executive",
        target = Size.XLarge,
        if = Some "build.branch != 'develop' && build.branch != 'compatible' && build.branch != 'develop-next'"
      },

  execute = \(testName : Text) -> \(dependsOn : List Command.TaggedKey.Type) ->
    Command.build
      Command.Config::{
        commands =
            [
              -- Download test dependencies
              Cmd.run "artifact-cache-helper.sh test_executive.exe && chmod +x test_executive.exe",
              Cmd.run "artifact-cache-helper.sh ${deployEnv}",
              Cmd.run "cat ${deployEnv}",

              -- Execute test based on BUILD image
              Cmd.run "source ${deployEnv} && ./buildkite/scripts/run-test-executive.sh ${testName}"
            ],
        artifact_paths = [SelectFiles.exactly "." "${testName}.test.log"],
        label = "${testName} integration test",
        key = "integration-test-${testName}",
        target = Size.Medium,
        depends_on = dependsOn,
        if = Some "build.branch != 'develop' && build.branch != 'compatible' && build.branch != 'develop-next'"
      }
}
