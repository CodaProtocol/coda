let S = ../../Lib/SelectFiles.dhall
let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall
let Command = ../../Command/Base.dhall
let RunInToolchain = ../../Command/RunInToolchain.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall
in

let user = "admin"
let password = "codarules"
let db = "archiver"
in

let one = \(label : Text) -> \(key : Text) -> \(script : Text) ->
  Command.build
    Command.Config::
      { commands = RunInToolchain.runInToolchainBuster ([] : List Text) script
      , label = label
      , key = key
      , target = Size.Large
      , docker = None Docker.Type
      , artifact_paths = [ S.contains "test_output/artifacts/*" ]
      }
in

Pipeline.build
  Pipeline.Config::
    { spec =
      JobSpec::
        { dirtyWhen =
          [ S.strictlyStart (S.contains "src/lib")
          , S.strictly (S.contains "Makefile")
          , S.strictlyStart (S.contains "src/app/cli")
          ]
        , path = "Test"
        , name = "MigratedIntegrationTests"
        }
    , steps =
      [ one "Snarkless integration test (migrated)" "test_postake_snarkless" "./buildkite/scripts/old_integration_tests/test_postake_snarkless.sh"
      , one "Split integration test (migrated)" "test_postake_split" "./buildkite/scripts/old_integration_tests/test_postake_split.sh"
      , one "Bootstrap integration test (migrated)" "bootstrap_test" "./buildkite/scripts/old_integration_tests/test_bootstrap.sh"
      , one "Delegation integration test (migrated)" "delegation_test" "./buildkite/scripts/old_integration_tests/test_delegation.sh"
      , one "Shared state integration test (migrated)" "shared_state_test" "./buildkite/scripts/old_integration_tests/test_shared_state.sh"
      ]
    }
