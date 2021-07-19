let Prelude =  ../../External/Prelude.dhall
let S = ../../Lib/SelectFiles.dhall
let Cmd =  ../../Lib/Cmds.dhall
let Pipeline = ../../Pipeline/Dsl.dhall
let Command = ../../Command/Base.dhall
let RunInToolchain = ../../Command/RunInToolchain.dhall
let WithCargo = ../../Command/WithCargo.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall
in

Pipeline.build
  Pipeline.Config::
    { spec =
      JobSpec::
        { dirtyWhen =
          [ S.strictlyStart (S.contains "buildkite/src/Jobs/Test/CompareSignatures")
          , S.strictlyStart (S.contains "src")
          ]
        , path = "Test"
        , name = "CompareSignatures"
        }
    , steps =
      [ Command.build
          Command.Config::
            { commands = RunInToolchain.runInToolchainBuster ([] : List Text) "./buildkite/scripts/compare_test_signatures.sh"
            , label = "Compare test signatures"
            , key = "compare-test-signatures"
            , target = Size.Medium
            , docker = None Docker.Type
            }
      ]
    }

