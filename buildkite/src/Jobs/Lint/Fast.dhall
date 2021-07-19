let Prelude = ../../External/Prelude.dhall

let B = ../../External/Buildkite.dhall

let S = ../../Lib/SelectFiles.dhall

let Pipeline = ../../Pipeline/Dsl.dhall

let JobSpec = ../../Pipeline/JobSpec.dhall

let Cmd = ../../Lib/Cmds.dhall

let Command = ../../Command/Base.dhall

let Docker = ../../Command/Docker/Type.dhall

let RunInToolchain = ../../Command/RunInToolchain.dhall

let Size = ../../Command/Size.dhall

let commands =
      [ Cmd.run "./scripts/lint_codeowners.sh"
      , Cmd.run "./scripts/lint_rfcs.sh"
      , Cmd.run "make check-snarky-submodule"
      , Cmd.run "./scripts/lint_preprocessor_deps.sh"
      ]

in  Pipeline.build
      Pipeline.Config::{
        spec = JobSpec::{
        , dirtyWhen = [ S.strictlyStart (S.contains "src/") ]
        , path = "Lint"
        , name = "Fast"
        }
      , steps =
        [ Command.build
            Command.Config::{
            , commands = commands
            , label =
                "Fast lint steps; CODEOWNERs, RFCs, Check Snarky Submodule, Preprocessor Deps"
            , key = "lint"
            , target = Size.Small
            , docker = Some Docker::{
              , image = (../../Constants/ContainerImages.dhall).toolchainBase
              }
            }
        , Command.build
            Command.Config::{
            , commands =
                RunInToolchain.runInToolchainBuster
                  [ "CI=true"
                  , "BASE_BRANCH_NAME=\$BUILDKITE_PULL_REQUEST_BASE_BRANCH"
                  ]
                  "./scripts/compare_ci_diff_types.sh"
            , label = "Optional fast lint steps; versions compatibility changes"
            , key = "lint-optional-types"
            , target = Size.Medium
            , docker = None Docker.Type
            }
        , Command.build
            Command.Config::{
            , commands =
                RunInToolchain.runInToolchainBuster
                  [ "CI=true"
                  , "BASE_BRANCH_NAME=\$BUILDKITE_PULL_REQUEST_BASE_BRANCH"
                  ]
                  "./scripts/compare_ci_diff_binables.sh"
            , label = "Optional fast lint steps; binable compatibility changes"
            , key = "lint-optional-binable"
            , target = Size.Medium
            , docker = None Docker.Type
            }
        ]
      }
