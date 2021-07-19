let Prelude = ../../External/Prelude.dhall

let S = ../../Lib/SelectFiles.dhall

let JobSpec = ../../Pipeline/JobSpec.dhall

let Cmd = ../../Lib/Cmds.dhall

let Pipeline = ../../Pipeline/Dsl.dhall

let RunInToolchain = ../../Command/RunInToolchain.dhall

let Command = ../../Command/Base.dhall

let Docker = ../../Command/Docker/Type.dhall

let Size = ../../Command/Size.dhall

in  Pipeline.build
      Pipeline.Config::{
      , spec =
          let dirtyDhallDir =
                S.strictlyStart (S.contains "buildkite/src/Jobs/Lint/OCaml")

          let dirtyDhallDirCompiles =
                  assert
                : S.compile [ dirtyDhallDir ] ≡ "^buildkite/src/Jobs/Lint/OCaml"

          in  JobSpec::{
              , dirtyWhen =
                [ dirtyDhallDir, S.strictlyStart (S.contains "src/") ]
              , path = "Lint"
              , name = "OCaml"
              }
      , steps =
        [ Command.build
            Command.Config::{
            , commands =
                RunInToolchain.runInToolchainBuster
                  ([] : List Text)
                  (     "./buildkite/scripts/lint-check-format.sh && "
                    ++  "./scripts/require-ppxs.py"
                  )
            , label = "OCaml Lints; Check-format, Require-ppx-version"
            , key = "check"
            , target = Size.Large
            , docker = None Docker.Type
            }
        ]
      }
