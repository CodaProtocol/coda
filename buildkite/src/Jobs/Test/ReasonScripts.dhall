let Prelude =  ../../External/Prelude.dhall
let S = ../../Lib/SelectFiles.dhall
let Cmd =  ../../Lib/Cmds.dhall
let Pipeline = ../../Pipeline/Dsl.dhall
let Command = ../../Command/Base.dhall
let WithCargo = ../../Command/WithCargo.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall
let ContainerImages = ../../Constants/ContainerImages.dhall
in

Pipeline.build
  Pipeline.Config::
    { spec =
      JobSpec::
        { dirtyWhen =
          [ S.strictlyStart (S.contains "buildkite/src/Jobs/Test/ReasonScripts")
          , S.strictlyStart (S.contains "frontend/block-timings")
          ]
        , path = "Test"
        , name = "ReasonScripts"
        }
    , steps =
      [ Command.build
          Command.Config::
            { commands = [ Cmd.run "cd frontend/block-timings && yarn install && yarn build" ]
            , label = "Build block-timings"
            , key = "build-block-timings"
            , target = Size.Small
            , docker = Some (Docker::{ image=ContainerImages.nodeToolchain })
            }
      ]
    }

