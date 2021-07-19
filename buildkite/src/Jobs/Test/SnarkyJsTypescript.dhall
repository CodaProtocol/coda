let Prelude =  ../../External/Prelude.dhall
let S = ../../Lib/SelectFiles.dhall
let Cmd =  ../../Lib/Cmds.dhall
let Pipeline = ../../Pipeline/Dsl.dhall
let Command = ../../Command/Base.dhall
let OpamInit = ../../Command/OpamInit.dhall
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
          [ S.strictlyStart (S.contains "buildkite/src/Jobs/Test/SnarkyJsTypescript")
          , S.strictlyStart (S.contains "frontend/snapps/snarkyjs")
          ]
        , path = "Test"
        , name = "SnarkyJsTypescript"
        }
    , steps =
      [ Command.build
          Command.Config::
            { commands = [ Cmd.run "cd frontend/snapps/snarkyjs && yarn install && yarn build && yarn test" ]
            , label = "Build and test typescript snarkyjs"
            , key = "snarky-js-typescript"
            , target = Size.Small
            , docker = Some (Docker::{ image=ContainerImages.nodeToolchain })
            }
      ]
    }

