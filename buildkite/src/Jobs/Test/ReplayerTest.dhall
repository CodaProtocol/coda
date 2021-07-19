let S = ../../Lib/SelectFiles.dhall

let Pipeline = ../../Pipeline/Dsl.dhall

let JobSpec = ../../Pipeline/JobSpec.dhall

let ReplayerTest = ../../Command/ReplayerTest.dhall

let dependsOn =
      [ { name = "MinaArtifactStretch", key = "archive-stretch-docker-image" } ]

in  Pipeline.build
      Pipeline.Config::{
      , spec = JobSpec::{
        , dirtyWhen =
          [ S.strictlyStart (S.contains "src")
          , S.exactly "buildkite/scripts/replayer-test" "sh"
          ]
        , path = "Test"
        , name = "ReplayerTest"
        }
      , steps = [ ReplayerTest.step dependsOn ]
      }
