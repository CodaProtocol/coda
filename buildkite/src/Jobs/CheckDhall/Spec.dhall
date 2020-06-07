let S = ../../Lib/SelectFiles.dhall
let D = S.PathPattern

let JobSpec = ../../Pipeline/JobSpec.dhall

in
JobSpec::{
  dirtyWhen = [
    S::{ dir = Some [D.Lit "buildkite/", D.Any], exts = Some ["dhall"] },
    S.contains "buildkite/Makefile",
    S.exactly "buildkite/scripts/generate-jobs" "sh"
  ],
  name = "CheckDhall"
}
