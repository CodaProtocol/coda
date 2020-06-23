-- A DSL for manipulating commands and their execution environments
let Prelude = ../External/Prelude.dhall
let P = Prelude
let List/map = P.List.map
let List/null = P.List.null
let Optional/toList = P.Optional.toList
let Optional/default = P.Optional.default
let Optional/map = P.Optional.map
let List/concatMap = P.List.concatMap
let List/concat = P.List.concat
let Text/concatSep = P.Text.concatSep
let Text/concatMap = P.Text.concatMap

-- abstract out defaultEnv so tests are less verbose
let module = \(environment : List Text) ->

  let Docker = {
    Type = {
      image : Text,
      extraEnv : List Text
    },
    default = {
      extraEnv = ([] : List Text)
    }
  }

  let Cmd = { line: Text, readable: Optional Text }
  let run : Text -> Cmd =
    \(script: Text) -> { line = script, readable = Some script }

  let quietly : Text -> Cmd =
    \(script: Text) -> { line = script, readable = None Text }
  let true : Cmd = quietly "true"
  let false : Cmd = quietly "false"

  let inDocker : Docker.Type -> Cmd -> Cmd =
    \(docker : Docker.Type) ->
    \(inner : Cmd) ->
    let envVars =
      Text/concatMap
        Text
        (\(var : Text) -> " --env ${var}")
        (docker.extraEnv # environment)
    let outerDir : Text =
      "/var/buildkite/builds/\$BUILDKITE_AGENT_NAME/\$BUILDKITE_ORGANIZATION_SLUG/\$BUILDKITE_PIPELINE_SLUG"
    in
    { line = "docker run -it --rm --init --volume ${outerDir}:/workdir --workdir /workdir${envVars} ${docker.image} bash -c '${inner.line}'"
    , readable = Optional/map Text Text (\(readable : Text) -> "Docker@${docker.image} ( ${readable} )") inner.readable
    }

  let runInDocker : Docker.Type -> Text -> Cmd =
    \(docker : Docker.Type) ->
    \(script : Text) ->
    inDocker docker (run script)

  let CompoundCmd = {
    Type = {
      -- unpackage data downloaded from gcloud (only on cache hit)
      unpackage : Optional Cmd,
      -- run your command to create data (only on miss)
      create : Cmd,
      -- package data before an upload to gcloud (only on miss)
      package : Cmd
    },
    default = {=}
  }

  let format : Cmd -> Text =
    \(cmd : Cmd) -> cmd.line

  -- Loads through cache, innards with docker, buildkite-agent interactions outside, continues in docker after hit or miss with continuation
  let cacheThrough : Docker.Type -> Text -> CompoundCmd.Type -> Cmd -> Cmd =
    \(docker : Docker.Type) ->
    \(cachePath : Text) ->
    \(cmd : CompoundCmd.Type) ->
    \(continuation : Cmd) ->
      let hitScript =
        ( format cmd.create ) ++ " && " ++
        ( format cmd.package ) ++ " && " ++
        ( format continuation )
      let missCmd =
        runInDocker docker hitScript
      let hitCmd =
        runInDocker docker (
          ( format (Optional/default Cmd true cmd.unpackage) ) ++ " && " ++
          ( format continuation )
        )
      in
      { line = "./buildkite/scripts/cache-through.sh ${cachePath} \"${format hitCmd}\" \"${format missCmd}\""
      , readable =
        let makeSnippet = \(label : Text) -> \(cmd : Cmd) ->
          Optional/toList Text (
            Optional/map
              Text
              Text
              (\(readable : Text) -> "${label} = ${readable}")
              cmd.readable)
        let details =
          List/concat
            Text
            [
              makeSnippet "onHit" hitCmd,
              makeSnippet "onMiss" missCmd
            ]
        let summary =
          Text/concatSep
            " ; "
            details
        in
        if List/null Text details then
          None Text
        else
          Some "Cache@${cachePath} ( ${summary} )"
      }
  in

  { Type = Cmd
  , Docker = Docker
  , CompoundCmd = CompoundCmd
  , quietly = quietly
  , run = run
  , true = true
  , false = false
  , runInDocker = runInDocker
  , inDocker = inDocker
  , cacheThrough = cacheThrough
  , format = format
  }

let tests =
  let M = module ["TEST"] in

  let dockerExample = assert :
  { line =
"docker run -it --rm --init --volume /var/buildkite/builds/$BUILDKITE_AGENT_NAME/$BUILDKITE_ORGANIZATION_SLUG/$BUILDKITE_PIPELINE_SLUG:/workdir --workdir /workdir --env ENV1 --env ENV2 --env TEST foo/bar:tag bash -c 'echo hello'"
  , readable =
    Some "Docker@foo/bar:tag ( echo hello )"
  }
  ===
    M.inDocker
      M.Docker::{
        image = "foo/bar:tag",
        extraEnv = [ "ENV1", "ENV2" ]
      }
      ( M.run "echo hello" )

  let cacheExample = assert :
''
  ./buildkite/scripts/cache-through.sh data.tar "docker run -it --rm --init --volume /var/buildkite/builds/$BUILDKITE_AGENT_NAME/$BUILDKITE_ORGANIZATION_SLUG/$BUILDKITE_PIPELINE_SLUG:/workdir --workdir /workdir --env ENV1 --env ENV2 --env TEST foo/bar:tag bash -c 'tar xvf data.tar -C /tmp/data && echo continue'" "docker run -it --rm --init --volume /var/buildkite/builds/$BUILDKITE_AGENT_NAME/$BUILDKITE_ORGANIZATION_SLUG/$BUILDKITE_PIPELINE_SLUG:/workdir --workdir /workdir --env ENV1 --env ENV2 --env TEST foo/bar:tag bash -c 'echo hello > /tmp/data/foo.txt && tar cvf data.tar /tmp/data && echo continue'"''
===
  M.format (
    M.cacheThrough
      M.Docker::{
        image = "foo/bar:tag",
        extraEnv = [ "ENV1", "ENV2" ]
      }
      "data.tar"
      M.CompoundCmd::{
        unpackage = Some (M.run "tar xvf data.tar -C /tmp/data"),
        create = M.run "echo hello > /tmp/data/foo.txt",
        package = M.run "tar cvf data.tar /tmp/data"
      }
      (M.run "echo continue")
  )
  in
  ""

in
module ../Constants/ContainerEnvVars.dhall

