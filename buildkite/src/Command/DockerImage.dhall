-- Execute Docker artifact release script according to build scoped DOCKER_DEPLOY_ENV

let Prelude = ../External/Prelude.dhall

let Command = ./Base.dhall
let Size = ./Size.dhall

let Cmd = ../Lib/Cmds.dhall

let DockerLogin = ../Command/DockerLogin/Type.dhall


let defaultArtifactStep = { name = "GitEnvUpload", key = "upload-git-env", deploy_env_file = "export-git-env-vars.sh" }

let ReleaseSpec = {
  Type = {
    deps : List Command.TaggedKey.Type,
    network: Text,
    service: Text,
    version: Text,
    deb_codename: Text,
    deb_release: Text,
    deb_version: Text,
    extra_args: Text,
    step_key: Text
  },
  default = {
    deps = [] : List Command.TaggedKey.Type,
    network = "devnet",
    version = "\\\${MINA_DOCKER_TAG}",
    service = "\\\${MINA_SERVICE}",
    deb_codename = "stretch",
    deb_release = "\\\${MINA_DEB_RELEASE}",
    deb_version = "\\\${MINA_DEB_VERSION}",
    extra_args = "",
    step_key = "daemon-devnet-docker-image"
  }
}

let generateStep = \(spec : ReleaseSpec.Type) ->

    let commands : List Cmd.Type =
    [
        Cmd.run (
          "[ ! -f ${defaultArtifactStep.deploy_env_file} ] && buildkite-agent artifact download --build \\\$BUILDKITE_BUILD_ID " ++
              "--include-retried-jobs --step _${defaultArtifactStep.name}-${defaultArtifactStep.key} ${defaultArtifactStep.deploy_env_file} ."
        ),
        Cmd.run (
          "export MINA_DEB_CODENAME=${spec.deb_codename} && source ${defaultArtifactStep.deploy_env_file} && ./scripts/release-docker.sh " ++
              "--service ${spec.service} --version ${spec.version}-${spec.network} --network ${spec.network} --deb-codename ${spec.deb_codename} --deb-release ${spec.deb_release} --deb-version ${spec.deb_version} --extra-args \\\"${spec.extra_args}\\\""
        )
    ]

    in

    Command.build
      Command.Config::{
        commands  = commands,
        label = "Release Docker Image: ${spec.step_key}",
        key = spec.step_key,
        target = Size.XLarge,
        docker_login = Some DockerLogin::{=},
        depends_on = spec.deps
      }

in

{ generateStep = generateStep, ReleaseSpec = ReleaseSpec }
