open Tc;
open Bindings;

module Command = {
  type t = {
    executable: string,
    args: array(string),
    env: Js.Dict.t(string),
  };
};

let (^/) = Filename.concat;

[@bs.module "electron"] [@bs.scope ("app")]
external getPath: string => string = "getPath";

let codaCommand = (~port, ~extraArgs) => {
  let del = Node.Path.delimiter;
  let env = ChildProcess.Process.env;
  let path = Js.Dict.get(env, "PATH") |> Option.with_default(~default="");
  let installPath = getPath("userData") ++ del ++ "coda";
  // NOTE: This is a workaround for keys that's very specific to unix based systems
  let keysPath = "/usr/local/var/coda/keys";
  Js.Dict.set(env, "PATH", path ++ del ++ installPath);
  Js.Dict.set(env, "MINA_LIBP2P_HELPER_PATH", installPath ++ del ++ "libp2p-helper");
  {
    Command.executable: "coda",
    args:
      Array.append(
        [|
          "daemon",
          "-rest-port",
          Js.Int.toString(port),
          "-genesis-ledger-dir",
          keysPath,
          "-config-directory",
          ProjectRoot.userData ^/ "mina-config",
        |],
        extraArgs,
      ),
    env,
  };
};

let fakerCommand = (~port) => {
  Command.executable: "node",
  args: [|
    Filename.concat(
      ProjectRoot.resource,
      "node_modules/graphql-faker/dist/index.js",
    ),
    "--port",
    string_of_int(port),
    "--",
    "schema.graphql",
  |],
  env: ChildProcess.Process.env,
};

module Process = {
  let logfileName = ProjectRoot.userData ^/ "coda-wallet.log";
  let start = (command: Command.t) => {
    let {Command.executable, args} = command;
    print_endline({j|Logging to `$logfileName`|j});

    /*
     child_process.spawn communicates with processes using sockets.
     When the proposer process starts up in the daemon, stdout and stderr
     are accessed using a helper function from async_unix:
     https://github.com/janestreet/async_unix/blob/ff13d69c96f4b857737263910b3f6b08311b5dfc/src/writer0.ml#L1465
     This causes async_unix to attempt to infer the type of stdout/err
     using and check if it has SO_ACCEPTCONN set using getsockopt.
     This flag is not supported on Mac, so it crashes with "Protocol not available"

     As a workaround, we have to redirect all output to a file.

     If this issue is ever resolved or we stop using async_unix, this code can be simplified:
     https://github.com/janestreet/async_unix/issues/15
     */

    let logFile = Fs.openSync(logfileName, "w");
    let log = s => {
      let str = "Wallet: " ++ s ++ "\n";
      Fs.writeSync(logFile, str);
      print_endline(str);
    };
    log({j|Started $executable with $args|j});
    let process =
      ChildProcess.spawn(
        command.executable,
        command.args,
        ChildProcess.spawnOptions(
          ~env=command.env,
          ~stdio=
            ChildProcess.makeIOTriple(
              `Ignore,
              `Stream(logFile),
              `Stream(logFile),
            ),
          (),
        ),
      );

    ChildProcess.Process.onError(process, e =>
      log(
        "Daemon process "
        ++ command.executable
        ++ " crashed. Logs are in `"
        ++ logfileName
        ++ "`. Error:"
        ++ ChildProcess.Error.messageGet(e),
      )
    );

    ChildProcess.Process.onExit(
      process,
      fun
      | `Code(code) =>
        if (code != 0) {
          log(
            Printf.sprintf(
              "Daemon process %s died with non-zero exit code: %d. Logs are in `%s`\n",
              command.executable,
              code,
              logfileName,
            ),
          );
        } else {
          log("Shutting down daemon process: " ++ command.executable);
        }
      | `Signal(signal) => {
          log({j|"Daemon process $executable died via signal: $signal"|j});
        },
    );

    process;
  };
};

module CodaProcess = {
  type t = ChildProcess.Process.t;

  let waitExit: t => Task.t('x, [> | `Code(int) | `Signal(string)]) =
    t => ChildProcess.Process.onExitTask(t);

  let kill: t => unit = t => ChildProcess.Process.kill(t, "SIGINT");

  let port = 0xc0d;

  let defaultPeers = [
    "/dns4/peer1-rising-phoenix.o1test.net/tcp/8303/p2p/12D3KooWHMmfuS9DmmK9eH4GC31arDhbtHEBQzX6PwPtQftxzwJs",
    "/dns4/peer2-rising-phoenix.o1test.net/tcp/8303/p2p/12D3KooWAux9MAW1yAdD8gsDbYHmgVjRvdfYkpkfX7AnyGvQaRPF",
    "/dns4/peer3-rising-phoenix.o1test.net/tcp/8303/p2p/12D3KooWCZA4pPWmDAkQf6riDQ3XMRN5k99tCsiRhBAPZCkA8re7",
  ];

  let defaultArgs =
    List.foldr(
      ~f=(peer, acc) => List.concat([["-peer", peer], acc]),
      defaultPeers,
      ~init=["-discovery-port", "8303"],
    );

  let start: list(string) => t =
    args => {
      switch (Js.Dict.get(ChildProcess.Process.env, "GRAPHQL_BACKEND")) {
      | Some("faker") => Process.start(fakerCommand(~port))
      | _ =>
        Process.start(codaCommand(~port, ~extraArgs=args |> Array.fromList))
      };
    };
};
