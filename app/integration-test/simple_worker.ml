open Core
open Async
open Spawner

module State_worker = struct
  type t =
    {mutable state: int; reader: int Pipe.Reader.t; writer: int Pipe.Writer.t}

  type input = int [@@deriving bin_io]

  type state = int [@@deriving bin_io]

  let create input =
    let reader, writer = Pipe.create () in
    return @@ {state= input; reader; writer}

  let new_states {reader} = reader

  let run t =
    t.state <- 2 * t.state ;
    Pipe.write t.writer t.state
end

module Worker = Parallel_worker.Make (State_worker)
module Master = Master.Make (Worker) (Int)

let master_command =
  let open Command.Let_syntax in
  let%map {host; executable_path; log_dir} = Command_util.config_arguments in
  fun () ->
    let open Deferred.Let_syntax in
    let open Master in
    let t = create () in
    let%bind log_dir = File_system.create_dir log_dir in
    let config = {Config.host; executable_path; log_dir} and process = 1 in
    let%bind () = add t 1 process ~config in
    let%bind () = Option.value_exn (run t process) in
    let reader = new_states t and expected_state = 2 in
    let%map _, actual_state = Linear_pipe.read_exn reader in
    assert (expected_state = actual_state)

let name = "simple-worker"

let command =
  Command.async master_command
    ~summary:"Tests that a worker can send updates to master"
