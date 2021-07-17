(* uptime_snark_worker.ml *)

open Core_kernel
open Async
open Mina_base
module Prod = Snark_worker__Prod.Inputs

module Worker_state = struct
  module type S = sig
    val perform_single :
         Sok_message.t * Prod.single_spec
      -> (Ledger_proof.t * Time.Span.t) Deferred.Or_error.t
  end

  (* bin_io required by rpc_parallel *)
  type init_arg = Logger.Stable.Latest.t [@@deriving bin_io_unversioned]

  type t = (module S)

  let create ~logger : t Deferred.t =
    Memory_stats.log_memory_stats logger ~process:"uptime service SNARK worker" ;
    Deferred.return
      (let module M = struct
         let perform_single (message, single_spec) =
           let%bind (worker_state : Prod.Worker_state.t) =
             Prod.Worker_state.create
               ~constraint_constants:
                 Genesis_constants.Constraint_constants.compiled
               ~proof_level:Full ()
           in
           Prod.perform_single worker_state ~message single_spec
       end in
      (module M : S))

  let get = Fn.id
end

module Worker = struct
  module T = struct
    module F = Rpc_parallel.Function

    type 'w functions =
      { perform_single:
          ( 'w
          , Sok_message.t * Prod.single_spec
          , (Ledger_proof.t * Time.Span.t) Or_error.t )
          F.t }

    module Worker_state = Worker_state

    module Connection_state = struct
      (* bin_io required by rpc_parallel *)
      type init_arg = unit [@@deriving bin_io_unversioned]

      type t = unit
    end

    module Functions
        (C : Rpc_parallel.Creator
             with type worker_state := Worker_state.t
              and type connection_state := Connection_state.t) =
    struct
      let perform_single (w : Worker_state.t) msg_and_single_spec =
        let (module M) = Worker_state.get w in
        M.perform_single msg_and_single_spec

      let functions =
        let f (i, o, f) =
          C.create_rpc
            ~f:(fun ~worker_state ~conn_state:_ i -> f worker_state i)
            ~bin_input:i ~bin_output:o ()
        in
        { perform_single=
            f
              ( [%bin_type_class:
                  Sok_message.Stable.Latest.t * Prod.single_spec]
              , [%bin_type_class:
                  (Ledger_proof.Stable.Latest.t * Time.Span.t) Or_error.t]
              , perform_single ) }

      let init_worker_state logger =
        [%log info] "Uptime SNARK worker started" ;
        Worker_state.create ~logger

      let init_connection_state ~connection:_ ~worker_state:_ () =
        Deferred.unit
    end
  end

  include Rpc_parallel.Make (T)
end

type t =
  { connection: Worker.Connection.t
  ; process: Process.t
  ; logger: Logger.Stable.Latest.t }

let create ~logger ~pids : t Deferred.t =
  let on_failure err =
    [%log error] "Uptime service SNARK worker process failed with error $err"
      ~metadata:[("err", Error_json.error_to_yojson err)] ;
    Error.raise err
  in
  [%log info] "Starting a new uptime service SNARK worker process" ;
  let%map connection, process =
    Worker.spawn_in_foreground_exn ~connection_timeout:(Time.Span.of_min 1.)
      ~on_failure ~shutdown_on:Disconnect ~connection_state_init_arg:() logger
  in
  [%log info]
    "Daemon started process of kind $process_kind with pid \
     $uptime_snark_worker_pid"
    ~metadata:
      [ ("uptime_snark_worker_pid", `Int (Process.pid process |> Pid.to_int))
      ; ( "process_kind"
        , `String
            Child_processes.Termination.(show_process_kind Uptime_snark_worker)
        ) ] ;
  Child_processes.Termination.register_process pids process
    Child_processes.Termination.Uptime_snark_worker ;
  don't_wait_for
  @@ Pipe.iter
       (Process.stdout process |> Reader.pipe)
       ~f:(fun stdout ->
         return
         @@ [%log debug] "Uptime SNARK worker stdout: $stdout"
              ~metadata:[("stdout", `String stdout)] ) ;
  don't_wait_for
  @@ Pipe.iter
       (Process.stderr process |> Reader.pipe)
       ~f:(fun stderr ->
         return
         @@ [%log error] "Uptime SNARK worker stderr: $stderr"
              ~metadata:[("stderr", `String stderr)] ) ;
  {connection; process; logger}

let perform_single {connection; _} ((_message, _single_spec) as arg) =
  Worker.Connection.run connection ~f:Worker.functions.perform_single ~arg
