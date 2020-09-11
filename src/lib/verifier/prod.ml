(* prod.ml *)

open Core_kernel
open Async
open Coda_base
open Coda_state
open Blockchain_snark

type ledger_proof = Ledger_proof.Prod.t

module Worker_state = struct
  module type S = sig
    val verify_blockchain_snark : Protocol_state.Value.t -> Proof.t -> bool

    val verify_transaction_snarks :
      (Transaction_snark.t * Sok_message.t) list -> bool
  end

  (* bin_io required by rpc_parallel *)
  type init_arg =
    { conf_dir: string option
    ; logger: Logger.Stable.Latest.t
    ; proof_level: Genesis_constants.Proof_level.Stable.Latest.t }
  [@@deriving bin_io_unversioned]

  type t = (module S)

  let create {logger; proof_level; _} : t Deferred.t =
    Memory_stats.log_memory_stats logger ~process:"verifier" ;
    match proof_level with
    | Full ->
        Deferred.return
          (let bc_vk = Precomputed_values.blockchain_verification ()
           and tx_vk = Precomputed_values.transaction_verification () in
           let module M = struct
             let verify_blockchain_snark state proof =
               Blockchain_snark.Blockchain_snark_state.verify state proof
                 ~key:bc_vk

             let verify_transaction_snarks ts =
               match
                 Or_error.try_with (fun () ->
                     Transaction_snark.verify ~key:tx_vk ts )
               with
               | Ok result ->
                   result
               | Error e ->
                   [%log error]
                     ~metadata:[("error", `String (Error.to_string_hum e))]
                     "Verifier threw an exception while verifying transaction \
                      snark" ;
                   failwith "Verifier crashed"
           end in
           (module M : S))
    | Check | None ->
        Deferred.return
        @@ ( module struct
             let verify_blockchain_snark _ _ = true

             let verify_transaction_snarks _ = true
           end
           : S )

  let get = Fn.id
end

module Worker = struct
  module T = struct
    module F = Rpc_parallel.Function

    type 'w functions =
      { verify_blockchain: ('w, Blockchain.t, bool) F.t
      ; verify_transaction_snarks:
          ('w, (Transaction_snark.t * Sok_message.t) list, bool) F.t }

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
      let verify_blockchain (w : Worker_state.t) (chain : Blockchain.t) =
        let (module M) = Worker_state.get w in
        Deferred.return (M.verify_blockchain_snark chain.state chain.proof)

      let verify_transaction_snarks (w : Worker_state.t) ts =
        let (module M) = Worker_state.get w in
        Deferred.return (M.verify_transaction_snarks ts)

      let functions =
        let f (i, o, f) =
          C.create_rpc
            ~f:(fun ~worker_state ~conn_state:_ i -> f worker_state i)
            ~bin_input:i ~bin_output:o ()
        in
        { verify_blockchain=
            f (Blockchain.Stable.Latest.bin_t, Bool.bin_t, verify_blockchain)
        ; verify_transaction_snarks=
            f
              ( [%bin_type_class:
                  ( Transaction_snark.Stable.Latest.t
                  * Sok_message.Stable.Latest.t )
                  list]
              , Bool.bin_t
              , verify_transaction_snarks ) }

      let init_worker_state Worker_state.{conf_dir; logger; proof_level} =
        ( if Option.is_some conf_dir then
          let max_size = 256 * 1024 * 512 in
          Logger.Consumer_registry.register ~id:"default"
            ~processor:(Logger.Processor.raw ())
            ~transport:
              (Logger.Transport.File_system.dumb_logrotate
                 ~directory:(Option.value_exn conf_dir)
                 ~log_filename:"coda-verifier.log" ~max_size) ) ;
        [%log info] "Verifier started" ;
        Worker_state.create {conf_dir; logger; proof_level}

      let init_connection_state ~connection:_ ~worker_state:_ () =
        Deferred.unit
    end
  end

  include Rpc_parallel.Make (T)
end

type t = ledger_proof Generic.t

(* TODO: investigate why conf_dir wasn't being used *)
let create ~logger ~proof_level ~pids ~conf_dir : t Deferred.t =
  let on_failure err =
    [%log error] "Verifier process failed with error $err"
      ~metadata:[("err", `String (Error.to_string_hum err))] ;
    Error.raise err
  in
  let%map connection, process =
    Worker.spawn_in_foreground_exn ~connection_timeout:(Time.Span.of_min 1.)
      ~on_failure ~shutdown_on:Disconnect ~connection_state_init_arg:()
      {conf_dir; logger; proof_level}
  in
  [%log info]
    "Daemon started process of kind $process_kind with pid $verifier_pid"
    ~metadata:
      [ ("verifier_pid", `Int (Process.pid process |> Pid.to_int))
      ; ( "process_kind"
        , `String Child_processes.Termination.(show_process_kind Verifier) ) ] ;
  Child_processes.Termination.register_process pids process
    Child_processes.Termination.Verifier ;
  don't_wait_for
  @@ Pipe.iter
       (Process.stdout process |> Reader.pipe)
       ~f:(fun stdout ->
         return
         @@ [%log debug] "Verifier stdout: $stdout"
              ~metadata:[("stdout", `String stdout)] ) ;
  don't_wait_for
  @@ Pipe.iter
       (Process.stderr process |> Reader.pipe)
       ~f:(fun stderr ->
         return
         @@ [%log error] "Verifier stderr: $stderr"
              ~metadata:[("stderr", `String stderr)] ) ;
  let res : t =
    { verify_blockchain_snark=
        (fun chain ->
          Worker.Connection.run connection
            ~f:Worker.functions.verify_blockchain ~arg:chain )
    ; verify_transaction_snarks=
        (fun ts ->
          Worker.Connection.run connection
            ~f:Worker.functions.verify_transaction_snarks ~arg:ts ) }
  in
  res

let verify_blockchain_snark (t : t) chain = t.verify_blockchain_snark chain

let verify_transaction_snarks (t : t) ts = t.verify_transaction_snarks ts

let of_generic t : t = t
