open Core
open Async
open Coda_base
open Coda_state
open Blockchain_snark
open Snark_params
open Fold_lib

type ledger_proof = Ledger_proof.Prod.t

module Worker_state = struct
  module type S = sig
    val verify_wrap : Protocol_state.Value.t -> Tock.Proof.t -> bool

    val verify_transaction_snark :
      Transaction_snark.t -> message:Sok_message.t -> bool
  end

  type init_arg = unit [@@deriving bin_io]

  type t = (module S) Deferred.t

  let create () : t Deferred.t =
    Deferred.return
      (let%map bc_vk = Snark_keys.blockchain_verification ()
       and tx_vk = Snark_keys.transaction_verification () in
       let self_wrap = tock_vk_to_bool_list bc_vk.wrap in
       let module T = Transaction_snark.Verification.Make (struct
         let keys = tx_vk
       end) in
       let module B = Blockchain_transition.Make (T) in
       let module M = struct
         let verify_wrap state proof =
           let fold =
             Fold.(
               group3 ~default:false (of_list self_wrap)
               +> State_hash.fold (Protocol_state.hash state))
           in
           (* state, fold, hashes, *)
           let instance_hash =
             Tick.Pedersen.digest_fold Hash_prefix.transition_system_snark fold
           in
           Logger.error (Logger.create ()) ~module_:__MODULE__
             ~location:__LOC__
             ~metadata:
               [ ("proof", `String (Proof.sexp_of_t proof |> Sexp.to_string))
               ; ("state", Protocol_state.Value.to_yojson state)
               ; ( "fold"
                 , `List
                     ( Fold.to_list fold
                     |> List.map ~f:(fun t3 ->
                            `String
                              ( [%sexp_of: bool Tuple_lib.Triple.t] t3
                              |> Sexp.to_string ) ) ) )
               ; ("instance_hash", `String (Tick.Field.to_string instance_hash))
               ; ( "bc_vk_wrap"
                 , `String (Tock_backend.Verification_key.to_string bc_vk.wrap)
                 ) ]
             {raw|Validating the wrap verification data:
               [ $proof
               , $state
               , $fold
               , $instance_hash
               , $bc_vk_wrap ] |raw} ;
           Tock.verify proof bc_vk.wrap
             Tock.Data_spec.[Wrap_input.typ]
             (Wrap_input.of_tick_field instance_hash)

         let verify_transaction_snark = T.verify
       end in
       (module M : S))

  let get = Fn.id
end

module Worker = struct
  module T = struct
    module F = Rpc_parallel.Function

    type 'w functions =
      { verify_blockchain: ('w, Blockchain.t, bool) F.t
      ; verify_transaction_snark:
          ('w, Transaction_snark.t * Sok_message.t, bool) F.t }

    module Worker_state = Worker_state

    module Connection_state = struct
      type init_arg = unit [@@deriving bin_io]

      type t = unit
    end

    module Functions
        (C : Rpc_parallel.Creator
             with type worker_state := Worker_state.t
              and type connection_state := Connection_state.t) =
    struct
      let verify_blockchain (w : Worker_state.t) (chain : Blockchain.t) =
        match Coda_compile_config.proof_level with
        | "full" ->
            let%map (module M) = Worker_state.get w in
            M.verify_wrap chain.state chain.proof
        | "check" | "none" ->
            Deferred.return true
        | _ ->
            failwith "unknown proof_level"

      let verify_transaction_snark (w : Worker_state.t) (p, message) =
        match Coda_compile_config.proof_level with
        | "full" ->
            let%map (module M) = Worker_state.get w in
            M.verify_transaction_snark p ~message
        | "check" | "none" ->
            Deferred.return true
        | _ ->
            failwith "unknown proof_level"

      let functions =
        let f (i, o, f) =
          C.create_rpc
            ~f:(fun ~worker_state ~conn_state:_ i -> f worker_state i)
            ~bin_input:i ~bin_output:o ()
        in
        { verify_blockchain= f (Blockchain.bin_t, Bool.bin_t, verify_blockchain)
        ; verify_transaction_snark=
            f
              ( [%bin_type_class:
                  Transaction_snark.Stable.V1.t * Sok_message.Stable.V1.t]
              , Bool.bin_t
              , verify_transaction_snark ) }

      let init_worker_state () = Worker_state.create ()

      let init_connection_state ~connection:_ ~worker_state:_ = return
    end
  end

  include Rpc_parallel.Make (T)
end

type t = Worker.Connection.t

(* TODO: investigate why conf_dir wasn't being used *)
let create () =
  let%map connection, process =
    Worker.spawn_in_foreground_exn ~connection_timeout:(Time.Span.of_min 1.)
      ~on_failure:Error.raise ~shutdown_on:Disconnect
      ~connection_state_init_arg:() ()
  in
  File_system.dup_stdout process ;
  File_system.dup_stderr process ;
  connection

let verify_blockchain_snark t chain =
  Worker.Connection.run t ~f:Worker.functions.verify_blockchain ~arg:chain

let verify_transaction_snark t snark ~message =
  Worker.Connection.run t ~f:Worker.functions.verify_transaction_snark
    ~arg:(snark, message)
