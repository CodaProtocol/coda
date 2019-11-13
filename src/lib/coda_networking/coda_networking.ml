open Core_kernel
open Async
open Coda_base
open Coda_state
open Coda_transition
open Network_peer
open Network_pool
open Pipe_lib

let refused_answer_query_string = "Refused to answer_query"

type exn += No_initial_peers

(* INSTRUCTIONS FOR ADDING A NEW RPC:
 *   - define a new module under the Rpcs module
 *   - add an entry to the Rpcs.rpc GADT definition for the new module
 *   - add the new constructor for Rpcs.rpc to Rpcs.all_of_type_erased_rpc
 *   - add a pattern matching case to Rpcs.implementation_of_rpc mapping the
 *     new constructor to the new module for your RPC
 *)
module Rpcs = struct
  (* for versioning of the types here, see

     RFC 0012, and

     https://ocaml.janestreet.com/ocaml-core/latest/doc/async_rpc_kernel/Async_rpc_kernel/Versioned_rpc/

   *)

  module Get_staged_ledger_aux_and_pending_coinbases_at_hash = struct
    module Master = struct
      let name = "get_staged_ledger_aux_and_pending_coinbases_at_hash"

      module T = struct
        type query = State_hash.t

        type response =
          (Staged_ledger.Scan_state.t * Ledger_hash.t * Pending_coinbase.t)
          option
      end

      module Caller = T
      module Callee = T
    end

    include Master.T
    module M = Versioned_rpc.Both_convert.Plain.Make (Master)
    include M

    include Perf_histograms.Rpc.Plain.Extend (struct
      include M
      include Master
    end)

    module V1 = struct
      module T = struct
        type query = State_hash.Stable.V1.t [@@deriving bin_io, version {rpc}]

        type response =
          ( Staged_ledger.Scan_state.Stable.V1.t
          * Ledger_hash.Stable.V1.t
          * Pending_coinbase.Stable.V1.t )
          option
        [@@deriving bin_io, version {rpc}]

        let query_of_caller_model = Fn.id

        let callee_model_of_query = Fn.id

        let response_of_callee_model = Fn.id

        let caller_model_of_response = Fn.id
      end

      module T' =
        Perf_histograms.Rpc.Plain.Decorate_bin_io (struct
            include M
            include Master
          end)
          (T)

      include T'
      include Register (T')
    end
  end

  module Answer_sync_ledger_query = struct
    module Master = struct
      let name = "answer_sync_ledger_query"

      module T = struct
        type query = Ledger_hash.t * Sync_ledger.Query.t

        type response = Sync_ledger.Answer.t Core.Or_error.t
      end

      module Caller = T
      module Callee = T
    end

    include Master.T
    module M = Versioned_rpc.Both_convert.Plain.Make (Master)
    include M

    include Perf_histograms.Rpc.Plain.Extend (struct
      include M
      include Master
    end)

    module V1 = struct
      module T = struct
        type query = Ledger_hash.Stable.V1.t * Sync_ledger.Query.Stable.V1.t
        [@@deriving bin_io, sexp, version {rpc}]

        type response =
          Sync_ledger.Answer.Stable.V1.t Core.Or_error.Stable.V1.t
        [@@deriving bin_io, sexp, version {rpc}]

        let query_of_caller_model = Fn.id

        let callee_model_of_query = Fn.id

        let response_of_callee_model = Fn.id

        let caller_model_of_response = Fn.id
      end

      module T' =
        Perf_histograms.Rpc.Plain.Decorate_bin_io (struct
            include M
            include Master
          end)
          (T)

      include T'
      include Register (T')
    end
  end

  module Get_transition_chain = struct
    module Master = struct
      let name = "get_transition_chain"

      module T = struct
        type query = State_hash.t list [@@deriving sexp, to_yojson]

        type response = External_transition.t list option
      end

      module Caller = T
      module Callee = T
    end

    include Master.T
    module M = Versioned_rpc.Both_convert.Plain.Make (Master)
    include M

    include Perf_histograms.Rpc.Plain.Extend (struct
      include M
      include Master
    end)

    module V1 = struct
      module T = struct
        type query = State_hash.Stable.V1.t list
        [@@deriving bin_io, sexp, version {rpc}]

        type response = External_transition.Stable.V1.t list option
        [@@deriving bin_io, version {rpc}]

        let query_of_caller_model = Fn.id

        let callee_model_of_query = Fn.id

        let response_of_callee_model = Fn.id

        let caller_model_of_response = Fn.id
      end

      module T' =
        Perf_histograms.Rpc.Plain.Decorate_bin_io (struct
            include M
            include Master
          end)
          (T)

      include T'
      include Register (T')
    end
  end

  module Get_transition_chain_proof = struct
    module Master = struct
      let name = "get_transition_chain_proof"

      module T = struct
        type query = State_hash.t [@@deriving sexp, to_yojson]

        type response = (State_hash.t * State_body_hash.t list) option
      end

      module Caller = T
      module Callee = T
    end

    include Master.T
    module M = Versioned_rpc.Both_convert.Plain.Make (Master)
    include M

    include Perf_histograms.Rpc.Plain.Extend (struct
      include M
      include Master
    end)

    module V1 = struct
      module T = struct
        type query = State_hash.Stable.V1.t
        [@@deriving bin_io, sexp, version {rpc}]

        type response =
          (State_hash.Stable.V1.t * State_body_hash.Stable.V1.t list) option
        [@@deriving bin_io, version {rpc}]

        let query_of_caller_model = Fn.id

        let callee_model_of_query = Fn.id

        let response_of_callee_model = Fn.id

        let caller_model_of_response = Fn.id
      end

      module T' =
        Perf_histograms.Rpc.Plain.Decorate_bin_io (struct
            include M
            include Master
          end)
          (T)

      include T'
      include Register (T')
    end
  end

  module Get_ancestry = struct
    module Master = struct
      let name = "get_ancestry"

      module T = struct
        type query = Consensus.Data.Consensus_state.Value.t
        [@@deriving sexp, to_yojson]

        type response =
          ( External_transition.t
          , State_body_hash.t list * External_transition.t )
          Proof_carrying_data.t
          option
      end

      module Caller = T
      module Callee = T
    end

    include Master.T
    module M = Versioned_rpc.Both_convert.Plain.Make (Master)
    include M

    include Perf_histograms.Rpc.Plain.Extend (struct
      include M
      include Master
    end)

    module V1 = struct
      module T = struct
        type query = Consensus.Data.Consensus_state.Value.Stable.V1.t
        [@@deriving bin_io, sexp, version {rpc}]

        type response =
          ( External_transition.Stable.V1.t
          , State_body_hash.Stable.V1.t list * External_transition.Stable.V1.t
          )
          Proof_carrying_data.Stable.V1.t
          option
        [@@deriving bin_io, version {rpc}]

        let query_of_caller_model = Fn.id

        let callee_model_of_query = Fn.id

        let response_of_callee_model = Fn.id

        let caller_model_of_response = Fn.id
      end

      module T' =
        Perf_histograms.Rpc.Plain.Decorate_bin_io (struct
            include M
            include Master
          end)
          (T)

      include T'
      include Register (T')
    end
  end

  module Ban_notify = struct
    module Master = struct
      let name = "ban_notify"

      module T = struct
        (* banned until this time *)
        type query = Core.Time.t [@@deriving sexp]

        type response = unit
      end

      module Caller = T
      module Callee = T
    end

    include Master.T
    module M = Versioned_rpc.Both_convert.Plain.Make (Master)
    include M

    include Perf_histograms.Rpc.Plain.Extend (struct
      include M
      include Master
    end)

    module V1 = struct
      module T = struct
        type query = Core.Time.Stable.V1.t
        [@@deriving bin_io, sexp, version {rpc}]

        type response = unit [@@deriving bin_io, version {rpc}]

        let query_of_caller_model = Fn.id

        let callee_model_of_query = Fn.id

        let response_of_callee_model = Fn.id

        let caller_model_of_response = Fn.id
      end

      module T' =
        Perf_histograms.Rpc.Plain.Decorate_bin_io (struct
            include M
            include Master
          end)
          (T)

      include T'
      include Register (T')
    end
  end

  module Get_bootstrappable_best_tip = struct
    module Master = struct
      let name = "get_bootstrappable_best_tip"

      module T = struct
        type query = Consensus.Data.Consensus_state.Value.t
        [@@deriving sexp, to_yojson]

        type response =
          ( External_transition.t
          , State_body_hash.t list * External_transition.t )
          Proof_carrying_data.t
          option
      end

      module Caller = T
      module Callee = T
    end

    include Master.T
    module M = Versioned_rpc.Both_convert.Plain.Make (Master)
    include M

    include Perf_histograms.Rpc.Plain.Extend (struct
      include M
      include Master
    end)

    module V1 = struct
      module T = struct
        type query = Consensus.Data.Consensus_state.Value.Stable.V1.t
        [@@deriving bin_io, sexp, version {rpc}]

        type response =
          ( External_transition.Stable.V1.t
          , State_body_hash.Stable.V1.t list * External_transition.Stable.V1.t
          )
          Proof_carrying_data.Stable.V1.t
          option
        [@@deriving bin_io, version {rpc}]

        let query_of_caller_model = Fn.id

        let callee_model_of_query = Fn.id

        let response_of_callee_model = Fn.id

        let caller_model_of_response = Fn.id
      end

      module T' =
        Perf_histograms.Rpc.Plain.Decorate_bin_io (struct
            include M
            include Master
          end)
          (T)

      include T'
      include Register (T')
    end
  end

  type ('query, 'response) rpc =
    | Get_staged_ledger_aux_and_pending_coinbases_at_hash
        : ( Get_staged_ledger_aux_and_pending_coinbases_at_hash.query
          , Get_staged_ledger_aux_and_pending_coinbases_at_hash.response )
          rpc
    | Answer_sync_ledger_query
        : ( Answer_sync_ledger_query.query
          , Answer_sync_ledger_query.response )
          rpc
    | Get_transition_chain
        : (Get_transition_chain.query, Get_transition_chain.response) rpc
    | Get_transition_chain_proof
        : ( Get_transition_chain_proof.query
          , Get_transition_chain_proof.response )
          rpc
    | Get_ancestry : (Get_ancestry.query, Get_ancestry.response) rpc
    | Ban_notify : (Ban_notify.query, Ban_notify.response) rpc
    | Get_bootstrappable_best_tip
        : ( Get_bootstrappable_best_tip.query
          , Get_bootstrappable_best_tip.response )
          rpc
    | Consensus_rpc : ('q, 'r) Consensus.Hooks.Rpcs.rpc -> ('q, 'r) rpc

  type rpc_handler =
    | Rpc_handler : ('q, 'r) rpc * ('q, 'r) Rpc_intf.rpc_fn -> rpc_handler

  let implementation_of_rpc : type q r.
      (q, r) rpc -> (q, r) Rpc_intf.rpc_implementation = function
    | Get_staged_ledger_aux_and_pending_coinbases_at_hash ->
        (module Get_staged_ledger_aux_and_pending_coinbases_at_hash)
    | Answer_sync_ledger_query ->
        (module Answer_sync_ledger_query)
    | Get_transition_chain ->
        (module Get_transition_chain)
    | Get_transition_chain_proof ->
        (module Get_transition_chain_proof)
    | Get_ancestry ->
        (module Get_ancestry)
    | Ban_notify ->
        (module Ban_notify)
    | Get_bootstrappable_best_tip ->
        (module Get_bootstrappable_best_tip)
    | Consensus_rpc rpc ->
        Consensus.Hooks.Rpcs.implementation_of_rpc rpc

  let match_handler : type q r.
         rpc_handler
      -> (q, r) rpc
      -> do_:((q, r) Rpc_intf.rpc_fn -> 'a)
      -> 'a option =
   fun handler rpc ~do_ ->
    match (rpc, handler) with
    | ( Get_staged_ledger_aux_and_pending_coinbases_at_hash
      , Rpc_handler (Get_staged_ledger_aux_and_pending_coinbases_at_hash, f) )
      ->
        Some (do_ f)
    | Answer_sync_ledger_query, Rpc_handler (Answer_sync_ledger_query, f) ->
        Some (do_ f)
    | Get_transition_chain, Rpc_handler (Get_transition_chain, f) ->
        Some (do_ f)
    | Get_transition_chain_proof, Rpc_handler (Get_transition_chain_proof, f)
      ->
        Some (do_ f)
    | Get_ancestry, Rpc_handler (Get_ancestry, f) ->
        Some (do_ f)
    | Ban_notify, Rpc_handler (Ban_notify, f) ->
        Some (do_ f)
    | Get_bootstrappable_best_tip, Rpc_handler (Get_bootstrappable_best_tip, f)
      ->
        Some (do_ f)
    | Consensus_rpc rpc_a, Rpc_handler (Consensus_rpc rpc_b, f) ->
        Consensus.Hooks.Rpcs.match_handler (Rpc_handler (rpc_b, f)) rpc_a ~do_
    | _ ->
        None
end

module Gossip_net = Gossip_net.Make (Rpcs)

module Config = struct
  type log_gossip_heard =
    {snark_pool_diff: bool; transaction_pool_diff: bool; new_state: bool}
  [@@deriving make]

  type t =
    { logger: Logger.t
    ; trust_system: Trust_system.t
    ; time_controller: Block_time.Controller.t
    ; consensus_local_state: Consensus.Data.Local_state.t
    ; creatable_gossip_net: Gossip_net.Any.creatable
    ; log_gossip_heard: log_gossip_heard }
  [@@deriving make]
end

type t =
  { logger: Logger.t
  ; trust_system: Trust_system.t
  ; gossip_net: Gossip_net.Any.t
  ; states:
      (External_transition.t Envelope.Incoming.t * Block_time.t)
      Strict_pipe.Reader.t
  ; transaction_pool_diffs:
      Transaction_pool.Resource_pool.Diff.t Envelope.Incoming.t
      Linear_pipe.Reader.t
  ; snark_pool_diffs:
      Snark_pool.Resource_pool.Diff.t Envelope.Incoming.t Linear_pipe.Reader.t
  ; online_status: [`Offline | `Online] Broadcast_pipe.Reader.t
  ; first_received_message_signal: unit Ivar.t }
[@@deriving fields]

let offline_time =
  Block_time.Span.of_ms @@ Int64.of_int Consensus.Constants.inactivity_ms

let setup_timer time_controller sync_state_broadcaster =
  Block_time.Timeout.create time_controller offline_time ~f:(fun _ ->
      Broadcast_pipe.Writer.write sync_state_broadcaster `Offline
      |> don't_wait_for )

let online_broadcaster time_controller received_messages =
  let online_reader, online_writer = Broadcast_pipe.create `Offline in
  let init =
    Block_time.Timeout.create time_controller
      (Block_time.Span.of_ms Int64.zero)
      ~f:ignore
  in
  Strict_pipe.Reader.fold received_messages ~init ~f:(fun old_timeout _ ->
      let%map () = Broadcast_pipe.Writer.write online_writer `Online in
      Block_time.Timeout.cancel time_controller old_timeout () ;
      setup_timer time_controller online_writer )
  |> Deferred.ignore |> don't_wait_for ;
  online_reader

let wrap_rpc_data_in_envelope conn data =
  let inet_addr = Unix.Inet_addr.of_string conn.Host_and_port.host in
  let sender = Envelope.Sender.Remote inet_addr in
  Envelope.Incoming.wrap ~data ~sender

let create (config : Config.t)
    ~(get_staged_ledger_aux_and_pending_coinbases_at_hash :
          State_hash.t Envelope.Incoming.t
       -> (Staged_ledger.Scan_state.t * Ledger_hash.t * Pending_coinbase.t)
          option
          Deferred.t)
    ~(answer_sync_ledger_query :
          (Ledger_hash.t * Ledger.Location.Addr.t Syncable_ledger.Query.t)
          Envelope.Incoming.t
       -> Sync_ledger.Answer.t Deferred.Or_error.t)
    ~(get_ancestry :
          Consensus.Data.Consensus_state.Value.t Envelope.Incoming.t
       -> ( External_transition.t
          , State_body_hash.t list * External_transition.t )
          Proof_carrying_data.t
          Deferred.Option.t)
    ~(get_bootstrappable_best_tip :
          Consensus.Data.Consensus_state.Value.t Envelope.Incoming.t
       -> ( External_transition.t
          , State_body_hash.t list * External_transition.t )
          Proof_carrying_data.t
          Deferred.Option.t)
    ~(get_transition_chain_proof :
          State_hash.t Envelope.Incoming.t
       -> (State_hash.t * State_body_hash.t list) Deferred.Option.t)
    ~(get_transition_chain :
          State_hash.t list Envelope.Incoming.t
       -> External_transition.t list Deferred.Option.t) =
  let run_for_rpc_result conn data ~f action_msg msg_args =
    let data_in_envelope = wrap_rpc_data_in_envelope conn data in
    let sender = Envelope.Incoming.sender data_in_envelope in
    let%bind () =
      Trust_system.(
        record_envelope_sender config.trust_system config.logger sender
          Actions.(Made_request, Some (action_msg, msg_args)))
    in
    let%bind result = f data_in_envelope in
    return (result, sender)
  in
  let record_unknown_item result sender action_msg msg_args =
    let%bind () =
      if Option.is_none result then
        Trust_system.(
          record_envelope_sender config.trust_system config.logger sender
            Actions.(Requested_unknown_item, Some (action_msg, msg_args)))
      else return ()
    in
    return result
  in
  (* each of the passed-in procedures expects an enveloped input, so
     we wrap the data received via RPC *)
  let get_staged_ledger_aux_and_pending_coinbases_at_hash_rpc conn ~version:_
      hash =
    let action_msg = "Staged ledger and pending coinbases at hash: $hash" in
    let msg_args = [("hash", State_hash.to_yojson hash)] in
    let%bind result, sender =
      run_for_rpc_result conn hash
        ~f:get_staged_ledger_aux_and_pending_coinbases_at_hash action_msg
        msg_args
    in
    record_unknown_item result sender action_msg msg_args
  in
  let answer_sync_ledger_query_rpc conn ~version:_ ((hash, query) as sync_query)
      =
    let%bind result, sender =
      run_for_rpc_result conn sync_query ~f:answer_sync_ledger_query
        "Answer_sync_ledger_query: $query"
        [("query", Sync_ledger.Query.to_yojson query)]
    in
    let%bind () =
      match result with
      | Ok _ ->
          return ()
      | Error err ->
          (* N.B.: to_string_mach double-quotes the string, don't want that *)
          let err_msg = Error.to_string_hum err in
          if String.is_prefix err_msg ~prefix:refused_answer_query_string then
            Trust_system.(
              record_envelope_sender config.trust_system config.logger sender
                Actions.
                  ( Requested_unknown_item
                  , Some
                      ( "Sync ledger query with hash: $hash, query: $query, \
                         with error: $error"
                      , [ ("hash", Ledger_hash.to_yojson hash)
                        ; ( "query"
                          , Syncable_ledger.Query.to_yojson
                              Ledger.Addr.to_yojson query )
                        ; ("error", `String err_msg) ] ) ))
          else return ()
    in
    return result
  in
  let get_ancestry_rpc conn ~version:_ query =
    Logger.debug config.logger ~module_:__MODULE__ ~location:__LOC__
      "Sending root proof to peer with IP %s" conn.Host_and_port.host ;
    let action_msg = "Get_ancestry query: $query" in
    let msg_args = [("query", Rpcs.Get_ancestry.query_to_yojson query)] in
    let%bind result, sender =
      run_for_rpc_result conn query ~f:get_ancestry action_msg msg_args
    in
    record_unknown_item result sender action_msg msg_args
  in
  let get_bootstrappable_best_tip_rpc conn ~version:_ query =
    Logger.debug config.logger ~module_:__MODULE__ ~location:__LOC__
      "Sending best_tip to peer with IP %s" conn.Host_and_port.host ;
    let action_msg = "Get_bootstrappable_best_ti. query: $query" in
    let msg_args =
      [("query", Rpcs.Get_bootstrappable_best_tip.query_to_yojson query)]
    in
    let%bind result, sender =
      run_for_rpc_result conn query ~f:get_bootstrappable_best_tip action_msg
        msg_args
    in
    record_unknown_item result sender action_msg msg_args
  in
  let get_transition_chain_proof_rpc conn ~version:_ query =
    Logger.info config.logger ~module_:__MODULE__ ~location:__LOC__
      "Sending transition_chain_proof to peer with IP %s"
      conn.Host_and_port.host ;
    let action_msg = "Get_transition_chain_proof query: $query" in
    let msg_args =
      [("query", Rpcs.Get_transition_chain_proof.query_to_yojson query)]
    in
    let%bind result, sender =
      run_for_rpc_result conn query ~f:get_transition_chain_proof action_msg
        msg_args
    in
    record_unknown_item result sender action_msg msg_args
  in
  let get_transition_chain_rpc conn ~version:_ query =
    Logger.info config.logger ~module_:__MODULE__ ~location:__LOC__
      "Sending transition_chain to peer with IP %s" conn.Host_and_port.host ;
    let action_msg = "Get_transition_chain query: $query" in
    let msg_args =
      [("query", Rpcs.Get_transition_chain.query_to_yojson query)]
    in
    let%bind result, sender =
      run_for_rpc_result conn query ~f:get_transition_chain action_msg msg_args
    in
    record_unknown_item result sender action_msg msg_args
  in
  let ban_notify_rpc conn ~version:_ ban_until =
    (* the port in `conn' is an ephemeral port, not of interest *)
    Logger.warn config.logger ~module_:__MODULE__ ~location:__LOC__
      "Node banned by peer $peer until $ban_until"
      ~metadata:
        [ ("peer", `String conn.Host_and_port.host)
        ; ( "ban_until"
          , `String (Time.to_string_abs ~zone:Time.Zone.utc ban_until) ) ] ;
    (* no computation to do; we're just getting notification *)
    Deferred.unit
  in
  let rpc_handlers =
    let open Rpcs in
    [ Rpc_handler
        ( Get_staged_ledger_aux_and_pending_coinbases_at_hash
        , get_staged_ledger_aux_and_pending_coinbases_at_hash_rpc )
    ; Rpc_handler (Answer_sync_ledger_query, answer_sync_ledger_query_rpc)
    ; Rpc_handler (Get_bootstrappable_best_tip, get_bootstrappable_best_tip_rpc)
    ; Rpc_handler (Get_ancestry, get_ancestry_rpc)
    ; Rpc_handler (Get_transition_chain, get_transition_chain_rpc)
    ; Rpc_handler (Get_transition_chain_proof, get_transition_chain_proof_rpc)
    ; Rpc_handler (Ban_notify, ban_notify_rpc) ]
    @ Consensus.Hooks.Rpcs.(
        List.map
          (rpc_handlers ~logger:config.logger
             ~local_state:config.consensus_local_state)
          ~f:(fun (Rpc_handler (rpc, f)) ->
            Rpcs.(Rpc_handler (Consensus_rpc rpc, f)) ))
  in
  let%map gossip_net =
    Gossip_net.Any.create config.creatable_gossip_net rpc_handlers
  in
  don't_wait_for
    (Gossip_net.Any.on_first_connect gossip_net ~f:(fun () ->
         (* After first_connect this list will only be empty if we filtered out all the peers due to mismatched chain id. *)
         let initial_peers = Gossip_net.Any.peers gossip_net in
         if List.is_empty initial_peers then (
           Logger.fatal config.logger "Failed to connect to any initial peers"
             ~module_:__MODULE__ ~location:__LOC__ ;
           raise No_initial_peers ) )) ;
  (* TODO: Think about buffering:
     I.e., what do we do when too many messages are coming in, or going out.
     For example, some things you really want to not drop (like your outgoing
     block announcment).
  *)
  let received_gossips, online_notifier =
    Strict_pipe.Reader.Fork.two
      (Gossip_net.Any.received_message_reader gossip_net)
  in
  let online_status =
    online_broadcaster config.time_controller online_notifier
  in
  let first_received_message_signal = Ivar.create () in
  let states, snark_pool_diffs, transaction_pool_diffs =
    Strict_pipe.Reader.partition_map3 received_gossips ~f:(fun envelope ->
        Ivar.fill_if_empty first_received_message_signal () ;
        match Envelope.Incoming.data envelope with
        | New_state state ->
            Perf_histograms.add_span ~name:"external_transition_latency"
              (Core.Time.abs_diff
                 Block_time.(now config.time_controller |> to_time)
                 ( External_transition.protocol_state state
                 |> Protocol_state.blockchain_state
                 |> Blockchain_state.timestamp |> Block_time.to_time )) ;
            if config.log_gossip_heard.new_state then
              Logger.debug config.logger ~module_:__MODULE__ ~location:__LOC__
                "Received a block $block from $sender"
                ~metadata:
                  [ ("block", External_transition.to_yojson state)
                  ; ( "sender"
                    , Envelope.(Sender.to_yojson (Incoming.sender envelope)) )
                  ] ;
            `Fst
              ( Envelope.Incoming.map envelope ~f:(fun _ -> state)
              , Block_time.now config.time_controller )
        | Snark_pool_diff diff ->
            if config.log_gossip_heard.snark_pool_diff then
              Logger.debug config.logger ~module_:__MODULE__ ~location:__LOC__
                "Received Snark-pool diff $work from $sender"
                ~metadata:
                  [ ("work", Snark_pool.Resource_pool.Diff.compact_json diff)
                  ; ( "sender"
                    , Envelope.(Sender.to_yojson (Incoming.sender envelope)) )
                  ] ;
            Coda_metrics.(
              Counter.inc_one Snark_work.completed_snark_work_received_gossip) ;
            `Snd (Envelope.Incoming.map envelope ~f:(fun _ -> diff))
        | Transaction_pool_diff diff ->
            if config.log_gossip_heard.transaction_pool_diff then
              Logger.debug config.logger ~module_:__MODULE__ ~location:__LOC__
                "Received transaction-pool diff $txns from $sender"
                ~metadata:
                  [ ("txns", Transaction_pool.Resource_pool.Diff.to_yojson diff)
                  ; ( "sender"
                    , Envelope.(Sender.to_yojson (Incoming.sender envelope)) )
                  ] ;
            let diff' =
              List.filter diff ~f:(fun cmd ->
                  if User_command.is_trivial cmd then (
                    Logger.debug config.logger ~module_:__MODULE__
                      ~location:__LOC__
                      "Filtering trivial user command in transaction-pool \
                       diff $cmd from $sender"
                      ~metadata:
                        [ ("cmd", User_command.to_yojson cmd)
                        ; ( "sender"
                          , Envelope.(
                              Sender.to_yojson (Incoming.sender envelope)) ) ] ;
                    false )
                  else true )
            in
            `Trd (Envelope.Incoming.map envelope ~f:(fun _ -> diff')) )
  in
  { gossip_net
  ; logger= config.logger
  ; trust_system= config.trust_system
  ; states
  ; snark_pool_diffs= Strict_pipe.Reader.to_linear_pipe snark_pool_diffs
  ; transaction_pool_diffs=
      Strict_pipe.Reader.to_linear_pipe transaction_pool_diffs
  ; online_status
  ; first_received_message_signal }

(* lift and expose select gossip net functions *)
include struct
  open Gossip_net.Any

  let lift f {gossip_net; _} = f gossip_net

  let peers = lift peers

  let initial_peers = lift initial_peers

  let ban_notification_reader = lift ban_notification_reader

  let random_peers = lift random_peers

  let random_peers_except = lift random_peers_except

  let peers_by_ip = lift peers_by_ip

  (* these cannot be directly lifted due to the value restriction *)
  let query_peer t = lift query_peer t

  let on_first_connect t = lift on_first_connect t

  let on_first_high_connectivity t = lift on_first_high_connectivity t
end

let on_first_received_message {first_received_message_signal; _} ~f =
  Ivar.read first_received_message_signal >>| f

(* TODO: Have better pushback behavior *)
let broadcast t msg =
  Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
    ~metadata:[("message", Gossip_net.Message.msg_to_yojson msg)]
    !"Broadcasting %s over gossip net"
    (Gossip_net.Message.summary msg) ;
  Gossip_net.Any.broadcast t.gossip_net msg

let broadcast_state t state = broadcast t (Gossip_net.Message.New_state state)

let broadcast_transaction_pool_diff t diff =
  broadcast t (Gossip_net.Message.Transaction_pool_diff diff)

let broadcast_snark_pool_diff t diff =
  broadcast t (Gossip_net.Message.Snark_pool_diff diff)

(* TODO: This is kinda inefficient *)
let find_map xs ~f =
  let open Async in
  let ds = List.map xs ~f in
  let filter ~f =
    Deferred.bind ~f:(fun x -> if f x then return x else Deferred.never ())
  in
  let none_worked =
    Deferred.bind (Deferred.all ds) ~f:(fun ds ->
        if List.for_all ds ~f:Option.is_none then return None
        else Deferred.never () )
  in
  Deferred.any (none_worked :: List.map ~f:(filter ~f:Option.is_some) ds)

(* TODO: Don't copy and paste *)
let find_map' xs ~f =
  let open Async in
  let ds = List.map xs ~f in
  let filter ~f =
    Deferred.bind ~f:(fun x -> if f x then return x else Deferred.never ())
  in
  let none_worked =
    Deferred.bind (Deferred.all ds) ~f:(fun ds ->
        (* TODO: Validation applicative here *)
        if List.for_all ds ~f:Or_error.is_error then
          return (Or_error.error_string "all none")
        else Deferred.never () )
  in
  Deferred.any (none_worked :: List.map ~f:(filter ~f:Or_error.is_ok) ds)

let online_status t = t.online_status

let make_rpc_request ~rpc ~label t peer input =
  let open Deferred.Let_syntax in
  match%map query_peer t peer rpc input with
  | Ok (Some response) ->
      Ok response
  | Ok None ->
      Or_error.errorf
        !"Peer %{sexp:Network_peer.Peer.t} doesn't have the requested %s"
        peer label
  | Error e ->
      Error e

let get_transition_chain_proof =
  make_rpc_request ~rpc:Rpcs.Get_transition_chain_proof ~label:"transition"

let get_transition_chain =
  make_rpc_request ~rpc:Rpcs.Get_transition_chain ~label:"chain of transitions"

let get_bootstrappable_best_tip =
  make_rpc_request ~rpc:Rpcs.Get_bootstrappable_best_tip ~label:"best tip"

let ban_notify t peer banned_until =
  query_peer t peer Rpcs.Ban_notify banned_until

let net2 t = Gossip_net.Any.net2 t.gossip_net

let try_non_preferred_peers t input peers ~rpc =
  let max_current_peers = 8 in
  let rec loop peers num_peers =
    if num_peers > max_current_peers then
      return
        (Or_error.error_string
           "None of randomly-chosen peers can handle the request")
    else
      let current_peers, remaining_peers = List.split_n peers num_peers in
      find_map' current_peers ~f:(fun peer ->
          let%bind response_or_error = query_peer t peer rpc input in
          match response_or_error with
          | Ok (Some response) ->
              let%bind () =
                Trust_system.(
                  record t.trust_system t.logger peer.host
                    Actions.
                      ( Fulfilled_request
                      , Some ("Nonpreferred peer returned valid response", [])
                      ))
              in
              return (Ok response)
          | Ok None ->
              loop remaining_peers (2 * num_peers)
          | Error _ ->
              loop remaining_peers (2 * num_peers) )
  in
  loop peers 1

let try_preferred_peer t inet_addr input ~rpc =
  let peers_at_addr = peers_by_ip t inet_addr in
  (* if there's a single peer at inet_addr, call it the preferred peer *)
  match peers_at_addr with
  | [peer] -> (
      let get_random_peers () =
        let max_peers = 15 in
        let except = Peer.Hash_set.of_list [peer] in
        random_peers_except t max_peers ~except
      in
      let%bind response = query_peer t peer rpc input in
      match response with
      | Ok (Some data) ->
          let%bind () =
            Trust_system.(
              record t.trust_system t.logger peer.host
                Actions.
                  ( Fulfilled_request
                  , Some ("Preferred peer returned valid response", []) ))
          in
          return (Ok data)
      | Ok None ->
          let%bind () =
            Trust_system.(
              record t.trust_system t.logger peer.host
                Actions.
                  ( Violated_protocol
                  , Some ("When querying preferred peer, got no response", [])
                  ))
          in
          let peers = get_random_peers () in
          try_non_preferred_peers t input peers ~rpc
      | Error _ ->
          (* TODO: determine what punishments apply here *)
          Logger.error t.logger ~module_:__MODULE__ ~location:__LOC__
            !"get error from %{sexp: Peer.t}"
            peer ;
          let peers = get_random_peers () in
          try_non_preferred_peers t input peers ~rpc )
  | _ ->
      (* no preferred peer *)
      let max_peers = 16 in
      let peers = random_peers t max_peers in
      try_non_preferred_peers t input peers ~rpc

let get_staged_ledger_aux_and_pending_coinbases_at_hash t inet_addr input =
  try_preferred_peer t inet_addr input
    ~rpc:Rpcs.Get_staged_ledger_aux_and_pending_coinbases_at_hash

let get_ancestry t inet_addr input =
  try_preferred_peer t inet_addr input ~rpc:Rpcs.Get_ancestry

let glue_sync_ledger t query_reader response_writer =
  (* We attempt to query 3 random peers, retry_max times. We keep track of the
     peers that couldn't answer a particular query and won't try them
     again. *)
  let retry_max = 6 in
  let retry_interval = Core.Time.Span.of_ms 200. in
  let rec answer_query ctr peers_tried query =
    O1trace.trace_event "ask sync ledger query" ;
    let peers = random_peers_except t 3 ~except:peers_tried in
    Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
      !"SL: Querying the following peers %{sexp: Peer.t list}"
      peers ;
    match%bind
      find_map peers ~f:(fun peer ->
          Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
            !"Asking %{sexp: Peer.t} query regarding ledger_hash %{sexp: \
              Ledger_hash.t}"
            peer (fst query) ;
          match%map query_peer t peer Rpcs.Answer_sync_ledger_query query with
          | Ok (Ok answer) ->
              Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
                !"Received answer from peer %{sexp: Peer.t} on ledger_hash \
                  %{sexp: Ledger_hash.t}"
                peer (fst query) ;
              (* TODO : here is a place where an envelope could contain
                 a Peer.t, and not just an IP address, if desired
              *)
              let inet_addr = peer.host in
              Some
                (Envelope.Incoming.wrap ~data:answer
                   ~sender:(Envelope.Sender.Remote inet_addr))
          | Ok (Error e) ->
              Logger.info t.logger ~module_:__MODULE__ ~location:__LOC__
                "Peer $peer didn't have enough information to answer \
                 ledger_hash query. See error for more details: $error"
                ~metadata:[("error", `String (Error.to_string_hum e))] ;
              Hash_set.add peers_tried peer ;
              None
          | Error err ->
              Logger.warn t.logger ~module_:__MODULE__ ~location:__LOC__
                "Network error: %s" (Error.to_string_mach err) ;
              None )
    with
    | Some answer ->
        Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
          !"Succeeding with answer on ledger_hash %{sexp: Ledger_hash.t}"
          (fst query) ;
        (* TODO *)
        Linear_pipe.write_if_open response_writer (fst query, snd query, answer)
    | None ->
        Logger.info t.logger ~module_:__MODULE__ ~location:__LOC__
          !"None of the peers contacted were able to answer ledger_hash query \
            -- trying more" ;
        if ctr > retry_max then Deferred.unit
        else
          let%bind () = Clock.after retry_interval in
          answer_query (ctr + 1) peers_tried query
  in
  Linear_pipe.iter_unordered ~max_concurrency:8 query_reader
    ~f:(answer_query 0 (Peer.Hash_set.of_list []))
  |> don't_wait_for
