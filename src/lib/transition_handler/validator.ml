open Async_kernel
open Core_kernel
open Pipe_lib.Strict_pipe
open Mina_base
open Mina_state
open Cache_lib
open Mina_transition
open Network_peer

let validate_transition ~consensus_constants ~logger ~frontier
    ~unprocessed_transition_cache
    (enveloped_transition :
      External_transition.Initial_validated.t Envelope.Incoming.t) =
  let open Result.Let_syntax in
  let transition =
    Envelope.Incoming.data enveloped_transition
    |> External_transition.Validation.forget_validation_with_hash
  in
  let transition_hash = With_hash.hash transition in
  let root_breadcrumb = Transition_frontier.root frontier in
  let%bind () =
    Option.fold
      (Transition_frontier.find frontier transition_hash)
      ~init:Result.(Ok ())
      ~f:(fun _ _ -> Result.Error (`In_frontier transition_hash))
  in
  let%bind () =
    Option.fold
      (Unprocessed_transition_cache.final_state unprocessed_transition_cache
         enveloped_transition)
      ~init:Result.(Ok ())
      ~f:(fun _ final_state -> Result.Error (`In_process final_state))
  in
  let%map () =
    Result.ok_if_true
      (Consensus.Hooks.equal_select_status `Take
         (Consensus.Hooks.select ~constants:consensus_constants
            ~logger:
              (Logger.extend logger
                 [("selection_context", `String "Transition_handler.Validator")])
            ~existing:
              (Transition_frontier.Breadcrumb.consensus_state_with_hash
                 root_breadcrumb)
            ~candidate:
              (With_hash.map ~f:External_transition.consensus_state transition)))
      ~error:`Disconnected
  in
  (* we expect this to be Ok since we just checked the cache *)
  Unprocessed_transition_cache.register_exn unprocessed_transition_cache
    enveloped_transition

let run ~logger ~consensus_constants ~trust_system ~time_controller ~frontier
    ~transition_reader
    ~(valid_transition_writer :
       ( ( External_transition.Initial_validated.t Envelope.Incoming.t
         , State_hash.t )
         Cached.t
       , crash buffered
       , unit )
       Writer.t) ~unprocessed_transition_cache =
  let module Lru = Core_extended_cache.Lru in
  don't_wait_for
    (Reader.iter transition_reader ~f:(fun transition_env ->
         let {With_hash.hash= transition_hash; data= transition}, _ =
           Envelope.Incoming.data transition_env
         in
         let sender = Envelope.Incoming.sender transition_env in
         match
           validate_transition ~consensus_constants ~logger ~frontier
             ~unprocessed_transition_cache transition_env
         with
         | Ok cached_transition ->
             let%map () =
               Trust_system.record_envelope_sender trust_system logger sender
                 ( Trust_system.Actions.Sent_useful_gossip
                 , Some
                     ( "external transition $state_hash"
                     , [ ("state_hash", State_hash.to_yojson transition_hash)
                       ; ( "transition"
                         , External_transition.to_yojson transition ) ] ) )
             in
             let transition_time =
               External_transition.protocol_state transition
               |> Protocol_state.blockchain_state |> Blockchain_state.timestamp
               |> Block_time.to_time
             in
             Perf_histograms.add_span
               ~name:"accepted_transition_remote_latency"
               (Core_kernel.Time.diff
                  Block_time.(now time_controller |> to_time)
                  transition_time) ;
             Writer.write valid_transition_writer cached_transition
         | Error (`In_frontier _) | Error (`In_process _) ->
             Trust_system.record_envelope_sender trust_system logger sender
               ( Trust_system.Actions.Sent_old_gossip
               , Some
                   ( "external transition with state hash $state_hash"
                   , [ ("state_hash", State_hash.to_yojson transition_hash)
                     ; ("transition", External_transition.to_yojson transition)
                     ] ) )
         | Error `Disconnected ->
             Mina_metrics.(Counter.inc_one Rejected_blocks.worse_than_root) ;
             [%log error]
               ~metadata:
                 [ ("state_hash", State_hash.to_yojson transition_hash)
                 ; ("reason", `String "not selected over current root")
                 ; ( "protocol_state"
                   , External_transition.protocol_state transition
                     |> Protocol_state.value_to_yojson ) ]
               "Validation error: external transition with state hash \
                $state_hash was rejected for reason $reason" ;
             Trust_system.record_envelope_sender trust_system logger sender
               ( Trust_system.Actions.Disconnected_chain
               , Some
                   ( "received transition that was not connected to our chain \
                      from $sender"
                   , [ ( "sender"
                       , Envelope.Sender.to_yojson
                           (Envelope.Incoming.sender transition_env) )
                     ; ("transition", External_transition.to_yojson transition)
                     ] ) ) ))
