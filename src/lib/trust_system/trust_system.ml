(** The trust system, instantiated with Coda-specific stuff. *)
open Core

open Async

module Actions = struct
  type action =
    | Incoming_connection_error
        (** Connection error while peer connected to node. *)
    | Outgoing_connection_error
        (** Encountered connection error while connecting to a peer. *)
    | Gossiped_old_transition of int64 * int
        (** Peer gossiped a transition which was too old. Includes time before cutoff period in which the transition was received, expressed in slots and delta. *)
    | Gossiped_future_transition
        (** Peer gossiped a transition before its slot. *)
    | Gossiped_invalid_transition
        (** Peer gossiped an invalid transition to us. *)
    | Disconnected_chain
        (** Peer has been determined to be on a chain that is not connected to our chain. *)
    | Sent_bad_hash
        (** Peer sent us some data that doesn't hash to the expected value *)
    | Sent_invalid_signature
        (** Peer sent us something with a signature that doesn't check *)
    | Sent_invalid_transaction  (** Peer sent us an invalid transaction *)
    | Sent_invalid_proof  (** Peer sent us a proof that does not verify. *)
    | Sent_invalid_signature_or_proof
        (** Peer either sent us a proof or a signature that does not verify. *)
    | Sent_invalid_protocol_version
        (** Peer sent block with invalid protocol version *)
    | Sent_mismatched_protocol_version
        (** Peer sent block with protocol version not matching daemon protocol version *)
    | Has_invalid_genesis_protocol_state
        (**Peer gossiped a transition that has a different genesis protocol state from that of mine*)
    | Sent_invalid_transition_chain_merkle_proof
        (** Peer sent us a transition chain witness that does not verify *)
    | Violated_protocol  (** Peer violated the specification of the protocol. *)
    | Made_request
        (** Peer made a valid request. This causes a small decrease to mitigate
            DoS. *)
    | Connected
        (** Peer connected to TCP server. Very small decrease to mitigate DoS *)
    | Requested_unknown_item
        (** Peer requested something we don't know. They might be ahead of us or
          they might be malicious. *)
    | Fulfilled_request  (** Peer fulfilled a request we made. *)
    | Epoch_ledger_provided  (** Special case of request fulfillment *)
    | Sent_useful_gossip
        (** Peer sent us a gossip item that we added to our pool*)
    | Sent_useless_gossip
        (** Peer sent us a gossip item that we rejected from our pool for reasons
          that may be innocent. e.g. too low of a fee for a user command, out of
          date, etc.
      *)
    | Sent_old_gossip  (** Peer sent us a gossip item we already knew. *)
    | No_reply_from_preferred_peer
        (** A peer that should have had the response to a query did not provide it. *)
    | Unknown_rpc  (** A peer made an unknown RPC. *)
    | Decoding_failed  (** A gossip message could not be decoded. *)
  [@@deriving show]

  (** The action they took, paired with a message and associated JSON metadata
      for logging. *)
  type t = action * (string * (string, Yojson.Safe.t) List.Assoc.t) option

  let to_trust_response (action, _) =
    let open Peer_trust.Trust_response in
    (* FIXME figure out a good value for this *)
    let fulfilled_increment = Peer_trust.max_rate 10. in
    (* the summed decreases of a connection and request equals
       the increase of a fulfilled request *)
    let request_increment = 0.90 *. fulfilled_increment in
    let connected_increment = 0.10 *. fulfilled_increment in
    let epoch_ledger_provided_increment = 10. *. fulfilled_increment in
    let old_gossip_increment = Peer_trust.max_rate 20. in
    match action with
    | Gossiped_old_transition (slot_diff, delta) ->
        (* NOTE: slot_diff here is [received_slot - (produced_slot + Δ)]
         *
         * We want to decrease the score exponentially based on how out of date the transition
         * we received was. We would like the base score decrease to be some constant
         * [c], and we would like to instantly ban any peers who send us transitions
         * received more than [Δ] slots out of date. Therefore, we want some function
         * [f] where [f(1) = c] and [f(Δ) >= 2]. We start by fitting an geometric function
         * such that [f(Δ) = 2]. [(1/y)x^2] should be [2] when [x] is [Δ], so if we solve for
         * [(1/y)Δ^2 = 2], we get [y = Δ^2/2]. Therefore, we can define our function
         * [f(x) = (1/(Δ^2/2))x^2]. This does not satisfy [f(1) = c], but since we only constrain
         * [f(Δ) >= 2], we can just offset the function by [c] in order to satisfy both constraints,
         * giving us [f(x) = (1/(Δ^2/2))x^2 + c].
         *)
        let c = 0.1 in
        (* this delta has not been updated for delta=0. Leaving since this system is off and likely to be replaced*)
        let y = (Float.of_int delta ** 2.0) /. 2.0 in
        let f x = (1.0 /. y *. (x ** 2.0)) +. c in
        Trust_decrease (f (Int64.to_float slot_diff))
    | Gossiped_future_transition ->
        Insta_ban
    | Gossiped_invalid_transition ->
        Insta_ban
    | Disconnected_chain ->
        Insta_ban
    | Sent_bad_hash ->
        Insta_ban
    | Sent_invalid_signature ->
        Insta_ban
    | Sent_invalid_transaction ->
        Insta_ban
    | Sent_invalid_proof ->
        Insta_ban
    | Sent_invalid_signature_or_proof ->
        Insta_ban
    | Sent_invalid_protocol_version ->
        Insta_ban
    (* allow nodes to send wrong current protocol version a small number of times *)
    | Sent_mismatched_protocol_version ->
        Trust_decrease 0.25
    (*Genesis ledger (and the genesis protocol state) is now a runtime config, so we should ban nodes that are running using a different genesis ledger*)
    | Has_invalid_genesis_protocol_state ->
        Insta_ban
    | Sent_invalid_transition_chain_merkle_proof ->
        Insta_ban
    (* incoming and outgoing connection errors can happen due to network
       failures, killing the client, or ungraceful shutdown, so we need to be
       pretty lenient. *)
    | Incoming_connection_error ->
        Trust_decrease 0.05
    | Outgoing_connection_error ->
        Trust_decrease 0.05
    | No_reply_from_preferred_peer ->
        Trust_decrease 0.05
    | Violated_protocol ->
        Insta_ban
    | Made_request ->
        Trust_decrease request_increment
    | Connected ->
        Trust_decrease connected_increment
    | Requested_unknown_item ->
        Trust_decrease (Peer_trust.max_rate 1.)
    | Fulfilled_request ->
        Trust_increase fulfilled_increment
    | Epoch_ledger_provided ->
        Trust_increase epoch_ledger_provided_increment
    (* Processing old gossip is fast, a single lookup in our table, while
       processing useless gossip is more expensive since we have to do
       validation on it. In expectation, we get every gossipped message
       'replication factor' times, which is 8. That ratio applies to individual
       peers too, so we give 7x credit for useful gossip than we take away for
       old gossip, plus some headroom for normal variance. *)
    | Sent_useful_gossip ->
        Trust_increase (old_gossip_increment *. 10.)
    | Sent_useless_gossip ->
        Trust_decrease (old_gossip_increment *. 3.)
    | Sent_old_gossip ->
        Trust_decrease old_gossip_increment
    | Unknown_rpc ->
        (* TODO: Should do a soft ban of some kind if this happens enough. *)
        Trust_decrease 0.05
    | Decoding_failed ->
        Insta_ban

  (* Disabling everything except insta-ban *)
  let to_trust_response t =
    match to_trust_response t with
    | Insta_ban ->
        Peer_trust.Trust_response.Insta_ban
    | _ ->
        Trust_decrease 0.

  let to_log : t -> string * (string, Yojson.Safe.t) List.Assoc.t =
   fun (action, extra_opt) ->
    match extra_opt with
    | None ->
        (show_action action, [])
    | Some (fmt, metadata) ->
        (sprintf !"%s (%s)" (show_action action) fmt, metadata)
end

module Banned_status = Banned_status
module Peer_status = Peer_status
module Peer_trust = Peer_trust.Make (Actions)
include Peer_trust

let record_envelope_sender :
       t
    -> Logger.t
    -> Network_peer.Envelope.Sender.t
    -> Actions.t
    -> unit Deferred.t =
 fun t logger sender action ->
  match sender with
  | Local ->
      let action_fmt, action_metadata = Actions.to_log action in
      [%log debug]
        ~metadata:(("action", `String action_fmt) :: action_metadata)
        "Attempted to record trust action of ourselves: $action" ;
      Deferred.unit
  | Remote peer ->
      record t logger peer action
