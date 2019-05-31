open Coda_base
open Core

(* Cache represents a graph. The key is a State_hash, which is the node in
   the graph, and the value is the children transitions of the node *)
module type S = sig
  type t

  type external_transition_with_initial_validation

  type state_hash

  val create : unit -> t

  val add :
       t
    -> parent:state_hash
    -> external_transition_with_initial_validation Envelope.Incoming.t
    -> unit

  val data :
    t -> external_transition_with_initial_validation Envelope.Incoming.t list
end

module type Inputs_intf = Transition_frontier.Inputs_intf

module Make (Inputs : Inputs_intf) :
  S
  with type external_transition_with_initial_validation :=
              Inputs.External_transition.with_initial_validation
   and type state_hash := State_hash.t = struct
  type t =
    Inputs.External_transition.with_initial_validation Envelope.Incoming.t list
    State_hash.Table.t

  let create () = State_hash.Table.create ()

  let add (t : t) ~parent new_child =
    State_hash.Table.update t parent ~f:(function
      | None ->
          [new_child]
      | Some children ->
          if
            List.mem children new_child ~equal:(fun e1 e2 ->
                State_hash.equal
                  (Envelope.Incoming.data e1 |> fst |> With_hash.hash)
                  (Envelope.Incoming.data e2 |> fst |> With_hash.hash) )
          then children
          else new_child :: children )

  let data t =
    let collected_transitions = State_hash.Table.data t |> List.concat in
    assert (
      List.length collected_transitions
      = List.length (List.stable_dedup collected_transitions) ) ;
    collected_transitions
end
