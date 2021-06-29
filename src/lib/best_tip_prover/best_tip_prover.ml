open Core_kernel
open Mina_base
open Mina_state
open Async_kernel
open Mina_transition

module type Inputs_intf = sig
  module Transition_frontier : module type of Transition_frontier
end

module Make (Inputs : Inputs_intf) :
  Mina_intf.Best_tip_prover_intf
  with type transition_frontier := Inputs.Transition_frontier.t = struct
  open Inputs

  module Merkle_list_prover = Merkle_list_prover.Make_ident (struct
    type value = External_transition.Validated.t

    type context = Transition_frontier.t

    type proof_elem = State_body_hash.t

    let to_proof_elem external_transition =
      external_transition |> External_transition.Validated.protocol_state
      |> Protocol_state.body |> Protocol_state.Body.hash

    let get_previous ~context transition =
      let parent_hash =
        transition |> External_transition.Validated.protocol_state
        |> Protocol_state.previous_state_hash
      in
      let open Option.Let_syntax in
      let%map breadcrumb = Transition_frontier.find context parent_hash in
      Transition_frontier.Breadcrumb.validated_transition breadcrumb
  end)

  module Merkle_list_verifier = Merkle_list_verifier.Make (struct
    type hash = State_hash.t [@@deriving equal]

    type proof_elem = State_body_hash.t

    let hash acc body_hash =
      Protocol_state.hash_abstract ~hash_body:Fn.id
        {previous_state_hash= acc; body= body_hash}
  end)

  let prove ~logger frontier =
    let open Option.Let_syntax in
    let genesis_constants = Transition_frontier.genesis_constants frontier in
    let root = Transition_frontier.root frontier in
    let root_state_hash = Frontier_base.Breadcrumb.state_hash root in
    let root_is_genesis =
      State_hash.(
        root_state_hash = Transition_frontier.genesis_state_hash frontier)
    in
    let%map () =
      Option.some_if
        ( Transition_frontier.best_tip_path_length_exn frontier
          = Transition_frontier.global_max_length genesis_constants
        || root_is_genesis )
        ()
    in
    let best_tip_breadcrumb = Transition_frontier.best_tip frontier in
    let best_verified_tip =
      Transition_frontier.Breadcrumb.validated_transition best_tip_breadcrumb
    in
    let best_tip =
      External_transition.Validation.forget_validation_with_hash
        best_verified_tip
    in
    let root =
      Transition_frontier.root frontier
      |> Transition_frontier.Breadcrumb.validated_transition
    in
    let _, merkle_list =
      Merkle_list_prover.prove ~context:frontier best_verified_tip
    in
    [%log debug]
      ~metadata:
        [ ( "merkle_list"
          , `List (List.map ~f:State_body_hash.to_yojson merkle_list) ) ]
      "Best tip prover produced a merkle list of $merkle_list" ;
    Proof_carrying_data.
      { data= best_tip
      ; proof=
          ( merkle_list
          , root |> External_transition.Validation.forget_validation ) }

  let validate_proof ~verifier transition_with_hash =
    let open Deferred.Result.Monad_infix in
    External_transition.(
      Validation.wrap transition_with_hash
      |> skip_time_received_validation
           `This_transition_was_not_received_via_gossip
      |> skip_genesis_protocol_state_validation
           `This_transition_was_generated_internally
      |> skip_protocol_versions_validation
           `This_transition_has_valid_protocol_versions
      |> (fun x -> validate_proofs ~verifier [x] >>| List.hd_exn)
      >>= Fn.compose Deferred.Result.return
            (skip_delta_transition_chain_validation
               `This_transition_was_not_received_via_gossip)
      |> Deferred.map
           ~f:
             (Result.map_error ~f:(Fn.const (Error.of_string "invalid proof"))))

  let verify ~verifier ~genesis_constants ~precomputed_values
      {Proof_carrying_data.data= best_tip; proof= merkle_list, root} =
    let open Deferred.Or_error.Let_syntax in
    let merkle_list_length = List.length merkle_list in
    let max_length = Transition_frontier.global_max_length genesis_constants in
    let genesis_protocol_state =
      Precomputed_values.genesis_state_with_hash precomputed_values
    in
    let genesis_state_hash = With_hash.hash genesis_protocol_state in
    let root_state_hash = External_transition.state_hash root in
    let root_is_genesis = State_hash.(root_state_hash = genesis_state_hash) in
    let%bind () =
      Deferred.return
        (Result.ok_if_true
           ~error:
             ( Error.of_string
             @@ sprintf
                  !"Peer should have given a proof of length %d but got %d"
                  max_length merkle_list_length )
           (Int.equal max_length merkle_list_length || root_is_genesis))
    in
    let best_tip_with_hash =
      With_hash.of_data best_tip ~hash_data:External_transition.state_hash
    in
    let root_transition_with_hash =
      With_hash.of_data root ~hash_data:External_transition.state_hash
    in
    let%bind (_ : State_hash.t Non_empty_list.t) =
      Deferred.return
        (Result.of_option
           (Merkle_list_verifier.verify ~init:root_transition_with_hash.hash
              merkle_list best_tip_with_hash.hash)
           ~error:
             (Error.of_string
                "Peer should have given a valid merkle list proof for their \
                 best tip"))
    in
    let%map root, best_tip =
      Deferred.Or_error.both
        (validate_proof ~verifier root_transition_with_hash)
        (validate_proof ~verifier best_tip_with_hash)
    in
    (`Root root, `Best_tip best_tip)
end

include Make (struct
  module Transition_frontier = Transition_frontier
end)
