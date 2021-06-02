open Async
open Core
open Otp_lib
open Mina_base
open Frontier_base

type input = Diff.Lite.E.t list

type create_args =
  { db: Database.t
  ; logger: Logger.t
  ; persistent_root_instance: Persistent_root.Instance.t }

module Worker = struct
  (* when this is transitioned to an RPC worker, the db argument
   * should just be a directory, but while this is still in process,
   * the full instance is needed to share with other threads
   *)
  type nonrec input = input

  type t =
    { db: Database.t
    ; logger: Logger.t
    ; persistent_root_instance: Persistent_root.Instance.t }

  type nonrec create_args = create_args

  type output = unit

  (* worker assumes database has already been checked and initialized *)
  let create ({db; logger; persistent_root_instance} : create_args) : t =
    {db; logger; persistent_root_instance}

  (* nothing to close *)
  let close _ = Deferred.unit

  type apply_diff_error =
    [`Apply_diff of [`New_node | `Root_transitioned | `Best_tip_changed]]

  type apply_diff_error_internal =
    [ `Not_found of
      [ `New_root_transition
      | `Old_root_transition
      | `Parent_transition of State_hash.t
      | `Arcs of State_hash.t
      | `Best_tip ] ]

  let apply_diff_error_internal_to_string = function
    | `Not_found `New_root_transition ->
        "new root transition not found"
    | `Not_found `Old_root_transition ->
        "old root transition not found"
    | `Not_found (`Parent_transition hash) ->
        Printf.sprintf "parent transition %s not found"
          (State_hash.to_string hash)
    | `Not_found `Best_tip ->
        "best tip not found"
    | `Not_found (`Arcs hash) ->
        Printf.sprintf "arcs not found for %s" (State_hash.to_string hash)

  let apply_diff (type mutant) (t : t) (diff : mutant Diff.Lite.t) :
      (mutant, apply_diff_error) Result.t =
    let map_error result ~diff_type ~diff_type_name =
      Result.map_error result ~f:(fun err ->
          [%log' error t.logger] "error applying %s diff: %s" diff_type_name
            (apply_diff_error_internal_to_string err) ;
          `Apply_diff diff_type )
    in
    match diff with
    | New_node (Lite transition) -> (
        let r =
          ( Database.add t.db ~transition
            :> (mutant, apply_diff_error_internal) Result.t )
        in
        match r with
        | Ok x ->
            Ok x
        | Error (`Not_found (`Parent_transition h | `Arcs h)) ->
            [%log' trace t.logger]
              "Did not add node $hash to DB. Its $parent has already been \
               thrown away"
              ~metadata:
                [ ( "hash"
                  , `String
                      (State_hash.to_base58_check
                         (Mina_transition.External_transition.Validated
                          .state_hash transition)) )
                ; ("parent", `String (State_hash.to_base58_check h)) ] ;
            Ok ()
        | _ ->
            map_error ~diff_type:`New_node ~diff_type_name:"New_node" r )
    | Root_transitioned {new_root; garbage= Lite garbage; just_emitted_a_proof}
      ->
        if just_emitted_a_proof then
          Persistent_root.Instance.dequeue_snarked_ledger
            t.persistent_root_instance ;
        map_error ~diff_type:`Root_transitioned
          ~diff_type_name:"Root_transitioned"
          ( Database.move_root t.db ~new_root ~garbage
            :> (mutant, apply_diff_error_internal) Result.t )
    | Best_tip_changed best_tip_hash ->
        map_error ~diff_type:`Best_tip_changed
          ~diff_type_name:"Best_tip_changed"
          ( Database.set_best_tip t.db best_tip_hash
            :> (mutant, apply_diff_error_internal) Result.t )

  let handle_diff t (Diff.Lite.E.E diff) =
    let open Result.Let_syntax in
    let%map _mutant = apply_diff t diff in
    ()

  (* result equivalent of Deferred.Or_error.List.fold *)
  let rec deferred_result_list_fold ls ~init ~f =
    let open Deferred.Result.Let_syntax in
    match ls with
    | [] ->
        return init
    | h :: t ->
        let%bind init = f init h in
        deferred_result_list_fold t ~init ~f

  let perform t input =
    match%map
      [%log' trace t.logger] "Applying %d diffs to the persistent frontier"
        (List.length input) ;
      (* Iterating over the diff application in this way
         * effectively allows the scheduler to scheduler
         * other tasks in between diff applications.
         * If implemented otherwise, all diffs would be
         * applied during the same scheduler cycle.
         *)
      deferred_result_list_fold input ~init:() ~f:(fun () diff ->
          Deferred.return (handle_diff t diff) )
    with
    | Ok () ->
        ()
    (* TODO: log the diff that failed *)
    | Error (`Apply_diff _) ->
        failwith "Failed to apply a diff to the persistent transition frontier"
end

include Worker_supervisor.Make (Worker)
