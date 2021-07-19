open Async_kernel
open Core
open Mina_base
open Mina_transition
open Frontier_base

(* TODO: bundle together with other writes by sharing batch requests between
 * function calls in this module (#3738) *)

let rec deferred_list_result_iter ls ~f =
  let open Deferred.Result.Let_syntax in
  match ls with
  | [] ->
      return ()
  | h :: t ->
      let%bind () = f h in
      deferred_list_result_iter t ~f

(* TODO: should debug assert garbage checks be added? *)
open Result.Let_syntax

(* TODO: implement versions with module versioning. For
 * now, this is just stubbed so we can add db migrations
 * later. (#3736) *)
let version = 1

module Schema = struct
  module Keys = struct
    module String = String

    module Prefixed_state_hash = struct
      [%%versioned
      module Stable = struct
        [@@@no_toplevel_latest_type]
        module V1 = struct
          type t = string * State_hash.Stable.V1.t

          let to_latest = Fn.id
        end
      end]
    end
  end

  type _ t =
    | Db_version : int t
    | Transition : State_hash.t -> External_transition.t t
    | Arcs : State_hash.t -> State_hash.t list t
    | Root : Root_data.Minimal.t t
    | Best_tip : State_hash.t t
    | Protocol_states_for_root_scan_state
        : Mina_state.Protocol_state.value list t

  let to_string : type a. a t -> string = function
    | Db_version ->
        "Db_version"
    | Transition _ ->
        "Transition _"
    | Arcs _ ->
        "Arcs _"
    | Root ->
        "Root"
    | Best_tip ->
        "Best_tip"
    | Protocol_states_for_root_scan_state ->
        "Protocol_states_for_root_scan_state"

  let binable_data_type (type a) : a t -> a Bin_prot.Type_class.t = function
    | Db_version ->
        [%bin_type_class: int]
    | Transition _ ->
        [%bin_type_class: External_transition.Stable.Latest.t]
    | Arcs _ ->
        [%bin_type_class: State_hash.Stable.Latest.t list]
    | Root ->
        [%bin_type_class: Root_data.Minimal.Stable.Latest.t]
    | Best_tip ->
        [%bin_type_class: State_hash.Stable.Latest.t]
    | Protocol_states_for_root_scan_state ->
        [%bin_type_class: Mina_state.Protocol_state.Value.Stable.Latest.t list]

  (* HACK: a simple way to derive Bin_prot.Type_class.t for each case of a GADT *)
  let gadt_input_type_class (type data a) :
         (module Binable.S with type t = data)
      -> to_gadt:(data -> a t)
      -> of_gadt:(a t -> data)
      -> a t Bin_prot.Type_class.t =
   fun (module M) ~to_gadt ~of_gadt ->
    let ({shape; writer= {size; write}; reader= {read; vtag_read}}
          : data Bin_prot.Type_class.t) =
      [%bin_type_class: M.t]
    in
    { shape
    ; writer=
        { size= Fn.compose size of_gadt
        ; write= (fun buffer ~pos gadt -> write buffer ~pos (of_gadt gadt)) }
    ; reader=
        { read= (fun buffer ~pos_ref -> to_gadt (read buffer ~pos_ref))
        ; vtag_read=
            (fun buffer ~pos_ref number ->
              to_gadt (vtag_read buffer ~pos_ref number) ) } }

  (* HACK: The OCaml compiler thought the pattern matching in of_gadts was
   non-exhaustive. However, it should not be since I constrained the
   polymorphic type *)
  let[@warning "-8"] binable_key_type (type a) :
      a t -> a t Bin_prot.Type_class.t = function
    | Db_version ->
        gadt_input_type_class
          (module Keys.String)
          ~to_gadt:(fun _ -> Db_version)
          ~of_gadt:(fun Db_version -> "db_version")
    | Transition _ ->
        gadt_input_type_class
          (module Keys.Prefixed_state_hash.Stable.Latest)
          ~to_gadt:(fun (_, hash) -> Transition hash)
          ~of_gadt:(fun (Transition hash) -> ("transition", hash))
    | Arcs _ ->
        gadt_input_type_class
          (module Keys.Prefixed_state_hash.Stable.Latest)
          ~to_gadt:(fun (_, hash) -> Arcs hash)
          ~of_gadt:(fun (Arcs hash) -> ("arcs", hash))
    | Root ->
        gadt_input_type_class
          (module Keys.String)
          ~to_gadt:(fun _ -> Root)
          ~of_gadt:(fun Root -> "root")
    | Best_tip ->
        gadt_input_type_class
          (module Keys.String)
          ~to_gadt:(fun _ -> Best_tip)
          ~of_gadt:(fun Best_tip -> "best_tip")
    | Protocol_states_for_root_scan_state ->
        gadt_input_type_class
          (module Keys.String)
          ~to_gadt:(fun _ -> Protocol_states_for_root_scan_state)
          ~of_gadt:(fun Protocol_states_for_root_scan_state ->
            "protocol_states_in_root_scan_state" )
end

module Error = struct
  type not_found_member =
    [ `Root
    | `Best_tip
    | `Frontier_hash
    | `Root_transition
    | `Best_tip_transition
    | `Parent_transition of State_hash.t
    | `New_root_transition
    | `Old_root_transition
    | `Transition of State_hash.t
    | `Arcs of State_hash.t
    | `Protocol_states_for_root_scan_state ]

  type not_found = [`Not_found of not_found_member]

  type raised = [`Raised of Error.t]

  type t = [not_found | raised | `Invalid_version]

  let not_found_message (`Not_found member) =
    let member_name, member_id =
      match member with
      | `Root ->
          ("root", None)
      | `Best_tip ->
          ("best tip", None)
      | `Frontier_hash ->
          ("frontier hash", None)
      | `Root_transition ->
          ("root transition", None)
      | `Best_tip_transition ->
          ("best tip transition", None)
      | `Parent_transition hash ->
          ("parent transition", Some hash)
      | `New_root_transition ->
          ("new root transition", None)
      | `Old_root_transition ->
          ("old root transition", None)
      | `Transition hash ->
          ("transition", Some hash)
      | `Arcs hash ->
          ("arcs", Some hash)
      | `Protocol_states_for_root_scan_state ->
          ("protocol states in root scan state", None)
    in
    let additional_context =
      Option.map member_id ~f:(fun id ->
          Printf.sprintf " (hash = %s)" (State_hash.raw_hash_bytes id) )
      |> Option.value ~default:""
    in
    Printf.sprintf "%s not found%s" member_name additional_context

  let message = function
    | `Invalid_version ->
        "invalid version"
    | `Not_found _ as err ->
        not_found_message err
    | `Raised err ->
        sprintf "Raised %s" (Error.to_string_hum err)
end

module Rocks = Rocksdb.Serializable.GADT.Make (Schema)

type t = {directory: string; logger: Logger.t; db: Rocks.t}

let create ~logger ~directory =
  if not (Result.is_ok (Unix.access directory [`Exists])) then
    Unix.mkdir ~perm:0o766 directory ;
  {directory; logger; db= Rocks.create directory}

let close t = Rocks.close t.db

open Schema
open Rocks

let mem db ~key = Option.is_some (get db ~key)

let get_if_exists db ~default ~key =
  match get db ~key with Some x -> x | None -> default

let get db ~key ~error =
  match get db ~key with Some x -> Ok x | None -> Error error

(* TODO: check that best tip is connected to root *)
(* TODO: check for garbage *)
let check t ~genesis_state_hash =
  Or_error.try_with (fun () ->
      let check_version () =
        match get_if_exists t.db ~key:Db_version ~default:0 with
        | 0 ->
            Error `Not_initialized
        | v when v = version ->
            Ok ()
        | _ ->
            Error `Invalid_version
      in
      (* checks the pointers, frontier hash, and checks pointer references *)
      let check_base () =
        let%bind root =
          get t.db ~key:Root ~error:(`Corrupt (`Not_found `Root))
        in
        let root_hash = Root_data.Minimal.hash root in
        let%bind best_tip =
          get t.db ~key:Best_tip ~error:(`Corrupt (`Not_found `Best_tip))
        in
        let%bind root_transition =
          get t.db ~key:(Transition root_hash)
            ~error:(`Corrupt (`Not_found `Root_transition))
        in
        let%bind _ =
          get t.db ~key:Protocol_states_for_root_scan_state
            ~error:(`Corrupt (`Not_found `Protocol_states_for_root_scan_state))
        in
        let%map _ =
          get t.db ~key:(Transition best_tip)
            ~error:(`Corrupt (`Not_found `Best_tip_transition))
        in
        (root_hash, root_transition)
      in
      let rec check_arcs pred_hash =
        let%bind successors =
          get t.db ~key:(Arcs pred_hash)
            ~error:(`Corrupt (`Not_found (`Arcs pred_hash)))
        in
        List.fold successors ~init:(Ok ()) ~f:(fun acc succ_hash ->
            let%bind () = acc in
            let%bind _ =
              get t.db ~key:(Transition succ_hash)
                ~error:(`Corrupt (`Not_found (`Transition succ_hash)))
            in
            check_arcs succ_hash )
      in
      let%bind () = check_version () in
      let%bind root_hash, root_transition = check_base () in
      let%bind () =
        let persisted_genesis_state_hash =
          External_transition.protocol_state root_transition
          |> Mina_state.Protocol_state.genesis_state_hash
        in
        if State_hash.equal persisted_genesis_state_hash genesis_state_hash
        then Ok ()
        else Error (`Genesis_state_mismatch persisted_genesis_state_hash)
      in
      let%map () = check_arcs root_hash in
      External_transition.blockchain_state root_transition
      |> Mina_state.Blockchain_state.snarked_ledger_hash )
  |> Result.map_error ~f:(fun err -> `Corrupt (`Raised err))
  |> Result.join

let initialize t ~root_data =
  let open Root_data.Limited in
  let {With_hash.hash= root_state_hash; data= root_transition}, _ =
    External_transition.Validated.erase (transition root_data)
  in
  [%log' trace t.logger]
    ~metadata:[("root_data", Root_data.Limited.to_yojson root_data)]
    "Initializing persistent frontier database with $root_data" ;
  Batch.with_batch t.db ~f:(fun batch ->
      Batch.set batch ~key:Db_version ~data:version ;
      Batch.set batch ~key:(Transition root_state_hash) ~data:root_transition ;
      Batch.set batch ~key:(Arcs root_state_hash) ~data:[] ;
      Batch.set batch ~key:Root ~data:(Root_data.Minimal.of_limited root_data) ;
      Batch.set batch ~key:Best_tip ~data:root_state_hash ;
      Batch.set batch ~key:Protocol_states_for_root_scan_state
        ~data:(List.unzip (protocol_states root_data) |> snd) )

let add t ~transition =
  let parent_hash = External_transition.Validated.parent_hash transition in
  let {With_hash.hash; data= raw_transition}, _ =
    External_transition.Validated.erase transition
  in
  let%bind () =
    Result.ok_if_true
      (mem t.db ~key:(Transition parent_hash))
      ~error:(`Not_found (`Parent_transition parent_hash))
  in
  let%map parent_arcs =
    get t.db ~key:(Arcs parent_hash) ~error:(`Not_found (`Arcs parent_hash))
  in
  Batch.with_batch t.db ~f:(fun batch ->
      Batch.set batch ~key:(Transition hash) ~data:raw_transition ;
      Batch.set batch ~key:(Arcs hash) ~data:[] ;
      Batch.set batch ~key:(Arcs parent_hash) ~data:(hash :: parent_arcs) )

let move_root t ~new_root ~garbage =
  let open Root_data.Limited in
  let%bind () =
    Result.ok_if_true
      (mem t.db ~key:(Transition (hash new_root)))
      ~error:(`Not_found `New_root_transition)
  in
  let%map old_root =
    get t.db ~key:Root ~error:(`Not_found `Old_root_transition)
  in
  let old_root_hash = Root_data.Minimal.hash old_root in
  (* TODO: Result compatible rocksdb batch transaction *)
  Batch.with_batch t.db ~f:(fun batch ->
      Batch.set batch ~key:Root ~data:(Root_data.Minimal.of_limited new_root) ;
      Batch.set batch ~key:Protocol_states_for_root_scan_state
        ~data:(List.map ~f:snd (protocol_states new_root)) ;
      List.iter (old_root_hash :: garbage) ~f:(fun node_hash ->
          (* because we are removing entire forks of the tree, there is
           * no need to have extra logic to any remove arcs to the node
           * we are deleting since there we are deleting all of a node's
           * parents as well
           *)
          Batch.remove batch ~key:(Transition node_hash) ;
          Batch.remove batch ~key:(Arcs node_hash) ) ) ;
  old_root_hash

let get_transition t hash =
  let%map transition =
    get t.db ~key:(Transition hash) ~error:(`Not_found (`Transition hash))
  in
  (* this transition was read from the database, so it must have been validated already *)
  let (`I_swear_this_is_safe_see_my_comment validated_transition) =
    External_transition.Validated.create_unsafe transition
  in
  validated_transition

let get_arcs t hash =
  get t.db ~key:(Arcs hash) ~error:(`Not_found (`Arcs hash))

let get_root t = get t.db ~key:Root ~error:(`Not_found `Root)

let get_protocol_states_for_root_scan_state t =
  get t.db ~key:Protocol_states_for_root_scan_state
    ~error:(`Not_found `Protocol_states_for_root_scan_state)

let get_root_hash t =
  let%map root = get_root t in
  Root_data.Minimal.hash root

let get_best_tip t = get t.db ~key:Best_tip ~error:(`Not_found `Best_tip)

let set_best_tip t hash =
  let%map old_best_tip_hash = get_best_tip t in
  (* no need to batch because we only do one operation *)
  set t.db ~key:Best_tip ~data:hash ;
  old_best_tip_hash

let rec crawl_successors t hash ~init ~f =
  let open Deferred.Result.Let_syntax in
  let%bind successors = Deferred.return (get_arcs t hash) in
  deferred_list_result_iter successors ~f:(fun succ_hash ->
      let%bind transition = Deferred.return (get_transition t succ_hash) in
      let%bind init' =
        Deferred.map (f init transition)
          ~f:(Result.map_error ~f:(fun err -> `Crawl_error err))
      in
      crawl_successors t succ_hash ~init:init' ~f )
