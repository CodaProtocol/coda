open Async_kernel
open Core
open Coda_base
open Coda_transition
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
    | Frontier_hash : Frontier_hash.t t

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
    | Frontier_hash ->
        "Frontier_hash"

  let binable_data_type (type a) : a t -> a Bin_prot.Type_class.t = function
    | Db_version ->
        [%bin_type_class: int]
    | Transition _ ->
        [%bin_type_class: External_transition.Stable.V1.t]
    | Arcs _ ->
        [%bin_type_class: State_hash.Stable.V1.t list]
    | Root ->
        [%bin_type_class: Root_data.Minimal.Stable.V1.t]
    | Best_tip ->
        [%bin_type_class: State_hash.Stable.V1.t]
    | Frontier_hash ->
        [%bin_type_class: Frontier_hash.Stable.V1.t]

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
          (module Keys.Prefixed_state_hash.Stable.V1)
          ~to_gadt:(fun (_, hash) -> Transition hash)
          ~of_gadt:(fun (Transition hash) -> ("transition", hash))
    | Arcs _ ->
        gadt_input_type_class
          (module Keys.Prefixed_state_hash.Stable.V1)
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
    | Frontier_hash ->
        gadt_input_type_class
          (module Keys.String)
          ~to_gadt:(fun _ -> Frontier_hash)
          ~of_gadt:(fun Frontier_hash -> "frontier_hash")
end

module Error = struct
  type not_found_member =
    [ `Root
    | `Best_tip
    | `Frontier_hash
    | `Root_transition
    | `Best_tip_transition
    | `Parent_transition
    | `New_root_transition
    | `Old_root_transition
    | `Transition of State_hash.t
    | `Arcs of State_hash.t ]

  type not_found = [`Not_found of not_found_member]

  type t = [not_found | `Invalid_version]

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
      | `Parent_transition ->
          ("parent transition", None)
      | `New_root_transition ->
          ("new root transition", None)
      | `Old_root_transition ->
          ("old root transition", None)
      | `Transition hash ->
          ("transition", Some hash)
      | `Arcs hash ->
          ("arcs", Some hash)
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

(* TODO: batch reads might be nice *)
let check t =
  match get_if_exists t.db ~key:Db_version ~default:0 with
  | 0 ->
      Error `Not_initialized
  | v when v = version ->
      let%bind root =
        get t.db ~key:Root ~error:(`Corrupt (`Not_found `Root))
      in
      let%bind best_tip =
        get t.db ~key:Best_tip ~error:(`Corrupt (`Not_found `Best_tip))
      in
      let%bind _ =
        get t.db ~key:Frontier_hash
          ~error:(`Corrupt (`Not_found `Frontier_hash))
      in
      let%bind _ =
        get t.db ~key:(Transition root.hash)
          ~error:(`Corrupt (`Not_found `Root_transition))
      in
      let%map _ =
        get t.db ~key:(Transition best_tip)
          ~error:(`Corrupt (`Not_found `Best_tip_transition))
      in
      (* TODO: crawl from root and validate tree structure is not malformed (#3737) *)
      ()
  | _ ->
      Error `Invalid_version

let initialize t ~root_data ~base_hash =
  let open Root_data.Limited.Stable.Latest in
  let {With_hash.hash= root_state_hash; data= root_transition}, _ =
    External_transition.Validated.erase root_data.transition
  in
  Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
    ~metadata:[("root_state_hash", State_hash.to_yojson root_state_hash)]
    "Initializing persistent frontier database with $minimal_root_data" ;
  Batch.with_batch t.db ~f:(fun batch ->
      Batch.set batch ~key:Db_version ~data:version ;
      Batch.set batch ~key:(Transition root_state_hash) ~data:root_transition ;
      Batch.set batch ~key:(Arcs root_state_hash) ~data:[] ;
      Batch.set batch ~key:Root ~data:(Root_data.Minimal.of_limited root_data) ;
      Batch.set batch ~key:Best_tip ~data:root_state_hash ;
      Batch.set batch ~key:Frontier_hash ~data:base_hash )

let add t ~transition =
  let parent_hash = External_transition.Validated.parent_hash transition in
  let {With_hash.hash; data= raw_transition}, _ =
    External_transition.Validated.erase transition
  in
  let%map () =
    Result.ok_if_true
      (mem t.db ~key:(Transition parent_hash))
      ~error:(`Not_found `Parent_transition)
  in
  let parent_arcs = get_if_exists t.db ~key:(Arcs parent_hash) ~default:[] in
  Batch.with_batch t.db ~f:(fun batch ->
      Batch.set batch ~key:(Transition hash) ~data:raw_transition ;
      Batch.set batch ~key:(Arcs parent_hash) ~data:(hash :: parent_arcs) )

let move_root t ~new_root ~garbage =
  let open Root_data.Minimal.Stable.V1 in
  let%bind () =
    Result.ok_if_true
      (mem t.db ~key:(Transition new_root.hash))
      ~error:(`Not_found `New_root_transition)
  in
  let%map old_root =
    get t.db ~key:Root ~error:(`Not_found `Old_root_transition)
  in
  (* TODO: Result compatible rocksdb batch transaction *)
  Batch.with_batch t.db ~f:(fun batch ->
      Batch.set batch ~key:Root ~data:new_root ;
      List.iter (old_root.hash :: garbage) ~f:(fun node_hash ->
          (* because we are removing entire forks of the tree, there is
           * no need to have extra logic to any remove arcs to the node
           * we are deleting since there we are deleting all of a node's
           * parents as well
           *)
          Batch.remove batch ~key:(Transition node_hash) ;
          Batch.remove batch ~key:(Arcs node_hash) ) ) ;
  old_root.hash

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

let get_root_hash t =
  let%map root = get_root t in
  root.hash

let get_best_tip t = get t.db ~key:Best_tip ~error:(`Not_found `Best_tip)

let set_best_tip t hash =
  let%map old_best_tip_hash = get_best_tip t in
  (* no need to batch because we only do one operation *)
  set t.db ~key:Best_tip ~data:hash ;
  old_best_tip_hash

let get_frontier_hash t =
  get t.db ~key:Frontier_hash ~error:(`Not_found `Frontier_hash)

let set_frontier_hash t hash = set t.db ~key:Frontier_hash ~data:hash

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
