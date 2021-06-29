(* maskable_merkle_tree.ml -- Merkle tree that can have associated masks *)

open Core

module type Inputs_intf = sig
  include Inputs_intf.S

  module Mask :
    Masking_merkle_tree_intf.S
    with module Location = Location
     and type account := Account.t
     and type location := Location.t
     and type hash := Hash.t
     and type key := Key.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t
     and type parent := Base.t

  val mask_to_base : Mask.Attached.t -> Base.t
end

module Make (Inputs : Inputs_intf) = struct
  open Inputs
  include Base

  (** Maps parent ledger UUIDs to child masks. *)
  let (registered_masks : Mask.Attached.t list Uuid.Table.t) =
    Uuid.Table.create ()

  module Node = struct
    type t = Mask.Attached.t

    type attached =
      {hash: string; uuid: string; total_currency: int; num_accounts: int}
    [@@deriving yojson]

    type dangling = {uuid: string; nulled_at: string} [@@deriving yojson]

    type display = [`Attached of attached | `Dangling_parent of dangling]
    [@@deriving yojson]

    let format_uuid mask =
      Visualization.display_prefix_of_string @@ Uuid.to_string
      @@ Mask.Attached.get_uuid mask

    let name mask = sprintf !"\"%s \"" (format_uuid mask)

    let display_attached_mask mask =
      let root_hash = Mask.Attached.merkle_root mask in
      let num_accounts = Mask.Attached.num_accounts mask in
      let total_currency =
        Mask.Attached.foldi mask ~init:0 ~f:(fun _ total_currency account ->
            (* only default token matters for total currency *)
            if Token_id.equal (Account.token account) Token_id.default then
              total_currency + (Balance.to_int @@ Account.balance account)
            else total_currency )
      in
      let uuid = format_uuid mask in
      { hash= Visualization.display_prefix_of_string @@ Hash.to_string root_hash
      ; num_accounts
      ; total_currency
      ; uuid }

    let display mask =
      try `Attached (display_attached_mask mask)
      with Mask.Attached.Dangling_parent_reference (_, nulled_at) ->
        `Dangling_parent {uuid= format_uuid mask; nulled_at}

    let equal mask1 mask2 =
      let open Mask.Attached in
      Uuid.equal (get_uuid mask1) (get_uuid mask2)

    let compare mask1 mask2 =
      let open Mask.Attached in
      Uuid.compare (get_uuid mask1) (get_uuid mask2)

    let hash mask = Uuid.hash @@ Mask.Attached.get_uuid mask
  end

  module Graphviz = Visualization.Make_ocamlgraph (Node)

  let to_graph () =
    let masks = List.concat @@ Uuid.Table.data registered_masks in
    let uuid_to_masks_table =
      Uuid.Table.of_alist_exn
        (List.map masks ~f:(fun mask -> (Mask.Attached.get_uuid mask, mask)))
    in
    let open Graphviz in
    Uuid.Table.fold uuid_to_masks_table ~init:empty
      ~f:(fun ~key:uuid ~data:mask graph ->
        let graph_with_mask = add_vertex graph mask in
        Uuid.Table.find registered_masks uuid
        |> Option.value_map ~default:graph_with_mask ~f:(fun children_masks ->
               List.fold ~init:graph_with_mask children_masks
                 ~f:(fun graph_with_mask_and_child ->
                   add_edge graph_with_mask_and_child mask ) ) )

  module Debug = struct
    let visualize ~filename =
      Out_channel.with_file filename ~f:(fun output_channel ->
          let graph = to_graph () in
          Graphviz.output_graph output_channel graph )
  end

  module Visualize = struct
    module Summary = struct
      type t = [`Uuid of Uuid.t] * [`Hash of Hash.t] [@@deriving sexp_of]
    end

    type t = Leaf of Summary.t | Node of Summary.t * t list
    [@@deriving sexp_of]

    module type Crawler_intf = sig
      type t

      val get_uuid : t -> Uuid.t

      val merkle_root : t -> Hash.t
    end

    let rec _crawl : type a. (module Crawler_intf with type t = a) -> a -> t =
     fun (module C) c ->
      let summary =
        let uuid = C.get_uuid c in
        ( `Uuid uuid
        , `Hash
            ( try C.merkle_root c
              with _ ->
                Core.printf !"CAUGHT %{sexp: Uuid.t}\n%!" uuid ;
                Hash.empty_account ) )
      in
      match Uuid.Table.find registered_masks (C.get_uuid c) with
      | None ->
          Leaf summary
      | Some masks ->
          Node (summary, List.map masks ~f:(_crawl (module Mask.Attached)))
  end

  let register_mask t mask =
    let attached_mask = Mask.set_parent mask t in
    List.iter (Uuid.Table.data registered_masks) ~f:(fun ms ->
        List.iter ms ~f:(fun m ->
            [%test_result: bool]
              ~message:
                "We've already registered a mask with this UUID; you have a bug"
              ~expect:false
              (Uuid.equal (Mask.Attached.get_uuid m) (Mask.get_uuid mask)) ) ) ;
    (* handles cases where no entries for t, or where there are existing entries *)
    Uuid.Table.add_multi registered_masks ~key:(get_uuid t) ~data:attached_mask ;
    attached_mask

  let rec unregister_mask_exn ?(grandchildren = `Check) ~loc
      (mask : Mask.Attached.t) : Mask.unattached =
    let parent_uuid = Mask.Attached.get_parent mask |> get_uuid in
    let error_msg suffix =
      sprintf "Couldn't unregister mask with UUID %s from parent %s, %s"
        (Mask.Attached.get_uuid mask |> Uuid.to_string_hum)
        (Uuid.to_string_hum parent_uuid)
        suffix
    in
    let trigger_detach_signal =
      match grandchildren with
      | `Check | `Recursive ->
          true
      | `I_promise_I_am_reparenting_this_mask ->
          false
    in
    ( match grandchildren with
    | `Check -> (
      match Hashtbl.find registered_masks (Mask.Attached.get_uuid mask) with
      | Some children ->
          failwith @@ error_msg
          @@ sprintf
               !"mask has children that must be unregistered first: %{sexp: \
                 Uuid.t list}"
               (List.map ~f:Mask.Attached.get_uuid children)
      | None ->
          () )
    | `I_promise_I_am_reparenting_this_mask ->
        ()
    | `Recursive ->
        (* You must not retain any references to children of the mask we're
           unregistering if you pass `Recursive, so this is only used in
           with_ephemeral_ledger. *)
        List.iter
          ( Hashtbl.find registered_masks (Mask.Attached.get_uuid mask)
          |> Option.value ~default:[] )
          ~f:(fun child_mask ->
            ignore
            @@ unregister_mask_exn ~loc ~grandchildren:`Recursive child_mask )
    ) ;
    match Uuid.Table.find registered_masks parent_uuid with
    | None ->
        failwith @@ error_msg "parent not in registered_masks"
    | Some masks ->
        ( match List.find masks ~f:(fun m -> phys_equal m mask) with
        | None ->
            failwith @@ error_msg "mask not registered with that parent"
        | Some _ -> (
            let bad, good =
              List.partition_tf masks ~f:(fun m -> phys_equal m mask)
            in
            assert (List.length bad = 1) ;
            match good with
            | [] ->
                (* no other masks for this maskable *)
                Uuid.Table.remove registered_masks parent_uuid
            | other_masks ->
                Uuid.Table.set registered_masks ~key:parent_uuid
                  ~data:other_masks ) ) ;
        Mask.Attached.unset_parent ~trigger_signal:trigger_detach_signal ~loc
          mask

  (** a set calls the Base implementation set, notifies registered mask childen *)
  let set t location account =
    Base.set t location account ;
    match Uuid.Table.find registered_masks (get_uuid t) with
    | None ->
        ()
    | Some masks ->
        List.iter masks ~f:(fun mask ->
            Mask.Attached.parent_set_notify mask account )

  let remove_and_reparent_exn t t_as_mask =
    let parent = Mask.Attached.get_parent t_as_mask in
    let merkle_root = Mask.Attached.merkle_root t_as_mask in
    (* we can only reparent if merkle roots are the same *)
    assert (Hash.equal (Base.merkle_root parent) merkle_root) ;
    let children =
      Hashtbl.find registered_masks (get_uuid t) |> Option.value ~default:[]
    in
    let dangling_masks =
      List.map children ~f:(fun c ->
          unregister_mask_exn ~loc:__LOC__
            ~grandchildren:`I_promise_I_am_reparenting_this_mask c )
    in
    ignore (unregister_mask_exn ~loc:__LOC__ t_as_mask : Mask.unattached) ;
    List.iter dangling_masks ~f:(fun m ->
        ignore (register_mask parent m : Mask.Attached.t) )

  let batch_notify_mask_children t accounts =
    match Uuid.Table.find registered_masks (get_uuid t) with
    | None ->
        ()
    | Some masks ->
        List.iter masks ~f:(fun mask ->
            List.iter accounts ~f:(fun account ->
                Mask.Attached.parent_set_notify mask account ) )

  let set_batch t locations_and_accounts =
    Base.set_batch t locations_and_accounts ;
    batch_notify_mask_children t (List.map locations_and_accounts ~f:snd)

  let set_batch_accounts t addresses_and_accounts =
    Base.set_batch_accounts t addresses_and_accounts ;
    batch_notify_mask_children t (List.map addresses_and_accounts ~f:snd)

  let set_all_accounts_rooted_at_exn t address accounts =
    Base.set_all_accounts_rooted_at_exn t address accounts ;
    batch_notify_mask_children t accounts
end
