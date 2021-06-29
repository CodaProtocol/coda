open Core_kernel

module type Inputs_intf = sig
  module Location : Location_intf.S

  module Location_binable : Hashable.S_binable with type t := Location.t

  module Key : Intf.Key

  module Token_id : Intf.Token_id

  module Account_id :
    Intf.Account_id with type key := Key.t and type token_id := Token_id.t

  module Balance : Intf.Balance

  module Account :
    Intf.Account
    with type balance := Balance.t
     and type account_id := Account_id.t
     and type token_id := Token_id.t

  module Hash : Intf.Hash with type account := Account.t

  module Base : sig
    type t

    val get : t -> Location.t -> Account.t option

    val last_filled : t -> Location.t option
  end

  val get_hash : Base.t -> Location.t -> Hash.t

  val location_of_account_addr : Location.Addr.t -> Location.t

  val location_of_hash_addr : Location.Addr.t -> Location.t

  val ledger_depth : Base.t -> int

  val set_raw_hash_batch : Base.t -> (Location.t * Hash.t) list -> unit

  val set_raw_account_batch : Base.t -> (Location.t * Account.t) list -> unit

  val set_location_batch :
       last_location:Location.t
    -> Base.t
    -> (Account_id.t * Location.t) Non_empty_list.t
    -> unit
end

module Make (Inputs : Inputs_intf) : sig
  val get_all_accounts_rooted_at_exn :
       Inputs.Base.t
    -> Inputs.Location.Addr.t
    -> (Inputs.Location.Addr.t * Inputs.Account.t) list

  val set_hash_batch :
    Inputs.Base.t -> (Inputs.Location.t * Inputs.Hash.t) list -> unit

  val set_batch :
    Inputs.Base.t -> (Inputs.Location.t * Inputs.Account.t) list -> unit

  val set_batch_accounts :
    Inputs.Base.t -> (Inputs.Location.Addr.t * Inputs.Account.t) list -> unit

  val set_all_accounts_rooted_at_exn :
    Inputs.Base.t -> Inputs.Location.Addr.t -> Inputs.Account.t list -> unit
end = struct
  let get_all_accounts_rooted_at_exn t address =
    let open Inputs in
    let result =
      Location.Addr.Range.fold
        (Location.Addr.Range.subtree_range
           ~ledger_depth:(Inputs.ledger_depth t) address)
        ~init:[]
        ~f:(fun bit_index acc ->
          let account = Base.get t (location_of_account_addr bit_index) in
          (bit_index, account) :: acc )
    in
    List.rev_filter_map result ~f:(function
      | _, None ->
          None
      | addr, Some account ->
          Some (addr, account) )

  let rec compute_affected_locations_and_hashes t locations_and_hashes acc =
    let ledger_depth = Inputs.ledger_depth t in
    if not @@ List.is_empty locations_and_hashes then
      let height =
        Inputs.Location.height ~ledger_depth
        @@ fst
        @@ List.hd_exn locations_and_hashes
      in
      if height < ledger_depth then
        let parents_to_children =
          List.fold locations_and_hashes ~init:Inputs.Location.Map.empty
            ~f:(fun parents_to_children (location, hash) ->
              let parent_location = Inputs.Location.parent location in
              Map.update parents_to_children parent_location ~f:(function
                | Some (`One_side (sibling_location, sibling_hash)) ->
                    assert (
                      not (Inputs.Location.equal location sibling_location) ) ;
                    (* If we have already recorded the sibling, we can compute
                       the hash now.
                    *)
                    let parent_hash =
                      let left_hash, right_hash =
                        Inputs.Location.order_siblings location hash
                          sibling_hash
                      in
                      Inputs.Hash.merge ~height left_hash right_hash
                    in
                    `Hash parent_hash
                | Some (`Hash _) ->
                    assert false
                | None ->
                    (* This is the first child of its parent that we have
                       encountered.
                    *)
                    `One_side (location, hash) ) )
        in
        let rev_parent_locations_and_hashes =
          Map.fold parents_to_children ~init:[] ~f:(fun ~key ~data acc ->
              match data with
              | `One_side (location, hash) ->
                  (* We haven't recorded the sibling, so query the ledger to get
                     the hash.
                  *)
                  let sibling_location = Inputs.Location.sibling location in
                  let sibling_hash = Inputs.get_hash t sibling_location in
                  let parent_hash =
                    let left_hash, right_hash =
                      Inputs.Location.order_siblings location hash sibling_hash
                    in
                    Inputs.Hash.merge ~height left_hash right_hash
                  in
                  (key, parent_hash) :: acc
              | `Hash parent_hash ->
                  (* We have already computed the hash above. *)
                  (key, parent_hash) :: acc )
        in
        compute_affected_locations_and_hashes t rev_parent_locations_and_hashes
          (List.rev_append rev_parent_locations_and_hashes acc)
      else acc
    else acc

  let set_hash_batch t locations_and_hashes =
    Inputs.set_raw_hash_batch t
      (compute_affected_locations_and_hashes t locations_and_hashes
         locations_and_hashes)

  let compute_last_index addresses =
    Non_empty_list.map addresses
      ~f:(Fn.compose Inputs.Location.Addr.to_int Inputs.Location.to_path_exn)
    |> Non_empty_list.max_elt ~compare:Int.compare

  let set_raw_addresses t addresses_and_accounts =
    let ledger_depth = Inputs.ledger_depth t in
    Option.iter (Non_empty_list.of_list_opt addresses_and_accounts)
      ~f:(fun nonempty_addresses_and_accounts ->
        let key_locations =
          Non_empty_list.map nonempty_addresses_and_accounts
            ~f:(fun (address, account) ->
              (Inputs.Account.identifier account, address) )
        in
        let new_last_location =
          let current_last_index =
            let open Option.Let_syntax in
            let%map last_location = Inputs.Base.last_filled t in
            Inputs.Location.Addr.to_int
            @@ Inputs.Location.to_path_exn last_location
          in
          let foreign_last_index =
            compute_last_index
              (Non_empty_list.map nonempty_addresses_and_accounts ~f:fst)
          in
          let max_index_in_all_accounts =
            Option.value_map current_last_index ~default:foreign_last_index
              ~f:(fun max_index -> Int.max max_index foreign_last_index)
          in
          Inputs.Location.(
            Account (Addr.of_int_exn ~ledger_depth max_index_in_all_accounts))
        in
        let last_location = new_last_location in
        Inputs.set_location_batch ~last_location t key_locations )

  (* TODO: When we do batch on a database, we should add accounts, locations and hashes
     simulatenously for full atomicity. *)
  let set_batch t locations_and_accounts =
    set_raw_addresses t locations_and_accounts ;
    Inputs.set_raw_account_batch t locations_and_accounts ;
    set_hash_batch t
    @@ List.map locations_and_accounts ~f:(fun (location, account) ->
           ( Inputs.location_of_hash_addr (Inputs.Location.to_path_exn location)
           , Inputs.Hash.hash_account account ) )

  let set_batch_accounts t addresses_and_accounts =
    set_batch t
    @@ List.map addresses_and_accounts ~f:(fun (addr, account) ->
           (Inputs.location_of_account_addr addr, account) )

  let set_all_accounts_rooted_at_exn t address accounts =
    let addresses =
      Sequence.to_list
      @@ Inputs.Location.Addr.Range.subtree_range_seq
           ~ledger_depth:(Inputs.ledger_depth t) address
    in
    let num_accounts = List.length accounts in
    List.(zip_exn (take addresses num_accounts) accounts)
    |> set_batch_accounts t
end
