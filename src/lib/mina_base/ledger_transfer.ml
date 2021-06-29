open Core_kernel

module type Base_ledger_intf =
  Merkle_ledger.Base_ledger_intf.S
  with type account := Account.t
   and type key := Signature_lib.Public_key.Compressed.t
   and type token_id := Token_id.t
   and type token_id_set := Token_id.Set.t
   and type account_id := Account_id.t
   and type account_id_set := Account_id.Set.t
   and type hash := Ledger_hash.t
   and type root_hash := Ledger_hash.t

module Make
    (Source : Base_ledger_intf)
    (Dest : Base_ledger_intf with type Addr.t = Source.Addr.t) : sig
  val transfer_accounts : src:Source.t -> dest:Dest.t -> Dest.t Or_error.t
end = struct
  let transfer_accounts ~src ~dest =
    let accounts =
      Source.foldi src ~init:[] ~f:(fun addr acc account ->
          (addr, account) :: acc )
    in
    Dest.set_batch_accounts dest accounts ;
    let src_hash = Source.merkle_root src in
    let dest_hash = Dest.merkle_root dest in
    if not (Ledger_hash.equal src_hash dest_hash) then
      Or_error.errorf
        "Merkle roots differ after transfer: expected %s, actual %s"
        (Ledger_hash.to_string src_hash)
        (Ledger_hash.to_string dest_hash)
    else Ok dest
end

module From_sparse_ledger (Dest : Base_ledger_intf) : sig
  val transfer_accounts :
    src:Sparse_ledger.t -> dest:Dest.t -> Dest.t Or_error.t
end = struct
  let transfer_accounts ~src ~dest =
    if Dest.depth dest = Sparse_ledger.depth src then
      let open Or_error.Let_syntax in
      let%bind () =
        Or_error.try_with (fun () ->
            Sparse_ledger.iteri src ~f:(fun _idx account ->
                let id = Account.identifier account in
                ignore
                  ( Dest.get_or_create_account dest id account
                    |> Or_error.ok_exn
                    : [`Added | `Existed] * Dest.Location.t ) ) )
      in
      let src_hash = Sparse_ledger.merkle_root src in
      let dest_hash = Dest.merkle_root dest in
      if not (Ledger_hash.equal src_hash dest_hash) then
        Or_error.errorf
          "Merkle roots differ after transfer: expected %s, actual %s"
          (Ledger_hash.to_string src_hash)
          (Ledger_hash.to_string dest_hash)
      else Ok dest
    else
      Or_error.errorf
        "Ledger depth of src and dest doesn't match: src %d, dest %d"
        (Sparse_ledger.depth src) (Dest.depth dest)
end
