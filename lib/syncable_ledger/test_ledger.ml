open Core
open Unsigned

module Make (Inputs : sig
  val depth : int

  val num_accts : int
end) =
struct
  open Merkle_ledger.Test_stubs

  module Root_hash = struct
    include Hash

    let to_hash = Fn.id

    type account = Account.t
  end

  module L = struct
    include Merkle_ledger.Ledger.Make (Key) (Account) (Hash) (Inputs)

    type path = Path.t

    type addr = Addr.t

    type account = Account.t

    type hash = Root_hash.t

    (* TODO: Make this into a functors *)
    let load_ledger n b =
      let ledger = create () in
      let keys = List.init n ~f:(fun i -> Int.to_string i) in
      List.iter keys ~f:(fun k ->
          set ledger k {Account.balance= UInt64.of_int b; public_key= k} ) ;
      (ledger, keys)
  end

  module SL =
    Syncable_ledger.Make (L.Addr) (Account) (Root_hash) (Root_hash) (L)
      (struct
        let subtree_height = 3
      end)

  module SR =
    Syncable_ledger.Make_sync_responder (L.Addr) (Account) (Root_hash)
      (Root_hash)
      (L)
      (SL)

  let num_accts = Inputs.num_accts
end
