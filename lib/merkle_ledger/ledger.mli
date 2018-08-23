module Make
    (Key : Intf.Key) (Account : sig
        type t [@@deriving sexp, eq, bin_io]

        val public_key : t -> Key.t
    end)
    (Hash : sig
              type t [@@deriving sexp, hash, compare, bin_io]

              include Intf.Hash with type t := t
            end
            with type account := Account.t) (Depth : sig
        val depth : int
    end) :
  Intf.Ledger_S
  with type hash := Hash.t
   and type account := Account.t
   and type key := Key.t
