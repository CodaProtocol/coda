module type S = sig
  type t [@@deriving bin_io, sexp]

  include Base_ledger_intf.S with type t := t
end
