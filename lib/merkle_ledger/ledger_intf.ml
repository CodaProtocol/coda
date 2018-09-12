open Core

module type S = sig
  type hash

  type account

  type key

  type index = int

  type t [@@deriving sexp, bin_io]

  val copy : t -> t

  module Path : Merkle_path.S with type hash := hash

  module Addr : Merkle_address.S

  module Location : sig
    type t
  end

  val create : unit -> t

  val location_of_key : t -> key -> Location.t option

  val get : t -> Location.t -> account option

  val set : t -> Location.t -> account -> unit

  val create_account_exn : t -> key -> account -> Location.t

  val hash : t -> Ppx_hash_lib.Std.Hash.hash_value

  val hash_fold_t :
    Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state

  val compare : t -> t -> int

  val merkle_path : t -> Location.t -> Path.t

  val key_of_index : t -> index -> key option

  val key_of_index_exn : t -> index -> key

  val index_of_key_exn : t -> key -> index

  val merkle_path_at_index : t -> index -> Path.t

  val get_at_index_exn : t -> index -> account

  val set_at_index_exn : t -> index -> account -> unit

  val set_at_addr_exn : t -> Addr.t -> account -> unit

  include Syncable_intf.S
          with type root_hash := hash
           and type hash := hash
           and type account := account
           and type addr := Addr.t
           and type t := t
           and type path := Path.t

  val recompute_tree : t -> unit
end
