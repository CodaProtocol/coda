open Core

module type Key = sig
  type t [@@deriving sexp]

  module Stable :
    sig
      module V1 : sig
        type t [@@deriving sexp, bin_io]
      end

      module Latest = V1
    end
    with type V1.t = t

  val empty : t

  val to_string : t -> string

  include Hashable.S_binable with type t := t

  include Comparable.S with type t := t
end

module type Token_id = sig
  type t [@@deriving sexp]

  module Stable :
    sig
      module Latest : sig
        type t [@@deriving bin_io]
      end
    end
    with type Latest.t = t

  val default : t

  val next : t -> t

  include Hashable.S_binable with type t := t

  include Comparable.S_binable with type t := t
end

module type Account_id = sig
  type key

  type token_id

  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t [@@deriving sexp]
    end
  end]

  val public_key : t -> key

  val token_id : t -> token_id

  val create : key -> token_id -> t

  include Hashable.S_binable with type t := t

  include Comparable.S with type t := t
end

module type Balance = sig
  type t [@@deriving eq]

  val zero : t

  val to_int : t -> int
end

module type Account = sig
  type t [@@deriving bin_io, eq, sexp, compare]

  type token_id

  type account_id

  type balance

  val token : t -> token_id

  val identifier : t -> account_id

  val balance : t -> balance

  val token_owner : t -> bool

  val empty : t
end

module type Hash = sig
  type t [@@deriving bin_io, compare, eq, sexp, yojson]

  val to_string : t -> string

  include Hashable.S_binable with type t := t

  type account

  val merge : height:int -> t -> t -> t

  val hash_account : account -> t

  val empty_account : t
end

module type Depth = sig
  val depth : int
end

module type Key_value_database = sig
  type t [@@deriving sexp]

  type config

  include
    Key_value_database.Intf.Ident
    with type t := t
     and type key := Bigstring.t
     and type value := Bigstring.t
     and type config := config

  val create_checkpoint : t -> string -> t

  val make_checkpoint : t -> string -> unit

  val get_uuid : t -> Uuid.t

  val set_batch :
       t
    -> ?remove_keys:Bigstring.t list
    -> key_data_pairs:(Bigstring.t * Bigstring.t) list
    -> unit

  val to_alist : t -> (Bigstring.t * Bigstring.t) list

  (* an association list, sorted by key *)
end

module type Storage_locations = sig
  val key_value_db_dir : string
end
