open Core

module type S = sig
  (** a Merkle hash associated with the root node *)
  type root_hash

  (** a Merkle hash associated any non-root node *)
  type hash

  type account

  type key

  type token_id

  type token_id_set

  type account_id

  type account_id_set

  type index = int

  (** no deriving, purposely; signatures that include this one may add deriving *)
  type t

  module Addr : module type of Merkle_address

  module Path : Merkle_path.S with type hash := hash

  module Location : sig
    type t [@@deriving sexp, compare, hash, eq]
  end

  include
    Syncable_intf.S
    with type root_hash := root_hash
     and type hash := hash
     and type account := account
     and type addr := Addr.t
     and type path = Path.t
     and type t := t

  (** list of accounts in the ledger *)
  val to_list : t -> account list

  (** iterate over all indexes and accounts *)
  val iteri : t -> f:(index -> account -> unit) -> unit

  (** fold over accounts in the ledger, passing the Merkle address *)
  val foldi :
    t -> init:'accum -> f:(Addr.t -> 'accum -> account -> 'accum) -> 'accum

  (** the set of [account_id]s are ledger elements to skip during the fold,
      because they're in a mask
  *)
  val foldi_with_ignored_accounts :
       t
    -> account_id_set
    -> init:'accum
    -> f:(Addr.t -> 'accum -> account -> 'accum)
    -> 'accum

  (** fold over accounts until stop condition reached when calling [f]; calls [finish] for
     result
 *)
  val fold_until :
       t
    -> init:'accum
    -> f:('accum -> account -> ('accum, 'stop) Base.Continue_or_stop.t)
    -> finish:('accum -> 'stop)
    -> 'stop

  (** set of account ids associated with accounts *)
  val accounts : t -> account_id_set

  (** Get the public key that owns a token. *)
  val token_owner : t -> token_id -> key option

  (** Get the set of all accounts which own a token. *)
  val token_owners : t -> account_id_set

  (** Get all of the tokens for which a public key has accounts. *)
  val tokens : t -> key -> token_id_set

  (** The next token that is not present in the ledger. This token will be used
      as the new token when a [Create_new_token] transaction is processed.
  *)
  val next_available_token : t -> token_id

  (** Sets the next available token in the ledger,
      to be returned by [next_available_token]
  *)
  val set_next_available_token : t -> token_id -> unit

  val location_of_account : t -> account_id -> Location.t option

  (** This may return an error if the ledger is full. *)
  val get_or_create_account :
    t -> account_id -> account -> ([`Added | `Existed] * Location.t) Or_error.t

  (** the ledger should not be used after calling [close] *)
  val close : t -> unit

  (** for account locations in the ledger, the last (rightmost) filled location *)
  val last_filled : t -> Location.t option

  val get_uuid : t -> Uuid.t

  (** return Some [directory] for ledgers that use a file system, else None *)
  val get_directory : t -> string option

  val get : t -> Location.t -> account option

  val set : t -> Location.t -> account -> unit

  val set_batch : t -> (Location.t * account) list -> unit

  val get_at_index_exn : t -> int -> account

  val set_at_index_exn : t -> int -> account -> unit

  val index_of_account_exn : t -> account_id -> int

  (** meant to be a fast operation: the root hash is stored, rather
      than calculated dynamically
  *)
  val merkle_root : t -> root_hash

  val merkle_path : t -> Location.t -> Path.t

  val merkle_path_at_index_exn : t -> int -> Path.t

  val remove_accounts_exn : t -> account_id list -> unit

  (** Triggers when the ledger has been detached and should no longer be
      accessed.
  *)
  val detached_signal : t -> unit Async.Deferred.t
end
