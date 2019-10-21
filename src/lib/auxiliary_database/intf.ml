open Signature_lib

module type Pagination = sig
  type time

  type cursor

  type value

  type t

  (** [get_total_values t public_key] returns all values that pertains to
      [public_key] if [public_key] is some value. Otherwise, it will return
      all values in the pagination data structure if none are provided *)
  val get_all_values : t -> Public_key.Compressed.t option -> value list

  (** [get_total_values t public_key] returns the number of values that
      pertains to [public_key] if [public_key] is some value. Otherwise, it
      will return the number of total values in the pagination data structure
      if none are provided *)
  val get_total_values : t -> Public_key.Compressed.t option -> int option

  (** [query t pk cursor n] makes a pagination query based on different inputs.
      If [cursor] is some value and the underlying pagination data structure
      contains the value, the pagination queries are offset by [cursor]. If
      [cursor] is none, then the offset will be the earliest value, if
      [navigation]=`Earlier. Otherwise, the offset will be the latest value.

      [query] will query the latest values that occurs before the offset if
      [navigation]=`Earlier. Otherwise, [query] will query the earliest values
      that occur after the offset if [navigation]=`Later.

      The pagination can paginate on all the values if
      [value_filter_specification]=`All or it will paginate just some user A value
      if [value_filter_specification]=[`User_only A]

      The number of values that the pagination request contains is based on the
      value of [num_pages]. If [num_pages] is None, then it will return all
      values that occurred after the cursor offset if [navigation]=`Later is
      specified, otherwise it will return all values that occurred before the
      cursor offset if [navigation]=`Earlier *)
  val query :
       t
    -> navigation:[`Earlier | `Later]
    -> cursor:cursor option
    -> value_filter_specification:[`All | `User_only of Public_key.Compressed.t]
    -> num_pages:int option
    -> value list * [`Has_earlier_page of bool] * [`Has_later_page of bool]
end

(** Transaction database is a database that stores transactions for a client.
    It queries a set of transactions involving a peer with a certain public
    key. This database allows a developer to store transactions and perform
    pagination queries of transactions via GraphQL *)
module type Transaction = sig
  type t

  type time

  type transaction

  val create : logger:Logger.t -> string -> t

  val close : t -> unit

  val add : t -> transaction -> time -> unit

  include
    Pagination
    with type t := t
     and type time := time
     and type value := transaction
     and type cursor := transaction
end

module type External_transition = sig
  type t

  type filtered_external_transition

  type hash

  type time

  val create : logger:Logger.t -> string -> t

  val close : t -> unit

  val add :
    t -> (filtered_external_transition, hash) With_hash.t -> time -> unit

  include
    Pagination
    with type t := t
     and type time := time
     and type cursor := hash
     and type value := (filtered_external_transition, hash) With_hash.t
end
