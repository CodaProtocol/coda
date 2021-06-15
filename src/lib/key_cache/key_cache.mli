open Core_kernel

module Spec : sig
  type t =
    | On_disk of {directory: string; should_write: bool}
    | S3 of {bucket_prefix: string; install_path: string}

  include Comparable.S_plain with type t := t

  module List : sig
    type nonrec t = t list

    include Comparable.S_plain with type t := t
  end
end

val set_downloads_enabled : bool -> unit

module T (M : sig
  type _ t
end) : sig
  type ('a, 'b) t = {write: 'a -> 'b -> unit M.t; read: 'a -> 'b M.t}
end

module Disk_storable (M : sig
  type _ t
end) : sig
  type ('k, 'v) t =
    { to_string: 'k -> string
    ; read: 'k -> path:string -> 'v M.t
    ; write: 'k -> 'v -> string -> unit M.t }
end

module type S = sig
  module M : sig
    type _ t
  end

  type ('a, 'b) t = ('a, 'b) T(M).t =
    {write: 'a -> 'b -> unit M.t; read: 'a -> 'b M.t}

  module Disk_storable : sig
    type ('k, 'v) t = ('k, 'v) Disk_storable(M).t =
      { to_string: 'k -> string
      ; read: 'k -> path:string -> 'v M.t
      ; write: 'k -> 'v -> string -> unit M.t }

    val of_binable :
      ('k -> string) -> (module Binable.S with type t = 'v) -> ('k, 'v) t

    val simple :
         ('k -> string)
      -> ('k -> path:string -> 'v M.t)
      -> ('k -> 'v -> string -> unit M.t)
      -> ('k, 'v) t
  end

  val read :
       Spec.t list
    -> ('k, 'v) Disk_storable.t
    -> 'k
    -> ('v * [> `Cache_hit | `Locally_generated]) M.t

  val write : Spec.t list -> ('k, 'v) Disk_storable.t -> 'k -> 'v -> unit M.t
end

module Sync : S with module M := Or_error

module Async : S with module M := Async.Deferred.Or_error
