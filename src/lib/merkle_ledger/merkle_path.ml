open Core_kernel

module type S = sig
  type hash

  type elem = [`Left of hash | `Right of hash] [@@deriving sexp]

  val elem_hash : elem -> hash

  type t = elem list [@@deriving sexp]

  val implied_root : t -> hash -> hash

  val check_path : t -> hash -> hash -> bool
end

module Make (Hash : sig
  type t [@@deriving sexp]

  val merge : height:int -> t -> t -> t
end) : S with type hash := Hash.t = struct
  type elem = [`Left of Hash.t | `Right of Hash.t] [@@deriving sexp]

  let elem_hash = function `Left h | `Right h -> h

  type t = elem list [@@deriving sexp]

  let implied_root (t : t) leaf_hash =
    List.fold t ~init:(leaf_hash, 0) ~f:(fun (acc, height) elem ->
        let acc =
          match elem with
          | `Left h ->
              Hash.merge ~height acc h
          | `Right h ->
              Hash.merge ~height h acc
        in
        (acc, height + 1) )
    |> fst

  let check_path t leaf_hash root_hash = implied_root t leaf_hash = root_hash
end
