open Core_kernel
open Pickles_types
module Ds = Domains

let bits ~len n = List.init len ~f:(fun i -> (n lsr i) land 1 = 1)

let max_log2_degree = 32

module Width : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t [@@deriving sexp, equal, compare, hash, yojson]
    end
  end]

  val of_int_exn : int -> t

  val to_int : t -> int

  val to_bits : t -> bool list

  val zero : t

  module Max = Nat.N2

  module Max_vector : Vector.With_version(Max).S

  module Max_at_most : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type 'a t = ('a, Max.n) At_most.t
        [@@deriving compare, sexp, yojson, hash, equal]
      end
    end]
  end

  module Length : Nat.Add.Intf_transparent
end = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = char [@@deriving sexp, equal, compare, hash, yojson]

      let to_latest = Fn.id
    end
  end]

  let zero = Char.of_int_exn 0

  module Max = Nat.N2

  (* Think about versioning here! These vector types *will* change
     serialization if the numbers above change, and so will require a new
     version number. Thus, it's important that these are modules with new
     versioned types, and not just module aliases to the corresponding vector
     implementation.
  *)
  module Max_vector = struct
    [%%versioned
    module Stable = struct
      [@@@no_toplevel_latest_type]

      module V1 = struct
        type 'a t = 'a Vector.Vector_2.Stable.V1.t
        [@@deriving compare, yojson, sexp, hash, equal]
      end
    end]

    type 'a t = 'a Vector.Vector_2.t
    [@@deriving compare, yojson, sexp, hash, equal]

    let map = Vector.map

    let of_list_exn = Vector.Vector_2.of_list_exn

    let to_list = Vector.to_list
  end

  module Max_at_most = struct
    [%%versioned
    module Stable = struct
      [@@@no_toplevel_latest_type]

      module V1 = struct
        type 'a t = 'a At_most.At_most_2.Stable.V1.t
        [@@deriving compare, yojson, sexp, hash, equal]
      end
    end]

    type 'a t = 'a At_most.At_most_2.t
    [@@deriving compare, yojson, sexp, hash, equal]
  end

  module Length = Nat.N4

  let to_int = Char.to_int

  let to_bits = Fn.compose (bits ~len:(Nat.to_int Length.n)) to_int

  let of_int_exn : int -> t =
    let m = Nat.to_int Max.n in
    fun n ->
      assert (n <= m) ;
      Char.of_int_exn n
end

module Max_branches = struct
  include Nat.N8
  module Log2 = Nat.N3

  let%test "check max_branches" = Nat.to_int n = 1 lsl Nat.to_int Log2.n
end

module Max_branches_vec = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = 'a At_most.At_most_8.Stable.V1.t
      [@@deriving sexp, equal, compare, hash, yojson]
    end
  end]

  let () =
    let _f : type a. unit -> (a t, (a, Max_branches.n) At_most.t) Type_equal.t =
     fun () -> Type_equal.T
    in
    ()
end

module Domains = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = { h : 'a }
      [@@deriving sexp, equal, compare, hash, yojson, hlist, fields]
    end
  end]

  let iter { h } ~f = f h

  let map { h } ~f = { h = f h }
end

module Repr = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'g t =
        { step_data :
            (Domain.Stable.V1.t Domains.Stable.V1.t * Width.Stable.V1.t)
            Max_branches_vec.Stable.V1.t
        ; max_width : Width.Stable.V1.t
        ; wrap_index : 'g list Plonk_verification_key_evals.Stable.V1.t
        }

      let to_latest = Fn.id
    end
  end]
end

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('g, 'vk) t =
        { step_data :
            (Domain.Stable.V1.t Domains.Stable.V1.t * Width.Stable.V1.t)
            Max_branches_vec.Stable.V1.t
        ; max_width : Width.Stable.V1.t
        ; wrap_index : 'g list Plonk_verification_key_evals.Stable.V1.t
        ; wrap_vk : 'vk option
        }
      [@@deriving sexp, equal, compare, hash, yojson]
    end
  end]
end

let dummy_domains = { Domains.h = Domain.Pow_2_roots_of_unity 0 }

let dummy_width = Width.zero

let wrap_index_to_input (type gs f) (g : gs -> f array) =
  let open Random_oracle_input in
  fun t ->
    let [ g1
        ; g2
        ; g3
        ; g4
        ; g5
        ; g6
        ; g7
        ; g8
        ; g9
        ; g10
        ; g11
        ; g12
        ; g13
        ; g14
        ; g15
        ; g16
        ; g17
        ; g18
        ] =
      Plonk_verification_key_evals.to_hlist t
    in
    List.map
      [ g1
      ; g2
      ; g3
      ; g4
      ; g5
      ; g6
      ; g7
      ; g8
      ; g9
      ; g10
      ; g11
      ; g12
      ; g13
      ; g14
      ; g15
      ; g16
      ; g17
      ; g18
      ]
      ~f:(Fn.compose field_elements g)
    |> List.reduce_exn ~f:append

let to_input : _ Poly.t -> _ =
  let open Random_oracle_input in
  let map_reduce t ~f = Array.map t ~f |> Array.reduce_exn ~f:append in
  fun { step_data; max_width; wrap_index } : _ Random_oracle_input.t ->
    let bits ~len n = bitstring (bits ~len n) in
    let num_branches =
      bits ~len:(Nat.to_int Max_branches.Log2.n) (At_most.length step_data)
    in
    let step_domains, step_widths =
      At_most.extend_to_vector step_data
        (dummy_domains, dummy_width)
        Max_branches.n
      |> Vector.unzip
    in
    List.reduce_exn ~f:append
      [ map_reduce (Vector.to_array step_domains) ~f:(fun { Domains.h } ->
            map_reduce [| h |] ~f:(fun (Pow_2_roots_of_unity x) ->
                bits ~len:max_log2_degree x))
      ; Array.map (Vector.to_array step_widths) ~f:Width.to_bits |> bitstrings
      ; bitstring (Width.to_bits max_width)
      ; wrap_index_to_input
          (Fn.compose Array.of_list
             (List.concat_map ~f:(fun (x, y) -> [ x; y ])))
          wrap_index
      ; num_branches
      ]
