open Core_kernel
open Pickles_types
module Ds = Domains

let bits ~len n = List.init len ~f:(fun i -> (n lsr i) land 1 = 1)

let max_log2_degree = 32

module Width : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t [@@deriving sexp, eq, compare, hash, yojson]
    end
  end]

  val of_int_exn : int -> t

  val to_int : t -> int

  val to_bits : t -> bool list

  val zero : t

  module Max : Nat.Add.Intf_transparent

  module Length : Nat.Add.Intf_transparent
end = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = char [@@deriving sexp, eq, compare, hash, yojson]

      let to_latest = Fn.id
    end
  end]

  let zero = Char.of_int_exn 0

  module Max = Nat.N2
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
  module T = At_most.With_length (Max_branches)

  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = 'a T.t [@@deriving version {asserted}]
    end
  end]
end

module Domains = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = {h: 'a}
      [@@deriving sexp, eq, compare, hash, yojson, hlist, fields]
    end
  end]

  let iter {h} ~f = f h

  let map {h} ~f = {h= f h}
end

module Repr = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'g t =
        { step_data:
            (Domain.Stable.V1.t Domains.Stable.V1.t * Width.Stable.V1.t)
            Max_branches_vec.Stable.V1.t
        ; max_width: Width.Stable.V1.t
        ; wrap_index: 'g list Plonk_verification_key_evals.Stable.V1.t }

      let to_latest = Fn.id
    end
  end]
end

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('g, 'vk) t =
        { step_data:
            (Domain.Stable.V1.t Domains.Stable.V1.t * Width.Stable.V1.t)
            Max_branches_vec.T.t
        ; max_width: Width.Stable.V1.t
        ; wrap_index: 'g list Plonk_verification_key_evals.Stable.V1.t
        ; wrap_vk: ('vk option[@of_yojson fun _ -> Ok None]) }
      [@@deriving sexp, eq, compare, hash, yojson]
    end
  end]
end

let dummy_domains = {Domains.h= Domain.Pow_2_roots_of_unity 0}

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
        ; g18 ] =
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
      ; g18 ]
      ~f:(Fn.compose field_elements g)
    |> List.reduce_exn ~f:append

let to_input : _ Poly.t -> _ =
  let open Random_oracle_input in
  let map_reduce t ~f = Array.map t ~f |> Array.reduce_exn ~f:append in
  fun {step_data; max_width; wrap_index} ->
    ( let bits ~len n = bitstring (bits ~len n) in
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
        [ map_reduce (Vector.to_array step_domains) ~f:(fun {Domains.h} ->
              map_reduce [|h|] ~f:(fun (Pow_2_roots_of_unity x) ->
                  bits ~len:max_log2_degree x ) )
        ; Array.map (Vector.to_array step_widths) ~f:Width.to_bits
          |> bitstrings
        ; bitstring (Width.to_bits max_width)
        ; wrap_index_to_input
            (Fn.compose Array.of_list
               (List.concat_map ~f:(fun (x, y) -> [x; y])))
            wrap_index
        ; num_branches ]
      : _ Random_oracle_input.t )

let to_repr {Poly.step_data; max_width; wrap_index; wrap_vk= _} =
  {Repr.step_data; max_width; wrap_index}

module Make (G : sig
  type t [@@deriving sexp, bin_io, eq, compare, hash, yojson]
end) (Vk : sig
  type t [@@deriving sexp, eq, compare, hash, yojson]

  val of_repr : G.t Repr.t -> t
end) : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t = (G.t, Vk.t) Poly.Stable.V1.t
      [@@deriving sexp, eq, compare, hash, yojson]
    end
  end]
end = struct
  [%%versioned_binable
  module Stable = struct
    module V1 = struct
      type t = (G.t, Vk.t) Poly.Stable.V1.t
      [@@deriving sexp, eq, compare, hash, yojson]

      let of_yojson json =
        let open Result.Let_syntax in
        let%map t = of_yojson json in
        {t with Poly.wrap_vk= Some (Vk.of_repr (to_repr t))}

      let to_latest = Fn.id

      module R = struct
        type t = G.t Repr.Stable.Latest.t [@@deriving bin_io]
      end

      include Binable.Of_binable
                (R)
                (struct
                  type nonrec t = t

                  let to_binable = to_repr

                  let of_binable
                      ( {Repr.Stable.V1.step_data; max_width; wrap_index= c} as
                      t ) =
                    { Poly.step_data
                    ; max_width
                    ; wrap_index= c
                    ; wrap_vk= Some (Vk.of_repr t) }
                end)
    end
  end]
end
