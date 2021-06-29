open Intf
open Core
module Bignum_bigint = Snarky_backendless.Backend_extended.Bignum_bigint

module type Input_intf = sig
  type t

  module Bigint : Bigint.Intf

  val size : unit -> Bigint.t

  val size_in_bits : unit -> int

  val to_bigint : t -> Bigint.t

  val of_bigint : Bigint.t -> t

  val of_int : int -> t

  val domain_generator : int -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val inv : t -> t option

  val negate : t -> t

  val square : t -> t

  val sqrt : t -> t option

  val is_square : t -> bool

  val equal : t -> t -> bool

  val print : t -> unit

  val to_string : t -> string

  val of_string : string -> t

  val random : unit -> t

  val rng : int -> t

  val two_adic_root_of_unity : unit -> t

  val mut_add : t -> other:t -> unit

  val mut_mul : t -> other:t -> unit

  val mut_square : t -> unit

  val mut_sub : t -> other:t -> unit

  val copy : over:t -> t -> unit

  val to_bytes : t -> Bytes.t

  val of_bytes : Bytes.t -> t

  val domain_generator : int -> t

  module Vector : Snarky_intf.Vector.S with type elt = t
end

module type S = sig
  type t [@@deriving sexp, compare, yojson, bin_io, hash]

  include Input_intf with type t := t

  val size : Bigint.t

  val domain_generator : log2_size:int -> t

  val one : t

  val zero : t

  val inv : t -> t

  val sqrt : t -> t

  val size_in_bits : int

  val to_bits : t -> bool list

  val of_bits : bool list -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> t

  val ( / ) : t -> t -> t

  module Mutable : sig
    val add : t -> other:t -> unit

    val mul : t -> other:t -> unit

    val square : t -> unit

    val sub : t -> other:t -> unit

    val copy : over:t -> t -> unit
  end

  val ( += ) : t -> t -> unit

  val ( *= ) : t -> t -> unit

  val ( -= ) : t -> t -> unit
end

module type S_with_version = sig
  module Stable : sig
    module V1 : sig
      type t [@@deriving version, sexp, bin_io, compare, yojson, hash, equal]
    end

    module Latest = V1
  end

  include S with type t = Stable.Latest.t
end

module Make (F : Input_intf) :
  S_with_version
  with type Stable.V1.t = F.t
   and module Bigint = F.Bigint
   and module Vector = F.Vector = struct
  include F

  let size = size ()

  let size_in_bits = size_in_bits ()

  module Stable = struct
    module V1 = struct
      type t = F.t [@@deriving version {asserted}]

      include Binable.Of_binable
                (Bigint)
                (struct
                  type nonrec t = t

                  let to_binable = to_bigint

                  let of_binable = of_bigint
                end)

      include Sexpable.Of_sexpable
                (Bigint)
                (struct
                  type nonrec t = t

                  let to_sexpable = to_bigint

                  let of_sexpable = of_bigint
                end)

      let to_bignum_bigint n =
        let rec go i two_to_the_i acc =
          if Int.equal i size_in_bits then acc
          else
            let acc' =
              if Bigint.test_bit n i then Bignum_bigint.(acc + two_to_the_i)
              else acc
            in
            go (i + 1) Bignum_bigint.(two_to_the_i + two_to_the_i) acc'
        in
        go 0 Bignum_bigint.one Bignum_bigint.zero

      let hash_fold_t s x =
        Bignum_bigint.hash_fold_t s (to_bignum_bigint (to_bigint x))

      let hash = Hash.of_fold hash_fold_t

      let compare t1 t2 = Bigint.compare (to_bigint t1) (to_bigint t2)

      let equal = equal

      let to_yojson t : Yojson.Safe.t =
        `String (Bigint.to_hex_string (to_bigint t))

      let of_yojson j =
        match j with
        | `String h ->
            Ok (of_bigint (Bigint.of_hex_string h))
        | _ ->
            Error "expected hex string"
    end

    module Latest = V1
  end

  include (
    Stable.Latest : module type of Stable.Latest with type t := Stable.Latest.t )

  let domain_generator ~log2_size = domain_generator log2_size

  let one = of_int 1

  let zero = of_int 0

  (* TODO: Improve snarky interface so these aren't necessary.. *)
  let inv x = Option.value (inv x) ~default:zero

  let sqrt x = Option.value (sqrt x) ~default:zero

  let to_bits t =
    (* Avoids allocation *)
    let n = F.to_bigint t in
    List.init size_in_bits ~f:(Bigint.test_bit n)

  let of_bits bs =
    List.fold (List.rev bs) ~init:zero ~f:(fun acc b ->
        let acc = add acc acc in
        if b then add acc one else acc )

  let%test_unit "sexp round trip" =
    let t = random () in
    assert (equal t (t_of_sexp (sexp_of_t t)))

  let%test_unit "bin_io round trip" =
    let t = random () in
    [%test_eq: Stable.Latest.t] t
      (Binable.of_string
         (module Stable.Latest)
         (Binable.to_string (module Stable.Latest) t))

  let ( + ) = add

  let ( - ) = sub

  let ( * ) = mul

  let ( / ) = div

  module Mutable = struct
    let add = mut_add

    let mul = mut_mul

    let square = mut_square

    let sub = mut_sub

    let copy ~over t = F.copy over t
  end

  let op f t other = f t ~other

  let ( += ) = op Mutable.add

  let ( *= ) = op Mutable.mul

  let ( -= ) = op Mutable.sub

  let%test "of_bits to_bits" =
    let x = random () in
    equal x (of_bits (to_bits x))

  let%test_unit "to_bits of_bits" =
    Quickcheck.test
      (Quickcheck.Generator.list_with_length
         Int.(size_in_bits - 1)
         Bool.quickcheck_generator)
      ~f:(fun bs -> [%test_eq: bool list] (bs @ [false]) (to_bits (of_bits bs)))
end
