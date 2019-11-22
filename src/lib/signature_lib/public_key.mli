open Core
open Snark_params
open Tick
open Tuple_lib
open Fold_lib

type t = Field.t * Field.t [@@deriving sexp, hash]

include Codable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving bin_io, sexp, compare, eq, hash, yojson, version]
  end

  module Latest = V1
end

include Comparable.S_binable with type t := t

type var = Field.Var.t * Field.Var.t

val typ : (var, t) Typ.t

val var_of_t : t -> var

val assert_equal : var -> var -> (unit, 'a) Checked.t

val of_private_key_exn : Private_key.t -> t

module Compressed : sig
  module Poly : sig
    type ('field, 'boolean) t = {x: 'field; is_odd: 'boolean}

    module Stable :
      sig
        module V1 : sig
          type ('field, 'boolean) t
        end

        module Latest = V1
      end
      with type ('field, 'boolean) V1.t = ('field, 'boolean) t
  end

  type t = (Field.t, bool) Poly.t [@@deriving sexp, hash]

  include Codable.S with type t := t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, eq, compare, hash, version]

      include Codable.S with type t := t
    end

    module Latest = V1
  end

  val gen : t Quickcheck.Generator.t

  val empty : t

  val length_in_triples : int

  type var = (Field.Var.t, Boolean.var) Poly.t

  val typ : (var, t) Typ.t

  val var_of_t : t -> var

  include Comparable.S with type t := t

  include Hashable.S_binable with type t := t

  val fold : t -> bool Triple.t Fold.t

  val var_to_triples : var -> (Boolean.var Triple.t list, _) Checked.t

  val to_string : t -> string

  val to_base58_check : t -> string

  val of_base58_check_exn : string -> t

  val of_base58_check : string -> t Or_error.t

  module Checked : sig
    val equal : var -> var -> (Boolean.var, _) Checked.t

    val if_ : Boolean.var -> then_:var -> else_:var -> (var, _) Checked.t

    module Assert : sig
      val equal : var -> var -> (unit, _) Checked.t
    end
  end
end

val gen : t Quickcheck.Generator.t

val of_bigstring : Bigstring.t -> t Or_error.t

val to_bigstring : t -> Bigstring.t

val compress : t -> Compressed.t

val decompress : Compressed.t -> t option

val decompress_exn : Compressed.t -> t

val compress_var : var -> (Compressed.var, _) Checked.t

val decompress_var : Compressed.var -> (var, _) Checked.t

(* Note: this function will internally `compress` the key before transforming it *)
val to_base58_check : t -> string

val of_base58_check : string -> t Base.Or_error.t
