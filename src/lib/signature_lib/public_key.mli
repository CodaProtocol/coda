[%%import "/src/config.mlh"]

open Core_kernel

[%%ifdef consensus_mechanism]

open Snark_params
open Tick

[%%else]

open Snark_params_nonconsensus
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

type t = Field.t * Field.t [@@deriving sexp, hash]

include Codable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving bin_io, sexp, compare, equal, hash, yojson, version]
  end

  module Latest = V1
end

include Comparable.S_binable with type t := t

[%%ifdef consensus_mechanism]

type var = Field.Var.t * Field.Var.t

val typ : (var, t) Typ.t

val var_of_t : t -> var

val assert_equal : var -> var -> (unit, 'a) Checked.t

[%%endif]

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
      type nonrec t = t
      [@@deriving sexp, bin_io, equal, compare, hash, version]

      include Codable.S with type t := t
    end

    module Latest = V1
  end

  val gen : t Quickcheck.Generator.t

  val empty : t

  include Comparable.S with type t := t

  include Hashable.S_binable with type t := t

  val to_input : t -> (Field.t, bool) Random_oracle.Input.t

  val to_string : t -> string

  val to_base58_check : t -> string

  val of_base58_check_exn : string -> t

  val of_base58_check : string -> t Or_error.t

  [%%ifdef consensus_mechanism]

  type var = (Field.Var.t, Boolean.var) Poly.t

  val typ : (var, t) Typ.t

  val var_of_t : t -> var

  module Checked : sig
    val equal : var -> var -> (Boolean.var, _) Checked.t

    val to_input : var -> (Field.Var.t, Boolean.var) Random_oracle.Input.t

    val if_ : Boolean.var -> then_:var -> else_:var -> (var, _) Checked.t

    module Assert : sig
      val equal : var -> var -> (unit, _) Checked.t
    end
  end

  [%%endif]
end

val gen : t Quickcheck.Generator.t

val of_bigstring : Bigstring.t -> t Or_error.t

val to_bigstring : t -> Bigstring.t

val compress : t -> Compressed.t

val decompress : Compressed.t -> t option

val decompress_exn : Compressed.t -> t

(** Same as [Compressed.of_base58_check_exn] except that [of_base58_check_decompress_exn] fails if [decompress_exn] fails *)
val of_base58_check_decompress_exn : string -> Compressed.t

[%%ifdef consensus_mechanism]

val compress_var : var -> (Compressed.var, _) Checked.t

val decompress_var : Compressed.var -> (var, _) Checked.t

[%%endif]
