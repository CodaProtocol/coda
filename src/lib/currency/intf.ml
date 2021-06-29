[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_bits
open Snark_params.Tick

[%%else]

open Snark_bits_nonconsensus
module Random_oracle = Random_oracle_nonconsensus.Random_oracle
module Sgn = Sgn_nonconsensus.Sgn

[%%endif]

type uint64 = Unsigned.uint64

module type Basic = sig
  type t [@@deriving sexp, compare, hash, yojson]

  type magnitude = t [@@deriving sexp, compare]

  (* not automatically derived *)
  val dhall_type : Ppx_dhall_type.Dhall_type.t

  val max_int : t

  val length_in_bits : int

  include Comparable.S with type t := t

  val gen_incl : t -> t -> t Quickcheck.Generator.t

  val gen : t Quickcheck.Generator.t

  include Bits_intf.Convertible_bits with type t := t

  val to_input : t -> (_, bool) Random_oracle.Input.t

  val zero : t

  val one : t

  val of_string : string -> t

  val to_string : t -> string

  val of_formatted_string : string -> t

  val to_formatted_string : t -> string

  val of_int : int -> t

  val to_int : t -> int

  val to_uint64 : t -> uint64

  val of_uint64 : uint64 -> t

  [%%ifdef consensus_mechanism]

  type var

  val typ : (var, t) Typ.t

  val var_of_t : t -> var

  val var_to_number : var -> Number.t

  val var_to_bits : var -> Boolean.var Bitstring_lib.Bitstring.Lsb_first.t

  val var_to_input : var -> (_, Boolean.var) Random_oracle.Input.t

  val equal_var : var -> var -> (Boolean.var, _) Checked.t

  [%%endif]
end

module type Arithmetic_intf = sig
  type t

  val add : t -> t -> t option

  val sub : t -> t -> t option

  val ( + ) : t -> t -> t option

  val ( - ) : t -> t -> t option

  val scale : t -> int -> t option
end

module type Signed_intf = sig
  type magnitude

  [%%ifdef consensus_mechanism]

  type magnitude_var

  [%%endif]

  type t = (magnitude, Sgn.t) Signed_poly.t
  [@@deriving sexp, hash, compare, equal, yojson]

  val gen : t Quickcheck.Generator.t

  val create :
    magnitude:'magnitude -> sgn:'sgn -> ('magnitude, 'sgn) Signed_poly.t

  val sgn : t -> Sgn.t

  val magnitude : t -> magnitude

  val zero : t

  val to_input : t -> (_, bool) Random_oracle.Input.t

  val add : t -> t -> t option

  val ( + ) : t -> t -> t option

  val negate : t -> t

  val of_unsigned : magnitude -> t

  [%%ifdef consensus_mechanism]

  type var = (magnitude_var, Sgn.var) Signed_poly.t

  val typ : (var, t) Typ.t

  module Checked : sig
    val constant : t -> var

    val of_unsigned : magnitude_var -> var

    val negate : var -> var

    val if_ : Boolean.var -> then_:var -> else_:var -> (var, _) Checked.t

    val to_input : var -> (_, Boolean.var) Random_oracle.Input.t

    val add : var -> var -> (var, _) Checked.t

    val assert_equal : var -> var -> (unit, _) Checked.t

    val equal : var -> var -> (Boolean.var, _) Checked.t

    val ( + ) : var -> var -> (var, _) Checked.t

    val to_field_var : var -> (Field.Var.t, _) Checked.t

    val scale : Field.Var.t -> var -> (var, _) Checked.t

    val cswap :
         Boolean.var
      -> (magnitude_var, Sgn.t) Signed_poly.t
         * (magnitude_var, Sgn.t) Signed_poly.t
      -> (var * var, _) Checked.t

    type t = var
  end

  [%%endif]
end

[%%ifdef
consensus_mechanism]

module type Checked_arithmetic_intf = sig
  type value

  type var

  type t = var

  type signed_var

  val if_ : Boolean.var -> then_:var -> else_:var -> (var, _) Checked.t

  val if_value : Boolean.var -> then_:value -> else_:value -> var

  val add : var -> var -> (var, _) Checked.t

  val sub : var -> var -> (var, _) Checked.t

  val sub_flagged :
    var -> var -> (var * [`Underflow of Boolean.var], _) Checked.t

  val add_flagged :
    var -> var -> (var * [`Overflow of Boolean.var], _) Checked.t

  val ( + ) : var -> var -> (var, _) Checked.t

  val ( - ) : var -> var -> (var, _) Checked.t

  val add_signed : var -> signed_var -> (var, _) Checked.t

  val add_signed_flagged :
    var -> signed_var -> (var * [`Overflow of Boolean.var], _) Checked.t

  val assert_equal : var -> var -> (unit, _) Checked.t

  val equal : var -> var -> (Boolean.var, _) Checked.t

  val ( = ) : t -> t -> (Boolean.var, _) Checked.t

  val ( < ) : t -> t -> (Boolean.var, _) Checked.t

  val ( > ) : t -> t -> (Boolean.var, _) Checked.t

  val ( <= ) : t -> t -> (Boolean.var, _) Checked.t

  val ( >= ) : t -> t -> (Boolean.var, _) Checked.t

  val scale : Field.Var.t -> var -> (var, _) Checked.t
end

[%%endif]

module type S = sig
  include Basic

  include Arithmetic_intf with type t := t

  [%%ifdef consensus_mechanism]

  module Signed :
    Signed_intf with type magnitude := t and type magnitude_var := var

  module Checked :
    Checked_arithmetic_intf
    with type var := var
     and type signed_var := Signed.var
     and type value := t

  [%%else]

  module Signed : Signed_intf with type magnitude := t

  [%%endif]
end
