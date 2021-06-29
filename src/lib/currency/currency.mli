[%%import "/src/config.mlh"]

open Core_kernel
open Intf

[%%ifdef consensus_mechanism]

open Snark_params.Tick

[%%endif]

type uint64 = Unsigned.uint64

module Signed_poly = Signed_poly

module Fee : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t [@@deriving sexp, compare, hash, yojson, equal]

      (* not automatically derived *)
      val dhall_type : Ppx_dhall_type.Dhall_type.t
    end
  end]

  include Basic with type t := Stable.Latest.t

  include Arithmetic_intf with type t := t

  include Codable.S with type t := t

  (* TODO: Get rid of signed fee, use signed amount *)
  [%%ifdef consensus_mechanism]

  module Signed :
    Signed_intf with type magnitude := t and type magnitude_var := var

  [%%else]

  module Signed : Signed_intf with type magnitude := t

  [%%endif]

  [%%ifdef consensus_mechanism]

  module Checked : sig
    include
      Checked_arithmetic_intf
      with type var := var
       and type signed_var := Signed.var
       and type value := t

    val add_signed : var -> Signed.var -> (var, _) Checked.t
  end

  [%%endif]
end

module Amount : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t [@@deriving sexp, compare, hash, equal, yojson]

      (* not automatically derived *)
      val dhall_type : Ppx_dhall_type.Dhall_type.t
    end
  end]

  include Basic with type t := Stable.Latest.t

  include Arithmetic_intf with type t := t

  include Codable.S with type t := t

  [%%ifdef consensus_mechanism]

  module Signed :
    Signed_intf with type magnitude := t and type magnitude_var := var

  [%%else]

  module Signed : Signed_intf with type magnitude := t

  [%%endif]

  (* TODO: Delete these functions *)

  val of_fee : Fee.t -> t

  val to_fee : t -> Fee.t

  val add_fee : t -> Fee.t -> t option

  [%%ifdef consensus_mechanism]

  module Checked : sig
    include
      Checked_arithmetic_intf
      with type var := var
       and type signed_var := Signed.var
       and type value := t

    val add_signed : var -> Signed.var -> (var, _) Checked.t

    val of_fee : Fee.var -> var

    val to_fee : var -> Fee.var

    val add_fee : var -> Fee.var -> (var, _) Checked.t
  end

  [%%endif]
end

module Balance : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t [@@deriving sexp, compare, hash, yojson, equal]

      (* not automatically derived *)
      val dhall_type : Ppx_dhall_type.Dhall_type.t
    end
  end]

  include Basic with type t := Stable.Latest.t

  val to_amount : t -> Amount.t

  val add_amount : t -> Amount.t -> t option

  val sub_amount : t -> Amount.t -> t option

  val ( + ) : t -> Amount.t -> t option

  val ( - ) : t -> Amount.t -> t option

  [%%ifdef consensus_mechanism]

  module Checked : sig
    type t = var

    val add_signed_amount : var -> Amount.Signed.var -> (var, _) Checked.t

    val add_amount : var -> Amount.var -> (var, _) Checked.t

    val sub_amount : var -> Amount.var -> (var, _) Checked.t

    val sub_amount_flagged :
      var -> Amount.var -> (var * [`Underflow of Boolean.var], _) Checked.t

    val add_amount_flagged :
      var -> Amount.var -> (var * [`Overflow of Boolean.var], _) Checked.t

    val add_signed_amount_flagged :
         var
      -> Amount.Signed.var
      -> (var * [`Overflow of Boolean.var], _) Checked.t

    val ( + ) : var -> Amount.var -> (var, _) Checked.t

    val ( - ) : var -> Amount.var -> (var, _) Checked.t

    val equal : var -> var -> (Boolean.var, _) Checked.t

    val ( = ) : var -> var -> (Boolean.var, _) Checked.t

    val ( < ) : var -> var -> (Boolean.var, _) Checked.t

    val ( > ) : var -> var -> (Boolean.var, _) Checked.t

    val ( <= ) : var -> var -> (Boolean.var, _) Checked.t

    val ( >= ) : var -> var -> (Boolean.var, _) Checked.t

    val if_ : Boolean.var -> then_:var -> else_:var -> (var, _) Checked.t
  end

  [%%endif]
end
