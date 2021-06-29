module type Unchecked = sig
  type scalar

  type t [@@deriving equal, bin_io]

  val ( * ) : scalar -> t -> t

  val ( + ) : t -> t -> t
end

module type Checked = sig
  module Impl : Snarky_backendless.Snark_intf.S

  open Impl

  type scalar

  type t

  val equal : t -> t -> (Boolean.var, _) Checked.t

  module Assert : sig
    val equal : t -> t -> (unit, _) Checked.t
  end

  val ( * ) : scalar -> t -> (t, _) Checked.t

  val ( + ) : t -> t -> (t, _) Checked.t
end

module type S = sig
  module Impl : Snarky_backendless.Snark_intf.S

  module Scalar : Scalar_intf.S with module Impl := Impl

  module Unchecked : Unchecked with type scalar := Scalar.Unchecked.t

  module Checked :
    Checked with module Impl := Impl and type scalar := Scalar.Checked.t

  val typ : (Checked.t, Unchecked.t) Impl.Typ.t
end
