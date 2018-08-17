open Core_kernel
open Coda_numbers
open Snark_params
open Tick

type ('ledger_builder_hash, 'ledger_hash, 'time) t_ =
  { ledger_builder_hash: 'ledger_builder_hash
  ; ledger_hash: 'ledger_hash
  ; timestamp: 'time }
[@@deriving sexp, eq, compare, fields]

type t = (Ledger_builder_hash.t, Ledger_hash.t, Block_time.t) t_
[@@deriving sexp, eq, compare, hash]

module Stable : sig
  module V1 : sig
    type nonrec ('a, 'b, 'c) t_ = ('a, 'b, 'c) t_ =
      {ledger_builder_hash: 'a; ledger_hash: 'b; timestamp: 'c}
    [@@deriving bin_io, sexp, eq, compare, hash]

    type nonrec t =
      ( Ledger_builder_hash.Stable.V1.t
      , Ledger_hash.Stable.V1.t
      , Block_time.Stable.V1.t )
      t_
    [@@deriving bin_io, sexp, eq, compare, hash]
  end
end

type value = t [@@deriving bin_io, sexp, eq, compare, hash]

include Snarkable.S
        with type var =
                    ( Ledger_builder_hash.var
                    , Ledger_hash.var
                    , Block_time.Unpacked.var )
                    t_
         and type value := value

module Hash = State_hash

val create_value :
     ledger_builder_hash:Ledger_builder_hash.Stable.V1.t
  -> ledger_hash:Ledger_hash.Stable.V1.t
  -> timestamp:Block_time.Stable.V1.t
  -> value

val bit_length : int

val genesis : t

val set_timestamp : ('a, 'b, 'c) t_ -> 'c -> ('a, 'b, 'c) t_

val fold : t -> init:'acc -> f:('acc -> bool -> 'acc) -> 'acc

val var_to_bits : var -> (Boolean.var list, _) Checked.t

val to_bits : t -> bool list

module Message :
  Snarky.Signature.Message_intf
  with type ('a, 'b) checked := ('a, 'b) Tick.Checked.t
   and type boolean_var := Tick.Boolean.var
   and type curve_scalar_var := Snark_params.Tick.Signature_curve.Scalar.var
   and type t = t
   and type var = var

module Signature :
  Snarky.Signature.S
  with type ('a, 'b) typ := ('a, 'b) Tick.Typ.t
   and type ('a, 'b) checked := ('a, 'b) Tick.Checked.t
   and type boolean_var := Tick.Boolean.var
   and type curve := Snark_params.Tick.Signature_curve.value
   and type curve_var := Snark_params.Tick.Signature_curve.var
   and type curve_scalar := Snark_params.Tick.Signature_curve.Scalar.value
   and type curve_scalar_var := Snark_params.Tick.Signature_curve.Scalar.var
   and module Message := Message
