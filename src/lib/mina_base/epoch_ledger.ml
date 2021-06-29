open Core_kernel
open Currency
open Snark_params.Tick
open Bitstring_lib

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('ledger_hash, 'amount) t =
        {hash: 'ledger_hash; total_currency: 'amount}
      [@@deriving sexp, equal, compare, hash, yojson, hlist]
    end
  end]
end

module Value = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        (Frozen_ledger_hash0.Stable.V1.t, Amount.Stable.V1.t) Poly.Stable.V1.t
      [@@deriving sexp, equal, compare, hash, yojson]

      let to_latest = Fn.id
    end
  end]
end

let to_input ({hash; total_currency} : Value.t) =
  let open Snark_params.Tick in
  { Random_oracle.Input.field_elements= [|(hash :> Field.t)|]
  ; bitstrings= [|Amount.to_bits total_currency|] }

type var = (Frozen_ledger_hash0.var, Amount.var) Poly.t

let data_spec = Data_spec.[Frozen_ledger_hash0.typ; Amount.typ]

let typ : (var, Value.t) Typ.t =
  Typ.of_hlistable data_spec ~var_to_hlist:Poly.to_hlist
    ~var_of_hlist:Poly.of_hlist ~value_to_hlist:Poly.to_hlist
    ~value_of_hlist:Poly.of_hlist

let var_to_input ({Poly.hash; total_currency} : var) =
  { Random_oracle.Input.field_elements=
      [|Frozen_ledger_hash0.var_to_hash_packed hash|]
  ; bitstrings=
      [|Bitstring.Lsb_first.to_list (Amount.var_to_bits total_currency)|] }

let if_ cond ~(then_ : (Frozen_ledger_hash0.var, Amount.var) Poly.t)
    ~(else_ : (Frozen_ledger_hash0.var, Amount.var) Poly.t) =
  let open Checked.Let_syntax in
  let%map hash =
    Frozen_ledger_hash0.if_ cond ~then_:then_.hash ~else_:else_.hash
  and total_currency =
    Amount.Checked.if_ cond ~then_:then_.total_currency
      ~else_:else_.total_currency
  in
  {Poly.hash; total_currency}
