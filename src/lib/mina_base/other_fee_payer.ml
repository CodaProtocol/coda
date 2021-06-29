[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Signature_lib

[%%else]

open Signature_lib_nonconsensus

[%%endif]

module Impl = Pickles.Impls.Step

module Payload = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('pk, 'token_id, 'nonce, 'fee) t =
          {pk: 'pk; token_id: 'token_id; nonce: 'nonce; fee: 'fee}
        [@@deriving hlist, sexp, equal, yojson, hash, compare]
      end
    end]
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Public_key.Compressed.Stable.V1.t
        , Token_id.Stable.V1.t
        , Mina_numbers.Account_nonce.Stable.V1.t
        , Currency.Fee.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, equal, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  module Checked = struct
    type t =
      ( Public_key.Compressed.var
      , Token_id.Checked.t
      , Mina_numbers.Account_nonce.Checked.t
      , Currency.Fee.Checked.t )
      Poly.t

    let to_input ({pk; token_id; nonce; fee} : t) =
      let ( ! ) = Impl.run_checked in
      let open Random_oracle_input in
      List.reduce_exn ~f:append
        [ Public_key.Compressed.Checked.to_input pk
        ; !(Token_id.Checked.to_input token_id)
        ; !(Mina_numbers.Account_nonce.Checked.to_input nonce)
        ; Currency.Fee.var_to_input fee ]
  end

  open Pickles.Impls.Step

  let typ : (Checked.t, t) Typ.t =
    let open Poly in
    Typ.of_hlistable
      [ Public_key.Compressed.typ
      ; Token_id.typ
      ; Mina_numbers.Account_nonce.typ
      ; Currency.Fee.typ ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  let dummy : t =
    { pk= Public_key.Compressed.empty
    ; token_id= Token_id.invalid
    ; nonce= Mina_numbers.Account_nonce.zero
    ; fee= Currency.Fee.zero }

  let to_input ({pk; token_id; nonce; fee} : t) =
    let open Random_oracle_input in
    List.reduce_exn ~f:append
      [ Public_key.Compressed.to_input pk
      ; Token_id.to_input token_id
      ; Mina_numbers.Account_nonce.to_input nonce
      ; Currency.Fee.to_input fee ]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t = {payload: Payload.Stable.V1.t; signature: Signature.Stable.V1.t}
    [@@deriving sexp, equal, yojson, hash, compare]

    let to_latest = Fn.id
  end
end]
