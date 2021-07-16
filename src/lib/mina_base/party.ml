[%%import "/src/config.mlh"]

open Core_kernel

[%%ifdef consensus_mechanism]

open Snark_params.Tick
open Signature_lib
module Mina_numbers = Mina_numbers

[%%else]

open Signature_lib_nonconsensus
module Mina_numbers = Mina_numbers_nonconsensus.Mina_numbers
module Currency = Currency_nonconsensus.Currency
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

module Impl = Pickles.Impls.Step
open Mina_numbers
open Currency
open Pickles_types
module Digest = Random_oracle.Digest

module type Type = sig
  type t
end

module Update = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('state_element, 'pk, 'vk, 'perms) t =
          { app_state : 'state_element Snapp_state.V.Stable.V1.t
          ; delegate : 'pk
          ; verification_key : 'vk
          ; permissions : 'perms
          }
        [@@deriving compare, equal, sexp, hash, yojson, hlist]
      end
    end]
  end

  open Snapp_basic

  [%%versioned
  module Stable = struct
    module V1 = struct
      (* TODO: Have to check that the public key is not = Public_key.Compressed.empty here.  *)
      type t =
        ( F.Stable.V1.t Set_or_keep.Stable.V1.t
        , Public_key.Compressed.Stable.V1.t Set_or_keep.Stable.V1.t
        , ( Pickles.Side_loaded.Verification_key.Stable.V1.t
          , F.Stable.V1.t )
          With_hash.Stable.V1.t
          Set_or_keep.Stable.V1.t
        , Permissions.Stable.V1.t Set_or_keep.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving compare, equal, sexp, hash, yojson]

      let to_latest = Fn.id
    end
  end]

  module Checked = struct
    open Pickles.Impls.Step

    type t =
      ( Field.t Set_or_keep.Checked.t
      , Public_key.Compressed.var Set_or_keep.Checked.t
      , Field.t Set_or_keep.Checked.t
      , Permissions.Checked.t Set_or_keep.Checked.t )
      Poly.t

    let to_input ({ app_state; delegate; verification_key; permissions } : t) =
      let open Random_oracle_input in
      List.reduce_exn ~f:append
        [ Snapp_state.to_input app_state
            ~f:(Set_or_keep.Checked.to_input ~f:field)
        ; Set_or_keep.Checked.to_input delegate
            ~f:Public_key.Compressed.Checked.to_input
        ; Set_or_keep.Checked.to_input verification_key ~f:field
        ; Set_or_keep.Checked.to_input permissions
            ~f:Permissions.Checked.to_input
        ]
  end

  let noop : t =
    { app_state =
        Vector.init Snapp_state.Max_state_size.n ~f:(fun _ -> Set_or_keep.Keep)
    ; delegate = Keep
    ; verification_key = Keep
    ; permissions = Keep
    }

  let dummy = noop

  let to_input ({ app_state; delegate; verification_key; permissions } : t) =
    let open Random_oracle_input in
    List.reduce_exn ~f:append
      [ Snapp_state.to_input app_state
          ~f:(Set_or_keep.to_input ~dummy:Field.zero ~f:field)
      ; Set_or_keep.to_input delegate
          ~dummy:(Snapp_predicate.Eq_data.Tc.public_key ()).default
          ~f:Public_key.Compressed.to_input
      ; Set_or_keep.to_input
          (Set_or_keep.map verification_key ~f:With_hash.hash)
          ~dummy:Field.zero ~f:field
      ; Set_or_keep.to_input permissions ~dummy:Permissions.user_default
          ~f:Permissions.to_input
      ]

  let typ () : (Checked.t, t) Typ.t =
    let open Poly in
    let open Pickles.Impls.Step in
    Typ.of_hlistable
      [ Snapp_state.typ (Set_or_keep.typ ~dummy:Field.Constant.zero Field.typ)
      ; Set_or_keep.typ ~dummy:Public_key.Compressed.empty
          Public_key.Compressed.typ
      ; Set_or_keep.typ ~dummy:Field.Constant.zero Field.typ
        |> Typ.transport
             ~there:(Set_or_keep.map ~f:With_hash.hash)
             ~back:(Set_or_keep.map ~f:(fun _ -> failwith "vk typ"))
      ; Set_or_keep.typ ~dummy:Permissions.user_default Permissions.typ
      ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist
end

module Body = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('pk, 'update, 'token_id, 'signed_amount) t =
          { pk : 'pk
          ; update : 'update
          ; token_id : 'token_id
          ; delta : 'signed_amount
          }
        [@@deriving hlist, sexp, equal, yojson, hash, compare]
      end
    end]
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Public_key.Compressed.Stable.V1.t
        , Update.Stable.V1.t
        , Token_id.Stable.V1.t
        , (Amount.Stable.V1.t, Sgn.Stable.V1.t) Signed_poly.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, equal, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  module Checked = struct
    type t =
      ( Public_key.Compressed.var
      , Update.Checked.t
      , Token_id.Checked.t
      , Amount.Signed.var )
      Poly.t

    let to_input ({ pk; update; token_id; delta } : t) =
      List.reduce_exn ~f:Random_oracle_input.append
        [ Public_key.Compressed.Checked.to_input pk
        ; Update.Checked.to_input update
        ; Impl.run_checked (Token_id.Checked.to_input token_id)
        ; Amount.Signed.Checked.to_input delta
        ]

    let digest (t : t) =
      Random_oracle.Checked.(
        hash ~init:Hash_prefix.snapp_body (pack_input (to_input t)))
  end

  let typ () : (Checked.t, t) Typ.t =
    let open Poly in
    Typ.of_hlistable
      [ Public_key.Compressed.typ
      ; Update.typ ()
      ; Token_id.typ
      ; Amount.Signed.typ
      ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  let dummy : t =
    { pk = Public_key.Compressed.empty
    ; update = Update.dummy
    ; token_id = Token_id.default
    ; delta = Amount.Signed.zero
    }

  let to_input ({ pk; update; token_id; delta } : t) =
    List.reduce_exn ~f:Random_oracle_input.append
      [ Public_key.Compressed.to_input pk
      ; Update.to_input update
      ; Token_id.to_input token_id
      ; Amount.Signed.to_input delta
      ]

  let digest (t : t) =
    Random_oracle.(hash ~init:Hash_prefix.snapp_body (pack_input (to_input t)))

  module Digested = struct
    type t = Random_oracle.Digest.t

    module Checked = struct
      type t = Random_oracle.Checked.Digest.t
    end
  end
end

module Predicate = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        | Full of Snapp_predicate.Account.Stable.V1.t
        | Nonce of Account.Nonce.Stable.V1.t
        | Accept
      [@@deriving sexp, equal, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  let accept = lazy Random_oracle.(digest (salt "MinaPartyAccept"))

  let digest (t : t) =
    let digest x =
      Random_oracle.(
        hash ~init:Hash_prefix_states.party_predicate (pack_input x))
    in
    match t with
    | Full a ->
        Snapp_predicate.Account.to_input a |> digest
    | Nonce n ->
        Account.Nonce.to_input n |> digest
    | Accept ->
        Lazy.force accept

  module Checked = struct
    type t =
      | Nonce_or_accept of
          { nonce : Account.Nonce.Checked.t; accept : Boolean.var }
      | Full of Snapp_predicate.Account.Checked.t

    let digest (t : t) =
      let digest x =
        Random_oracle.Checked.(
          hash ~init:Hash_prefix_states.party_predicate (pack_input x))
      in
      match t with
      | Full a ->
          Snapp_predicate.Account.Checked.to_input a |> digest
      | Nonce_or_accept { nonce; accept = b } ->
          let open Impl in
          Field.(
            if_ b
              ~then_:(constant (Lazy.force accept))
              ~else_:
                (digest (run_checked (Account.Nonce.Checked.to_input nonce))))
  end

  let typ () : (Snapp_predicate.Account.Checked.t, t) Typ.t =
    Typ.transport
      (Snapp_predicate.Account.typ ())
      ~there:(function
        | Full s ->
            s
        | Nonce n ->
            { Snapp_predicate.Account.accept with
              nonce = Check { lower = n; upper = n }
            }
        | Accept ->
            Snapp_predicate.Account.accept)
      ~back:(fun s -> Full s)
end

module Predicated = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('body, 'predicate) t = { body : 'body; predicate : 'predicate }
        [@@deriving hlist, sexp, equal, yojson, hash, compare]
      end
    end]
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = (Body.Stable.V1.t, Predicate.Stable.V1.t) Poly.Stable.V1.t
      [@@deriving sexp, equal, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  let to_input ({ body; predicate } : t) =
    List.reduce_exn ~f:Random_oracle_input.append
      [ Body.to_input body
      ; Random_oracle_input.field (Predicate.digest predicate)
      ]

  let digest (t : t) =
    Random_oracle.(hash ~init:Hash_prefix.party (pack_input (to_input t)))

  let typ () : (_, t) Typ.t =
    let open Poly in
    Typ.of_hlistable
      [ Body.typ (); Predicate.typ () ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  module Checked = struct
    type t = (Body.Checked.t, Predicate.Checked.t) Poly.t

    let to_input ({ body; predicate } : t) =
      List.reduce_exn ~f:Random_oracle_input.append
        [ Body.Checked.to_input body
        ; Random_oracle_input.field (Predicate.Checked.digest predicate)
        ]

    let digest (t : t) =
      Random_oracle.Checked.(
        hash ~init:Hash_prefix.party (pack_input (to_input t)))
  end

  module Proved = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          ( Body.Stable.V1.t
          , Snapp_predicate.Account.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving sexp, equal, yojson, hash, compare]

        let to_latest = Fn.id
      end
    end]

    module Digested = struct
      type t = (Body.Digested.t, Snapp_predicate.Digested.t) Poly.t

      module Checked = struct
        type t = (Body.Digested.Checked.t, Field.Var.t) Poly.t
      end
    end

    module Checked = struct
      type t = (Body.Checked.t, Snapp_predicate.Account.Checked.t) Poly.t
    end
  end

  module Signed = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = (Body.Stable.V1.t, Account_nonce.Stable.V1.t) Poly.Stable.V1.t
        [@@deriving sexp, equal, yojson, hash, compare]

        let to_latest = Fn.id
      end
    end]

    module Checked = struct
      type t = (Body.Checked.t, Account_nonce.Checked.t) Poly.t
    end

    let dummy : t = { body = Body.dummy; predicate = Account_nonce.zero }
  end

  module Empty = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = (Body.Stable.V1.t, unit) Poly.Stable.V1.t
        [@@deriving sexp, equal, yojson, hash, compare]

        let to_latest = Fn.id
      end
    end]

    let dummy : t = { body = Body.dummy; predicate = () }

    let create body : t = { body; predicate = () }
  end

  let of_signed ({ body; predicate } : Signed.t) : t =
    { body; predicate = Nonce predicate }
end

module Poly (Data : Type) (Auth : Type) = struct
  type t = { data : Data.t; authorization : Auth.t }
end

module Proved = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
            Poly(Predicated.Proved.Stable.V1)
              (Pickles.Side_loaded.Proof.Stable.V1)
            .t =
        { data : Predicated.Proved.Stable.V1.t
        ; authorization : Pickles.Side_loaded.Proof.Stable.V1.t
        }
      [@@deriving sexp, equal, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]
end

module Signed = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Poly(Predicated.Signed.Stable.V1)(Signature.Stable.V1).t =
        { data : Predicated.Signed.Stable.V1.t
        ; authorization : Signature.Stable.V1.t
        }
      [@@deriving sexp, equal, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  let account_id (t : t) : Account_id.t =
    Account_id.create t.data.body.pk t.data.body.token_id
end

module Empty = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Poly(Predicated.Empty.Stable.V1)(Unit.Stable.V1).t =
        { data : Predicated.Empty.Stable.V1.t; authorization : unit }
      [@@deriving sexp, equal, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t = Poly(Predicated.Stable.V1)(Control.Stable.V1).t =
      { data : Predicated.Stable.V1.t; authorization : Control.Stable.V1.t }
    [@@deriving sexp, equal, yojson, hash, compare]

    let to_latest = Fn.id
  end
end]

let account_id (t : t) : Account_id.t =
  Account_id.create t.data.body.pk t.data.body.token_id

let of_signed ({ data; authorization } : Signed.t) : t =
  { authorization = Signature authorization; data = Predicated.of_signed data }
