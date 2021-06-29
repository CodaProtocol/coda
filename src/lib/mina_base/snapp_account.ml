[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick
module Mina_numbers = Mina_numbers
module Hash_prefix_states = Hash_prefix_states

[%%else]

module Mina_numbers = Mina_numbers_nonconsensus.Mina_numbers
module Currency = Currency_nonconsensus.Currency
module Random_oracle = Random_oracle_nonconsensus.Random_oracle
module Hash_prefix_states = Hash_prefix_states_nonconsensus.Hash_prefix_states

[%%endif]

open Snapp_basic

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('app_state, 'vk) t = {app_state: 'app_state; verification_key: 'vk}
      [@@deriving sexp, equal, compare, hash, yojson, hlist, fields]
    end
  end]
end

type ('app_state, 'vk) t_ = ('app_state, 'vk) Poly.t =
  {app_state: 'app_state; verification_key: 'vk}

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      ( Snapp_state.Value.Stable.V1.t
      , ( Side_loaded_verification_key.Stable.V1.t
        , F.Stable.V1.t )
        With_hash.Stable.V1.t
        option )
      Poly.Stable.V1.t
    [@@deriving sexp, equal, compare, hash, yojson]

    let to_latest = Fn.id
  end
end]

open Pickles_types

let digest_vk (t : Side_loaded_verification_key.t) =
  Random_oracle.(
    hash ~init:Hash_prefix_states.side_loaded_vk
      (pack_input (Side_loaded_verification_key.to_input t)))

[%%ifdef
consensus_mechanism]

module Checked = struct
  type t =
    ( Pickles.Impls.Step.Field.t Snapp_state.V.t
    , ( Pickles.Side_loaded.Verification_key.Checked.t
      , Pickles.Impls.Step.Field.t Lazy.t )
      With_hash.t )
    Poly.t

  let to_input' (t : _ Poly.t) =
    let open Random_oracle.Input in
    let f mk acc field = mk (Core_kernel.Field.get field t) :: acc in
    let app_state v = Random_oracle.Input.field_elements (Vector.to_array v) in
    Poly.Fields.fold ~init:[] ~app_state:(f app_state)
      ~verification_key:(f (fun x -> field x))
    |> List.reduce_exn ~f:append

  let to_input (t : t) =
    to_input' {t with verification_key= Lazy.force t.verification_key.hash}

  let digest_vk t =
    Random_oracle.Checked.(
      hash ~init:Hash_prefix_states.side_loaded_vk
        (pack_input (Pickles.Side_loaded.Verification_key.Checked.to_input t)))

  let digest t =
    Random_oracle.Checked.(
      hash ~init:Hash_prefix_states.snapp_account (pack_input (to_input t)))

  let digest' t =
    Random_oracle.Checked.(
      hash ~init:Hash_prefix_states.snapp_account (pack_input (to_input' t)))
end

let typ : (Checked.t, t) Typ.t =
  let open Poly in
  Typ.of_hlistable
    [ Snapp_state.typ Field.typ
    ; Typ.transport Pickles.Side_loaded.Verification_key.typ
        ~there:(function
          | None ->
              Pickles.Side_loaded.Verification_key.dummy
          | Some x ->
              With_hash.data x )
        ~back:(fun x -> Some (With_hash.of_data x ~hash_data:digest_vk))
      |> Typ.transport_var ~there:With_hash.data
           ~back:
             (With_hash.of_data ~hash_data:(fun x -> lazy (Checked.digest_vk x)))
    ]
    ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
    ~value_of_hlist:of_hlist

[%%endif]

let dummy_vk_hash =
  Memo.unit (fun () -> digest_vk Side_loaded_verification_key.dummy)

let to_input (t : t) =
  let open Random_oracle.Input in
  let f mk acc field = mk (Core_kernel.Field.get field t) :: acc in
  let app_state v = Random_oracle.Input.field_elements (Vector.to_array v) in
  Poly.Fields.fold ~init:[] ~app_state:(f app_state)
    ~verification_key:
      (f
         (Fn.compose field
            (Option.value_map ~default:(dummy_vk_hash ()) ~f:With_hash.hash)))
  |> List.reduce_exn ~f:append

let default : _ Poly.t =
  (* These are the permissions of a "user"/"non snapp" account. *)
  { app_state= Vector.init Snapp_state.Max_state_size.n ~f:(fun _ -> F.zero)
  ; verification_key= None }

let digest (t : t) =
  Random_oracle.(
    hash ~init:Hash_prefix_states.snapp_account (pack_input (to_input t)))

let default_digest = lazy (digest default)
