[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick

[%%else]

open Snark_params_nonconsensus
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

module T = Mina_numbers.Length

(*constants actually required for blockchain snark*)
(* k
  ,c
  ,slots_per_epoch
  ,slots_per_sub_window
  ,sub_windows_per_window
  ,checkpoint_window_size_in_slots
  ,block_window_duration_ms*)

module Poly = Genesis_constants.Protocol.Poly

module Value = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        (T.Stable.V1.t, T.Stable.V1.t, Block_time.Stable.V1.t) Poly.Stable.V1.t
      [@@deriving equal, ord, hash, sexp, yojson]

      let to_latest = Fn.id
    end
  end]

  let gen : t Quickcheck.Generator.t =
    let open Quickcheck.Let_syntax in
    let%bind k = Int.gen_incl 1 5000 in
    let%bind delta = Int.gen_incl 0 5000 in
    let%bind slots_per_epoch = Int.gen_incl k (8 * k) >>| ( * ) 3 >>| T.of_int
    and slots_per_sub_window = Int.gen_incl 1 ((k + 9) / 9) in
    (*TODO: Bug -> Block_time.(to_time x |> of_time) != x for certain values.
    Eg: 34702788243129 <--> 34702788243128, 8094 <--> 8093*)
    let%bind ms = Int64.(gen_log_uniform_incl 0L 9999999999999L) in
    let end_time = Block_time.of_int64 999999999999999L in
    let%map genesis_state_timestamp =
      Block_time.(gen_incl (of_int64 ms) end_time)
    in
    { Poly.k= T.of_int k
    ; delta= T.of_int delta
    ; slots_per_epoch
    ; slots_per_sub_window= T.of_int slots_per_sub_window
    ; genesis_state_timestamp }
end

type value = Value.t

let value_of_t (t : Genesis_constants.Protocol.t) : value =
  { k= T.of_int t.k
  ; delta= T.of_int t.delta
  ; slots_per_epoch= T.of_int t.slots_per_epoch
  ; slots_per_sub_window= T.of_int t.slots_per_sub_window
  ; genesis_state_timestamp= Block_time.of_int64 t.genesis_state_timestamp }

let t_of_value (v : value) : Genesis_constants.Protocol.t =
  { k= T.to_int v.k
  ; delta= T.to_int v.delta
  ; slots_per_epoch= T.to_int v.slots_per_epoch
  ; slots_per_sub_window= T.to_int v.slots_per_sub_window
  ; genesis_state_timestamp= Block_time.to_int64 v.genesis_state_timestamp }

let to_input (t : value) =
  Random_oracle.Input.bitstrings
    [| T.to_bits t.k
     ; T.to_bits t.delta
     ; T.to_bits t.slots_per_epoch
     ; T.to_bits t.slots_per_sub_window
     ; Block_time.Bits.to_bits t.genesis_state_timestamp |]

[%%if
defined consensus_mechanism]

type var = (T.Checked.t, T.Checked.t, Block_time.Unpacked.var) Poly.t

let data_spec =
  Data_spec.
    [ T.Checked.typ
    ; T.Checked.typ
    ; T.Checked.typ
    ; T.Checked.typ
    ; Block_time.Unpacked.typ ]

let typ =
  Typ.of_hlistable data_spec ~var_to_hlist:Poly.to_hlist
    ~var_of_hlist:Poly.of_hlist ~value_to_hlist:Poly.to_hlist
    ~value_of_hlist:Poly.of_hlist

let var_to_input (var : var) =
  let s = Bitstring_lib.Bitstring.Lsb_first.to_list in
  let%map k = T.Checked.to_bits var.k
  and delta = T.Checked.to_bits var.delta
  and slots_per_epoch = T.Checked.to_bits var.slots_per_epoch
  and slots_per_sub_window = T.Checked.to_bits var.slots_per_sub_window in
  let genesis_state_timestamp =
    Block_time.Unpacked.var_to_bits var.genesis_state_timestamp
  in
  Random_oracle.Input.bitstrings
    (Array.map ~f:s
       [| k
        ; delta
        ; slots_per_epoch
        ; slots_per_sub_window
        ; genesis_state_timestamp |])

let%test_unit "value = var" =
  let compiled = Genesis_constants.for_unit_tests.protocol in
  let test protocol_constants =
    let open Snarky_backendless in
    let p_var =
      let%map p = exists typ ~compute:(As_prover.return protocol_constants) in
      As_prover.read typ p
    in
    let _, res = Or_error.ok_exn (run_and_check p_var ()) in
    [%test_eq: Value.t] res protocol_constants ;
    [%test_eq: Value.t] protocol_constants
      (t_of_value protocol_constants |> value_of_t)
  in
  Quickcheck.test ~trials:100 Value.gen ~examples:[value_of_t compiled] ~f:test

[%%endif]
