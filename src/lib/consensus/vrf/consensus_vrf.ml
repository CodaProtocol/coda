open Core_kernel
open Fold_lib
open Snark_params

module Scalar = struct
  type t = Tick.Inner_curve.Scalar.t

  type value = t

  type var = Tick.Inner_curve.Scalar.var

  let to_string = Tick.Inner_curve.Scalar.to_string

  let of_string = Tick.Inner_curve.Scalar.of_string

  let to_yojson t = `String (to_string t)

  let of_yojson yojson =
    match yojson with
    | `String x ->
        Or_error.try_with (fun () -> of_string x)
        |> Result.map_error ~f:Error.to_string_hum
    | _ ->
        Error "Consensus_vrf.of_yojson: Expected a string"

  let typ : (var, value) Tick.Typ.t = Tick.Inner_curve.Scalar.typ
end

module Group = struct
  open Tick

  type t = Inner_curve.t [@@deriving sexp]

  let to_yojson (t : t) = Inner_curve.(Affine.to_yojson (to_affine_exn t))

  let of_yojson json =
    Result.map ~f:Inner_curve.of_affine (Inner_curve.Affine.of_yojson json)

  let to_string_list_exn (t : t) =
    let x, y = Inner_curve.to_affine_exn t in
    [Field.to_string x; Field.to_string y]

  let of_string_list_exn = function
    | [x; y] ->
        Inner_curve.of_affine (Field.of_string x, Field.of_string y)
    | _ ->
        invalid_arg
          "Consensus_vrf.Group.of_string_list_exn: wrong number of field \
           elements given, expected 2"

  type value = Inner_curve.t

  type var = Inner_curve.var

  let scale = Inner_curve.scale

  let typ = Inner_curve.typ

  let generator = Inner_curve.one

  let add = Inner_curve.add

  let negate = Inner_curve.negate

  let to_affine_exn = Inner_curve.to_affine_exn

  let of_affine = Inner_curve.of_affine

  module Checked = struct
    include Inner_curve.Checked

    let scale_generator shifted s ~init =
      scale_known shifted Inner_curve.one s ~init
  end
end

module Message = struct
  module Global_slot = Mina_numbers.Global_slot

  type ('global_slot, 'epoch_seed, 'delegator) t =
    {global_slot: 'global_slot; seed: 'epoch_seed; delegator: 'delegator}
  [@@deriving sexp, hlist]

  type value =
    (Global_slot.t, Mina_base.Epoch_seed.t, Mina_base.Account.Index.t) t
  [@@deriving sexp]

  type var =
    ( Global_slot.Checked.t
    , Mina_base.Epoch_seed.var
    , Mina_base.Account.Index.Unpacked.var )
    t

  let to_input
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      ({global_slot; seed; delegator} : value) =
    { Random_oracle.Input.field_elements= [|(seed :> Tick.field)|]
    ; bitstrings=
        [| Global_slot.Bits.to_bits global_slot
         ; Mina_base.Account.Index.to_bits
             ~ledger_depth:constraint_constants.ledger_depth delegator |] }

  let data_spec
      ~(constraint_constants : Genesis_constants.Constraint_constants.t) =
    let open Tick.Data_spec in
    [ Global_slot.typ
    ; Mina_base.Epoch_seed.typ
    ; Mina_base.Account.Index.Unpacked.typ
        ~ledger_depth:constraint_constants.ledger_depth ]

  let typ ~constraint_constants : (var, value) Tick.Typ.t =
    Tick.Typ.of_hlistable
      (data_spec ~constraint_constants)
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  let hash_to_group ~constraint_constants msg =
    Random_oracle.hash ~init:Mina_base.Hash_prefix.vrf_message
      (Random_oracle.pack_input (to_input ~constraint_constants msg))
    |> Group_map.to_group |> Tick.Inner_curve.of_affine

  module Checked = struct
    open Tick

    let to_input ({global_slot; seed; delegator} : var) =
      let open Tick.Checked.Let_syntax in
      let%map global_slot = Global_slot.Checked.to_bits global_slot in
      let s = Bitstring_lib.Bitstring.Lsb_first.to_list in
      { Random_oracle.Input.field_elements=
          [|Mina_base.Epoch_seed.var_to_hash_packed seed|]
      ; bitstrings= [|s global_slot; delegator|] }

    let hash_to_group msg =
      let%bind input = to_input msg in
      Tick.make_checked (fun () ->
          Random_oracle.Checked.hash ~init:Mina_base.Hash_prefix.vrf_message
            (Random_oracle.Checked.pack_input input)
          |> Group_map.Checked.to_group )
  end

  let gen ~(constraint_constants : Genesis_constants.Constraint_constants.t) =
    let open Quickcheck.Let_syntax in
    let%map global_slot = Global_slot.gen
    and seed = Mina_base.Epoch_seed.gen
    and delegator =
      Mina_base.Account.Index.gen
        ~ledger_depth:constraint_constants.ledger_depth
    in
    {global_slot; seed; delegator}
end

(* c is a constant factor on vrf-win likelihood *)
(* c = 2^0 is production behavior *)
(* c > 2^0 is a temporary hack for testnets *)
let c = `Two_to_the 0

let c_bias =
  let (`Two_to_the i) = c in
  fun xs -> List.drop xs i

module Output = struct
  module Truncated = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = string [@@deriving sexp, equal, compare, hash]

        let to_yojson t =
          `String (Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet t)

        let of_yojson = function
          | `String s ->
              Result.map_error
                  (Base64.decode ~alphabet:Base64.uri_safe_alphabet s)
                  ~f:(function `Msg err ->
                  sprintf
                    "Error decoding vrf output in \
                     Vrf.Output.Truncated.Stable.V1.of_yojson: %s"
                    err )
          | _ ->
              Error
                "Vrf.Output.Truncated.Stable.V1.of_yojson: Expected a string"

        let to_latest = Fn.id
      end
    end]

    include Codable.Make_base58_check (struct
      type t = Stable.Latest.t [@@deriving bin_io_unversioned]

      let version_byte = Base58_check.Version_bytes.vrf_truncated_output

      let description = "Vrf Truncated Output"
    end)

    open Tick

    let length_in_bits = Int.min 256 (Field.size_in_bits - 2)

    type var = Boolean.var array

    let typ : (var, t) Typ.t =
      Typ.array ~length:length_in_bits Boolean.typ
      |> Typ.transport
           ~there:(fun s ->
             Array.sub (Blake2.string_to_bits s) ~pos:0 ~len:length_in_bits )
           ~back:Blake2.bits_to_string

    let dummy =
      String.init
        (Base.Int.round ~dir:`Up ~to_multiple_of:8 length_in_bits / 8)
        ~f:(fun _ -> '\000')

    let to_bits t =
      Fold.(to_list (string_bits t)) |> Fn.flip List.take length_in_bits

    (* vrf_output / 2^256 *)
    let to_fraction vrf_output =
      let open Bignum_bigint in
      let n =
        of_bits_lsb (c_bias (Array.to_list (Blake2.string_to_bits vrf_output)))
      in
      Bignum.(
        of_bigint n / of_bigint Bignum_bigint.(shift_left one length_in_bits))
  end

  open Tick

  let typ = Field.typ

  let gen = Field.gen

  let truncate x =
    Random_oracle.Digest.to_bits ~length:Truncated.length_in_bits x
    |> Array.of_list |> Blake2.bits_to_string

  let hash ~constraint_constants msg g =
    let x, y = Non_zero_curve_point.of_inner_curve_exn g in
    let input =
      Random_oracle.Input.(
        append
          (Message.to_input ~constraint_constants msg)
          (field_elements [|x; y|]))
    in
    let open Random_oracle in
    hash ~init:Hash_prefix_states.vrf_output (pack_input input)

  module Checked = struct
    let truncate x =
      Tick.make_checked (fun () ->
          Random_oracle.Checked.Digest.to_bits ~length:Truncated.length_in_bits
            x
          |> Array.of_list )

    let hash msg (x, y) =
      let%bind msg = Message.Checked.to_input msg in
      let input = Random_oracle.Input.(append msg (field_elements [|x; y|])) in
      make_checked (fun () ->
          let open Random_oracle.Checked in
          hash ~init:Hash_prefix_states.vrf_output (pack_input input) )
  end

  let%test_unit "hash unchecked vs. checked equality" =
    let constraint_constants =
      Genesis_constants.Constraint_constants.for_unit_tests
    in
    let gen_inner_curve_point =
      let open Quickcheck.Generator.Let_syntax in
      let%map compressed = Non_zero_curve_point.gen in
      Non_zero_curve_point.to_inner_curve compressed
    in
    let gen_message_and_curve_point =
      let open Quickcheck.Generator.Let_syntax in
      let%map msg = Message.gen ~constraint_constants
      and g = gen_inner_curve_point in
      (msg, g)
    in
    Quickcheck.test ~trials:10 gen_message_and_curve_point
      ~f:
        (Test_util.test_equal ~equal:Field.equal
           Snark_params.Tick.Typ.(
             Message.typ ~constraint_constants
             * Snark_params.Tick.Inner_curve.typ)
           typ
           (fun (msg, g) -> Checked.hash msg g)
           (fun (msg, g) -> hash ~constraint_constants msg g))
end

module Threshold = struct
  open Unsigned

  (* f determines the fraction of slots that will have blocks if c = 2^0 *)
  let f = Bignum.(of_int 3 / of_int 4)

  let base = Bignum.(of_int 1 - f)

  let params =
    Snarky_taylor.Exp.params ~base
      ~field_size_in_bits:Snark_params.Tick.Field.size_in_bits

  let bigint_of_uint64 = Fn.compose Bigint.of_string UInt64.to_string

  (*  Check if
      vrf_output / 2^256 <= c * (1 - (1 - f)^(amount / total_stake))
  *)
  let is_satisfied ~my_stake ~total_stake vrf_output =
    let open Currency in
    let input =
      (* get first params.per_term_precision bits of top / bottom.

         This is equal to

         floor(2^params.per_term_precision * top / bottom) / 2^params.per_term_precision
          *)
      let k = params.per_term_precision in
      let top = bigint_of_uint64 (Balance.to_uint64 my_stake) in
      let bottom = bigint_of_uint64 (Amount.to_uint64 total_stake) in
      Bignum.(
        of_bigint Bignum_bigint.(shift_left top k / bottom)
        / of_bigint Bignum_bigint.(shift_left one k))
    in
    let rhs = Snarky_taylor.Exp.Unchecked.one_minus_exp params input in
    let lhs = Output.Truncated.to_fraction vrf_output in
    Bignum.(lhs <= rhs)

  module Checked = struct
    let is_satisfied ~my_stake ~total_stake (vrf_output : Output.Truncated.var)
        =
      let open Currency in
      let open Snark_params.Tick in
      let open Snarky_integer in
      let open Snarky_taylor in
      make_checked (fun () ->
          let open Run in
          let rhs =
            Exp.one_minus_exp ~m params
              (Floating_point.of_quotient ~m
                 ~precision:params.per_term_precision
                 ~top:(Integer.of_bits ~m (Balance.var_to_bits my_stake))
                 ~bottom:(Integer.of_bits ~m (Amount.var_to_bits total_stake))
                 ~top_is_less_than_bottom:())
          in
          let vrf_output = Array.to_list (vrf_output :> Boolean.var array) in
          let lhs = c_bias vrf_output in
          Floating_point.(
            le ~m
              (of_bits ~m lhs ~precision:Output.Truncated.length_in_bits)
              rhs) )
  end
end

module Evaluation_hash = struct
  let hash_for_proof ~constraint_constants message public_key g1 g2 =
    let input =
      let open Random_oracle_input in
      let g_to_input g =
        { field_elements=
            (let f1, f2 = Group.to_affine_exn g in
             [|f1; f2|])
        ; bitstrings= [||] }
      in
      Array.reduce_exn ~f:Random_oracle_input.append
        [| Message.to_input ~constraint_constants message
         ; g_to_input public_key
         ; g_to_input g1
         ; g_to_input g2 |]
    in
    let tick_output =
      Random_oracle.hash ~init:Mina_base.Hash_prefix.vrf_evaluation
        (Random_oracle.pack_input input)
    in
    (* This isn't great cryptographic practice.. *)
    Tick.Field.unpack tick_output |> Tick.Inner_curve.Scalar.project

  module Checked = struct
    let hash_for_proof message public_key g1 g2 =
      let open Tick.Checked.Let_syntax in
      let%bind input =
        let open Random_oracle_input in
        let g_to_input (f1, f2) =
          {field_elements= [|f1; f2|]; bitstrings= [||]}
        in
        let%map message_input = Message.Checked.to_input message in
        Array.reduce_exn ~f:Random_oracle_input.append
          [|message_input; g_to_input public_key; g_to_input g1; g_to_input g2|]
      in
      let%bind tick_output =
        Tick.make_checked (fun () ->
            Random_oracle.Checked.hash
              ~init:Mina_base.Hash_prefix.vrf_evaluation
              (Random_oracle.Checked.pack_input input) )
      in
      (* This isn't great cryptographic practice.. *)
      Tick.Field.Checked.unpack_full tick_output
  end
end

module Output_hash = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      module T = struct
        type t = Snark_params.Tick.Field.t
        [@@deriving sexp, compare, hash, version {asserted}]
      end

      include T

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t [@@deriving sexp, compare]

  type var = Random_oracle.Checked.Digest.t

  let hash = Output.hash

  module Checked = struct
    let hash = Output.Checked.hash
  end
end

module Integrated =
  Vrf_lib.Integrated.Make (Tick) (Scalar) (Group) (Message) (Output_hash)

module Standalone (Constraint_constants : sig
  val constraint_constants : Genesis_constants.Constraint_constants.t
end) =
struct
  open Constraint_constants

  include Vrf_lib.Standalone.Make (Tick) (Tick.Inner_curve.Scalar) (Group)
            (struct
              include Message

              let typ = typ ~constraint_constants

              let hash_to_group = hash_to_group ~constraint_constants
            end)
            (struct
              include Output_hash

              let hash = hash ~constraint_constants
            end)
            (struct
              include Evaluation_hash

              let hash_for_proof = hash_for_proof ~constraint_constants
            end)
end

type evaluation =
  ( Marlin_plonk_bindings_pasta_pallas.t
  , Marlin_plonk_bindings_pasta_fq.t
    Vrf_lib.Standalone.Evaluation.Discrete_log_equality.Poly.t )
  Vrf_lib.Standalone.Evaluation.Poly.t

type context =
  ( (Unsigned.uint32, Marlin_plonk_bindings_pasta_fp.t, int) Message.t
  , Marlin_plonk_bindings_pasta_pallas.t )
  Vrf_lib.Standalone.Context.t

module Layout = struct
  (* NB: These types are carefully structured to match the GraphQL
         representation. By keeping these in sync, we are able to pass the
         output of GraphQL commands to the input of command line tools and vice
         versa.
  *)
  module Message = struct
    type t =
      { global_slot: Mina_numbers.Global_slot.t [@key "globalSlot"]
      ; epoch_seed: Mina_base.Epoch_seed.t [@key "epochSeed"]
      ; delegator_index: int [@key "delegatorIndex"] }
    [@@deriving yojson]

    let to_message (t : t) : Message.value =
      { global_slot= t.global_slot
      ; seed= t.epoch_seed
      ; delegator= t.delegator_index }

    let of_message (t : Message.value) : t =
      { global_slot= t.global_slot
      ; epoch_seed= t.seed
      ; delegator_index= t.delegator }
  end

  module Threshold = struct
    type t =
      { delegated_stake: Currency.Balance.t [@key "delegatedStake"]
      ; total_stake: Currency.Amount.t [@key "totalStake"] }
    [@@deriving yojson]

    let is_satisfied vrf_output t =
      Threshold.is_satisfied ~my_stake:t.delegated_stake
        ~total_stake:t.total_stake vrf_output
  end

  module Evaluation = struct
    type t =
      { message: Message.t
      ; public_key: Signature_lib.Public_key.t [@key "publicKey"]
      ; c: Scalar.t
      ; s: Scalar.t
      ; scaled_message_hash: Group.t [@key "ScaledMessageHash"]
      ; vrf_threshold: Threshold.t option [@default None] [@key "vrfThreshold"]
      ; vrf_output: Output.Truncated.t option
            [@default None] [@key "vrfOutput"]
      ; vrf_output_fractional: float option
            [@default None] [@key "vrfOutputFractional"]
      ; threshold_met: bool option [@default None] [@key "thresholdMet"] }
    [@@deriving yojson]

    let to_evaluation_and_context (t : t) : evaluation * context =
      ( { discrete_log_equality= {c= t.c; s= t.s}
        ; scaled_message_hash= t.scaled_message_hash }
      , { message= Message.to_message t.message
        ; public_key= Group.of_affine t.public_key } )

    let of_evaluation_and_context
        ((evaluation, context) : evaluation * context) : t =
      { message= Message.of_message context.message
      ; public_key= Group.to_affine_exn context.public_key
      ; c= evaluation.discrete_log_equality.c
      ; s= evaluation.discrete_log_equality.s
      ; scaled_message_hash= evaluation.scaled_message_hash
      ; vrf_threshold= None
      ; vrf_output= None
      ; vrf_output_fractional= None
      ; threshold_met= None }

    let of_message_and_sk ~constraint_constants (message : Message.t)
        (private_key : Signature_lib.Private_key.t) =
      let module Standalone = Standalone (struct
        let constraint_constants = constraint_constants
      end) in
      let message = Message.to_message message in
      let standalone_eval = Standalone.Evaluation.create private_key message in
      let context : Standalone.Context.t =
        { message
        ; public_key=
            Signature_lib.Public_key.of_private_key_exn private_key
            |> Group.of_affine }
      in
      of_evaluation_and_context (standalone_eval, context)

    let to_vrf ~constraint_constants (t : t) =
      let module Standalone = Standalone (struct
        let constraint_constants = constraint_constants
      end) in
      let standalone_eval, context = to_evaluation_and_context t in
      Standalone.Evaluation.verified_output standalone_eval context

    let compute_vrf ~constraint_constants ?delegated_stake ?total_stake (t : t)
        =
      match to_vrf ~constraint_constants t with
      | None ->
          { t with
            vrf_output= None
          ; vrf_output_fractional= None
          ; threshold_met= None }
      | Some vrf ->
          let vrf_output = Output.truncate vrf in
          let vrf_output_fractional =
            Output.Truncated.to_fraction vrf_output |> Bignum.to_float
          in
          let vrf_threshold =
            match (delegated_stake, total_stake) with
            | Some delegated_stake, Some total_stake ->
                Some {Threshold.delegated_stake; total_stake}
            | _ ->
                t.vrf_threshold
          in
          let threshold_met =
            Option.map ~f:(Threshold.is_satisfied vrf_output) vrf_threshold
          in
          { t with
            vrf_threshold
          ; vrf_output= Some vrf_output
          ; vrf_output_fractional= Some vrf_output_fractional
          ; threshold_met }
  end
end

let%test_unit "Standalone and integrates vrfs are consistent" =
  let constraint_constants = Genesis_constants.Constraint_constants.compiled in
  let module Standalone = Standalone (struct
    let constraint_constants = constraint_constants
  end) in
  let inputs =
    let open Quickcheck.Generator.Let_syntax in
    let%bind private_key = Signature_lib.Private_key.gen in
    let%map message = Message.gen ~constraint_constants in
    (private_key, message)
  in
  Quickcheck.test ~seed:(`Deterministic "") inputs
    ~f:(fun (private_key, message) ->
      let integrated_vrf =
        Integrated.eval ~constraint_constants ~private_key message
      in
      let standalone_eval = Standalone.Evaluation.create private_key message in
      let context : Standalone.Context.t =
        { message
        ; public_key=
            Signature_lib.Public_key.of_private_key_exn private_key
            |> Group.of_affine }
      in
      let standalone_vrf =
        Standalone.Evaluation.verified_output standalone_eval context
      in
      [%test_eq: Output_hash.t option] (Some integrated_vrf) standalone_vrf )
