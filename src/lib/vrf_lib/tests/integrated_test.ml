open Core_kernel
open Snark_params
open Signature_lib

module Scalar = struct
  include Tick.Inner_curve.Scalar

  type value = t
end

module Group = struct
  type value = Tick.Inner_curve.t

  type var = Tick.Inner_curve.var

  let scale = Tick.Inner_curve.scale

  module Checked = struct
    include Tick.Inner_curve.Checked

    let scale_generator shifted s ~init =
      scale_known shifted Tick.Inner_curve.one s ~init
  end
end

module Message = struct
  type 'state_hash t = {state_hash: 'state_hash} [@@deriving hlist]

  type value = Mina_base.State_hash.t t

  type var = Mina_base.State_hash.var t

  let data_spec = Tick.Data_spec.[Mina_base.State_hash.typ]

  let typ =
    Tick.Typ.of_hlistable data_spec ~var_to_hlist:to_hlist
      ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

  let gen =
    let open Quickcheck.Let_syntax in
    let%map state_hash = Mina_base.State_hash.gen in
    {state_hash}

  let hash_to_group ~constraint_constants:_ msg =
    Group_map.to_group
      (Random_oracle.hash ~init:Mina_base.Hash_prefix.vrf_message
         [|msg.state_hash|])
    |> Tick.Inner_curve.of_affine

  module Checked = struct
    let hash_to_group msg =
      Tick.make_checked (fun () ->
          Group_map.Checked.to_group
            (Random_oracle.Checked.hash ~init:Mina_base.Hash_prefix.vrf_message
               (Random_oracle.Checked.pack_input
                  (Mina_base.State_hash.var_to_input msg.state_hash))) )
  end
end

module Output_hash = struct
  type value = Snark_params.Tick.Field.t [@@deriving eq, sexp]

  type t = value [@@deriving eq, sexp]

  type var = Random_oracle.Checked.Digest.t

  let typ : (var, value) Snark_params.Tick.Typ.t = Snark_params.Tick.Field.typ

  let hash ~constraint_constants:_ ({Message.state_hash} : Message.value) g =
    let x, y = Snark_params.Tick.Inner_curve.to_affine_exn g in
    Random_oracle.hash [|(state_hash :> Snark_params.Tick.Field.t); x; y|]

  module Checked = struct
    let hash ({state_hash} : Message.var) g =
      Snark_params.Tick.make_checked (fun () ->
          let x, y = g in
          Random_oracle.Checked.hash
            [|Mina_base.State_hash.var_to_hash_packed state_hash; x; y|] )
  end
end

module Vrf =
  Vrf_lib.Integrated.Make (Tick) (Scalar) (Group) (Message) (Output_hash)

let%test_unit "eval unchecked vs. checked equality" =
  let constraint_constants =
    Genesis_constants.Constraint_constants.for_unit_tests
  in
  let gen =
    let open Quickcheck.Let_syntax in
    let%map pk = Private_key.gen and msg = Message.gen in
    (pk, msg)
  in
  Quickcheck.test ~trials:10 gen
    ~f:
      (Tick.Test.test_equal ~sexp_of_t:[%sexp_of: Output_hash.value]
         ~equal:Output_hash.equal_value
         Tick.Typ.(Scalar.typ * Message.typ)
         Output_hash.typ
         (fun (private_key, msg) ->
           let open Tick.Checked in
           let%bind (module Shifted) = Group.Checked.Shifted.create () in
           Vrf.Checked.eval (module Shifted) ~private_key msg )
         (fun (private_key, msg) ->
           Vrf.eval ~constraint_constants ~private_key msg ))

let%bench_module "vrf bench module" =
  ( module struct
    let constraint_constants =
      Genesis_constants.Constraint_constants.for_unit_tests

    let gen =
      let open Quickcheck.Let_syntax in
      let%map pk = Private_key.gen and msg = Message.gen in
      (pk, msg)

    let%bench_fun "vrf eval unchecked" =
      let private_key, msg = Quickcheck.random_value gen in
      fun () -> Vrf.eval ~constraint_constants ~private_key msg

    let%bench_fun "vrf eval checked" =
      let private_key, msg = Quickcheck.random_value gen in
      fun () ->
        Tick.Test.checked_to_unchecked
          Tick.Typ.(Scalar.typ * Message.typ)
          Output_hash.typ
          (fun (private_key, msg) ->
            let open Tick.Checked in
            let%bind (module Shifted) = Group.Checked.Shifted.create () in
            Vrf.Checked.eval (module Shifted) ~private_key msg )
          (private_key, msg)
  end )
