open Core_kernel
open Async_kernel
open Snark_params
open Snark_bits
open Fold_lib
module Digest = Tick.Pedersen.Digest

module Keys = struct
  module Per_curve_location = struct
    module T = struct
      type t = {step: string; wrap: string} [@@deriving sexp]
    end

    include T
    include Sexpable.To_stringable (T)
  end

  module Proving = struct
    module Location = Per_curve_location

    let checksum ~step ~wrap =
      Md5.digest_string
        ("Blockchain_transition_proving" ^ Md5.to_hex step ^ Md5.to_hex wrap)

    type t = {step: Tick.Proving_key.t; wrap: Tock.Proving_key.t}

    let dummy =
      {step= Dummy_values.Tick.proving_key; wrap= Dummy_values.Tock.proving_key}

    let load ({step; wrap}: Location.t) =
      let open Storage.Disk in
      let parent_log = Logger.create () in
      let tick_controller =
        Controller.create ~parent_log Tick.Proving_key.bin_t
      in
      let tock_controller =
        Controller.create ~parent_log Tock.Proving_key.bin_t
      in
      let open Async in
      let load c p =
        match%map load_with_checksum c p with
        | Ok x -> x
        | Error _e -> failwithf "Transaction_snark: load failed on %s" p ()
      in
      let%map step = load tick_controller step
      and wrap = load tock_controller wrap in
      let t = {step= step.data; wrap= wrap.data} in
      (t, checksum ~step:step.checksum ~wrap:wrap.checksum)
  end

  module Verification = struct
    module Location = Per_curve_location

    let checksum ~step ~wrap =
      Md5.digest_string
        ( "Blockchain_transition_verification" ^ Md5.to_hex step
        ^ Md5.to_hex wrap )

    type t = {step: Tick.Verification_key.t; wrap: Tock.Verification_key.t}

    let dummy =
      { step= Dummy_values.Tick.verification_key
      ; wrap= Dummy_values.Tock.verification_key }

    let load ({step; wrap}: Location.t) =
      let open Storage.Disk in
      let parent_log = Logger.create () in
      let tick_controller =
        Controller.create ~parent_log Tick.Verification_key.bin_t
      in
      let tock_controller =
        Controller.create ~parent_log Tock.Verification_key.bin_t
      in
      let open Async in
      let load c p =
        match%map load_with_checksum c p with
        | Ok x -> x
        | Error _e -> failwithf "Transaction_snark: load failed on %s" p ()
      in
      let%map step = load tick_controller step
      and wrap = load tock_controller wrap in
      let t = {step= step.data; wrap= wrap.data} in
      (t, checksum ~step:step.checksum ~wrap:wrap.checksum)
  end

  type t = {proving: Proving.t; verification: Verification.t}

  let dummy = {proving= Proving.dummy; verification= Verification.dummy}

  module Checksum = struct
    type t = {proving: Md5.t; verification: Md5.t}
  end

  module Location = struct
    module T = struct
      type t =
        {proving: Proving.Location.t; verification: Verification.Location.t}
      [@@deriving sexp]
    end

    include T
    include Sexpable.To_stringable (T)
  end

  let load ({proving; verification}: Location.t) =
    let open Storage.Disk in
    let%map proving, proving_checksum = Proving.load proving
    and verification, verification_checksum = Verification.load verification in
    ( {proving; verification}
    , {Checksum.proving= proving_checksum; verification= verification_checksum}
    )
end

module Make
    (Consensus_mechanism : Consensus.Mechanism.S
                           with type Proof.t = Tock.Proof.t)
    (T : Transaction_snark.Verification.S) =
struct
  module Blockchain = Blockchain_state.Make (Consensus_mechanism)

  module System = struct
    module U = Blockchain.Make_update (T)
    module Update = Consensus_mechanism.Snark_transition

    module State = struct
      include Consensus_mechanism.Protocol_state

      include (
        Blockchain :
          module type of Blockchain with module Checked := Blockchain.Checked )

      include (U : module type of U with module Checked := U.Checked)

      module Hash = Nanobit_base.State_hash

      module Checked = struct
        include Blockchain.Checked
        include U.Checked
      end
    end
  end

  open Nanobit_base

  include Transition_system.Make (struct
              module Tick = struct
                module Packed = struct
                  type value = Tick.Pedersen.Digest.t

                  type var = Tick.Pedersen.Checked.Digest.var

                  let typ = Tick.Pedersen.Checked.Digest.typ
                end

                module Unpacked = struct
                  type value = Tick.Pedersen.Checked.Digest.Unpacked.t

                  type var = Tick.Pedersen.Checked.Digest.Unpacked.var

                  let typ : (var, value) Tick.Typ.t =
                    Tick.Pedersen.Checked.Digest.Unpacked.typ

                  let var_to_bits (x: var) = (x :> Tick.Boolean.var list)

                  let var_to_triples xs =
                    let open Fold in
                    to_list
                      (group3 ~default:Tick.Boolean.false_
                         (of_list (var_to_bits xs)))

                  let var_of_value =
                    Tick.Pedersen.Checked.Digest.Unpacked.constant
                end

                let project_value = Tick.Field.project

                let project_var = Tick.Pedersen.Checked.Digest.Unpacked.project

                let unpack_value = Tick.Field.unpack

                let choose_preimage_var =
                  Tick.Pedersen.Checked.Digest.choose_preimage
              end

              module Tock = Bits.Snarkable.Field (Tock)
            end)
            (System)

  module Keys = struct
    include Keys

    let step_cached =
      let load =
        let open Tick in
        let open Cached.Let_syntax in
        let%map verification =
          Cached.component ~label:"verification" ~f:Keypair.vk
            Verification_key.bin_t
        and proving =
          Cached.component ~label:"proving" ~f:Keypair.pk Proving_key.bin_t
        in
        (verification, proving)
      in
      Cached.Spec.create ~load ~directory:Cache_dir.cache_dir
        ~digest_input:
          (Fn.compose Md5.to_hex Tick.R1CS_constraint_system.digest)
        ~create_env:Tick.Keypair.generate
        ~input:
          (Tick.constraint_system ~exposing:(Step_base.input ()) Step_base.main)

    let cached () =
      let%bind step_vk, step_pk = Cached.run step_cached in
      let module Wrap = Wrap_base (struct
        let verification_key = step_vk.value
      end) in
      let wrap_cached =
        let load =
          let open Tock in
          let open Cached.Let_syntax in
          let%map verification =
            Cached.component ~label:"verification" ~f:Keypair.vk
              Verification_key.bin_t
          and proving =
            Cached.component ~label:"proving" ~f:Keypair.pk Proving_key.bin_t
          in
          (verification, proving)
        in
        Cached.Spec.create ~load ~directory:Cache_dir.cache_dir
          ~digest_input:
            (Fn.compose Md5.to_hex Tock.R1CS_constraint_system.digest)
          ~create_env:Tock.Keypair.generate
          ~input:(Tock.constraint_system ~exposing:(Wrap.input ()) Wrap.main)
      in
      let%map wrap_vk, wrap_pk = Cached.run wrap_cached in
      let location : Location.t =
        { proving= {step= step_pk.path; wrap= wrap_pk.path}
        ; verification= {step= step_vk.path; wrap= wrap_vk.path} }
      in
      let checksum : Checksum.t =
        { proving=
            Proving.checksum ~step:step_pk.checksum ~wrap:wrap_pk.checksum
        ; verification=
            Verification.checksum ~step:step_vk.checksum ~wrap:wrap_vk.checksum
        }
      in
      let t =
        { proving= {step= step_pk.value; wrap= wrap_pk.value}
        ; verification= {step= step_vk.value; wrap= wrap_vk.value} }
      in
      (location, t, checksum)
  end
end
