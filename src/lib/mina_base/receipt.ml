(* receipt.ml *)

[%%import
"/src/config.mlh"]

open Core_kernel
module B58_lib = Base58_check

[%%ifdef
consensus_mechanism]

open Snark_params.Tick

[%%else]

open Snark_params_nonconsensus
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

module Elt = struct
  type t =
    | Signed_command of Signed_command.Payload.t
    | Snapp_command of Random_oracle.Digest.t
end

module Chain_hash = struct
  include Data_hash.Make_full_size (struct
    let description = "Receipt chain hash"

    let version_byte = Base58_check.Version_bytes.receipt_chain_hash
  end)

  (* Data hash versioned boilerplate below *)

  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      module T = struct
        type t = Field.t [@@deriving sexp, compare, hash, version {asserted}]
      end

      include T

      let to_latest = Fn.id

      [%%define_from_scope
      to_yojson, of_yojson]

      include Comparable.Make (T)
      include Hashable.Make_binable (T)
    end
  end]

  type _unused = unit constraint t = Stable.Latest.t

  let empty = of_hash Random_oracle.(salt "CodaReceiptEmpty" |> digest)

  let cons (e : Elt.t) (t : t) =
    let open Random_oracle in
    let init, x =
      let open Hash_prefix in
      match e with
      | Signed_command payload ->
          ( receipt_chain_user_command
          , Transaction_union_payload.(
              to_input (of_user_command_payload payload)) )
      | Snapp_command s ->
          (receipt_chain_snapp, Input.field s)
    in
    Input.(append x (field (t :> Field.t)))
    |> pack_input |> hash ~init |> of_hash

  [%%if
  defined consensus_mechanism]

  module Checked = struct
    module Elt = struct
      type t =
        | Signed_command of Transaction_union_payload.var
        | Snapp_command of Random_oracle.Checked.Digest.t
    end

    let constant (t : t) =
      var_of_hash_packed (Field.Var.constant (t :> Field.t))

    type t = var

    let if_ = if_

    let cons (e : Elt.t) t =
      let open Random_oracle in
      let open Checked in
      let open Hash_prefix in
      let%bind init, x =
        match e with
        | Signed_command payload ->
            let%map payload =
              Transaction_union_payload.Checked.to_input payload
            in
            (receipt_chain_user_command, payload)
        | Snapp_command s ->
            Let_syntax.return (receipt_chain_snapp, Input.field s)
      in
      make_checked (fun () ->
          hash ~init (pack_input Input.(append x (var_to_input t)))
          |> var_of_hash_packed )
  end

  let%test_unit "checked-unchecked equivalence" =
    let open Quickcheck in
    test ~trials:20 (Generator.tuple2 gen Signed_command_payload.gen)
      ~f:(fun (base, payload) ->
        let unchecked = cons (Signed_command payload) base in
        let checked =
          let comp =
            let open Snark_params.Tick.Checked.Let_syntax in
            let payload =
              Transaction_union_payload.(
                Checked.constant (of_user_command_payload payload))
            in
            let%map res =
              Checked.cons (Signed_command payload) (var_of_t base)
            in
            As_prover.read typ res
          in
          let (), x = Or_error.ok_exn (run_and_check comp ()) in
          x
        in
        assert (equal unchecked checked) )

  let%test_unit "json" =
    Quickcheck.test ~trials:20 gen ~sexp_of:sexp_of_t ~f:(fun t ->
        assert (Codable.For_tests.check_encoding (module Stable.V1) ~equal t)
    )

  [%%endif]
end
