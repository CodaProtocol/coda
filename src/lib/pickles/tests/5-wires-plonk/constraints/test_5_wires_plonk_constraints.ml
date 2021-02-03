module Snarky = Snarky_backendless
open Marlin_plonk_bindings

module ComputationExample
    (Impl : Snarky.Snark_intf.Run
            with type prover_state = unit
             and type field = Pasta_fp.t) (Params : sig
        val params : Impl.field Sponge.Params.t
    end) =
struct
  open Core
  open Impl

  let computation x () =
    let open Field in
    for _j = 0 to 17 do
      (***** PACKING *****)
      let module Pack = Pack.Constraints (Impl) in
      let bits = Pack.unpack x in
      let scalar = Pack.pack bits in
      assert_ (Snarky.Constraint.equal x scalar) ;
      (***** POSEIDON PERMUTATION *****)
      let module Poseidon = Poseidon.Constraints (Impl) (Params) in
      let perm =
        Poseidon.permute [|x; x + one; x - one; square x; square x|] 31
      in
      assert_ (Snarky.Constraint.equal perm.(0) perm.(0)) ;
      (***** EC ARITHMETIC *****)
      let module Ecc = Ecc.Constraints (Impl) in
      let y = sqrt ((x * x * x) + Impl.Field.of_int 5) in
      let rec double (x, y) n =
        if n < 1 then (x, y) else double (Ecc.double (x, y)) Int.(n - 1)
      in
      let xd, yd = double (x, y) (Array.length bits) in
      let x1, y1 = Ecc.add (Ecc.scale (x, y) bits) (xd, negate yd) in
      assert_
        (Snarky.Constraint.equal (y1 * y1)
           ((x1 * x1 * x1) + Impl.Field.of_int 5)) ;
      let x1, y1 = Ecc.scale (x, y) bits in
      assert_
        (Snarky.Constraint.equal (y1 * y1)
           ((x1 * x1 * x1) + Impl.Field.of_int 5)) ;
      let x2, y2 = Ecc.scale_pack (x, y) x in
      assert_
        (Snarky.Constraint.equal (y2 * y2)
           ((x2 * x2 * x2) + Impl.Field.of_int 5)) ;
      let x3, y3 = Ecc.scale_pack (x2, y2) y in
      assert_
        (Snarky.Constraint.equal (y3 * y3)
           ((x3 * x3 * x3) + Impl.Field.of_int 5)) ;
      let x4, y4 = Ecc.endoscale (x3, y3) bits in
      assert_
        (Snarky.Constraint.equal (y4 * y4)
           ((x4 * x4 * x4) + Impl.Field.of_int 5)) ;
      assert_ (Snarky.Constraint.equal x1 x2) ;
      assert_ (Snarky.Constraint.equal y1 y2)
    done ;
    ()

  let input () = Impl.Data_spec.[Impl.Field.typ]

  let keys = Impl.generate_keypair ~exposing:(input ()) computation

  let statement = Impl.Field.Constant.of_int 13579

  let check () =
    let input = exists Impl.Field.typ ~compute:(fun () -> statement) in
    computation input ()

  let proof =
    Impl.prove (Impl.Keypair.pk keys) (input ()) computation () statement

  let%test_unit "check backend ComputationExample proof" =
    assert (Impl.verify proof (Impl.Keypair.vk keys) (input ()) statement)
end

(* Disabled. We don't need to run this in CI. *)
(*
let%test_module "backend test" =
  ( module struct
    let () =
      Zexe_backend.Pasta_5_wires.Vesta_based_plonk.Keypair.set_urs_info
        [On_disk {directory= "/tmp/"; should_write= true}]

    module Impl =
      Snarky.Snark.Run.Make
        (Zexe_backend.Pasta_5_wires.Vesta_based_plonk)
        (Core.Unit)

    module Params = struct
      let params =
        Sponge.Params.(map pasta_p5 ~f:Impl.Field.Constant.of_string)
    end

    include ComputationExample (Impl) (Params)
  end )
*)
