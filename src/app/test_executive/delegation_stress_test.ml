open Core_kernel
open Integration_test_lib

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs
  open Engine
  open Dsl

  type network = Network.t

  type node = Network.Node.t

  type dsl = Dsl.t

  let delegation_count = 10_000

  let config =
    let open Test_config in
    let open Test_config.Account_config in
    let block_producers = [{balance= "0"; timing= Untimed; delegate= None}] in
    let delegating_account =
      {balance= "1000"; timing= Untimed; delegate= Some (Block_producer 0)}
    in
    let additional_accounts =
      List.init delegation_count ~f:(Fn.const delegating_account)
    in
    { default with
      block_producers
    ; additional_accounts
        (* this test does not run long enough to require snark work *)
    ; num_snark_workers= 0 }

  let run network t =
    let open Malleable_error.Let_syntax in
    let block_producer = List.nth_exn (Network.block_producers network) 0 in
    let%bind () =
      wait_for t (Wait_condition.node_to_initialize block_producer)
    in
    (* This tests is the block producer can keep up with the VRF evaluations.
     * If the block producer were to fall behind the VRF evaluations, we would expect
     * it to be unable to produce blocks since it wouldn't find the winning slots
     * in time. *)
    section "blocks are produced"
      (wait_for t (Wait_condition.blocks_to_be_produced 6))
end
