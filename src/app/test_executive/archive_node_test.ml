(* archive_node.ml -- generate data from archive node in cloud *)

open Core_kernel
open Integration_test_lib

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs
  open Engine
  open Dsl

  type network = Network.t

  type node = Network.Node.t

  type dsl = Dsl.t

  let config =
    let open Test_config in
    { default with
      requires_graphql = true (* a few block producers, where few = 4 *)
    ; block_producers =
        [ { balance = "4000"; timing = Untimed }
        ; { balance = "9000"; timing = Untimed }
        ; { balance = "8000"; timing = Untimed }
        ; { balance = "17000"; timing = Untimed }
        ]
    ; num_archive_nodes = 1
    ; num_snark_workers = 0
    ; log_precomputed_blocks = true
    }

  (* number of minutes to let the network run, after initialization *)
  let runtime_min = 5.

  let run network t =
    let open Malleable_error.Let_syntax in
    let logger = Logger.create () in
    let archive_node = List.hd_exn @@ Network.archive_nodes network in
    let block_producers = Network.block_producers network in
    (* waiting for archive_node does not seem to work *)
    [%log info] "archive node test: waiting for block producers to initialize" ;
    let%bind () =
      Malleable_error.List.iter block_producers ~f:(fun bp ->
          wait_for t (Wait_condition.node_to_initialize bp))
    in
    [%log info] "archive node test: waiting for archive node to initialize" ;
    let%bind () = wait_for t (Wait_condition.node_to_initialize archive_node) in
    [%log info] "archive node test: running network for %0.1f minutes"
      runtime_min ;
    let%bind.Async.Deferred () = Async.after (Time.Span.of_min runtime_min) in
    [%log info] "archive node test: done running network" ;
    let%bind () =
      Network.Node.dump_archive_data ~logger archive_node
        ~data_file:"archive.sql"
    in
    [%log info] "archive node test: collecting block logs" ;
    let%map () =
      Malleable_error.List.iter block_producers ~f:(fun bp ->
          Network.Node.dump_precomputed_blocks ~logger bp)
    in
    [%log info] "archive node test: succesfully completed"
end
