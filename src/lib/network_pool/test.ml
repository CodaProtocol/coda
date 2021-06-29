open Async_kernel
open Core_kernel
open Pipe_lib
open Network_peer

let%test_module "network pool test" =
  ( module struct
    let trust_system = Mocks.trust_system

    let logger = Logger.null ()

    let precomputed_values = Lazy.force Precomputed_values.for_unit_tests

    let constraint_constants = precomputed_values.constraint_constants

    let consensus_constants = precomputed_values.consensus_constants

    let proof_level = precomputed_values.proof_level

    let time_controller = Block_time.Controller.basic ~logger

    let verifier =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Verifier.create ~logger ~proof_level ~constraint_constants
            ~conf_dir:None
            ~pids:(Child_processes.Termination.create_pid_table ()) )

    module Mock_snark_pool =
      Snark_pool.Make (Mocks.Base_ledger) (Mocks.Staged_ledger)
        (Mocks.Transition_frontier)

    let config =
      Mock_snark_pool.Resource_pool.make_config ~verifier ~trust_system
        ~disk_location:"/tmp/snark-pool"

    let%test_unit "Work that gets fed into apply_and_broadcast will be \
                   received in the pool's reader" =
      let pool_reader, _pool_writer =
        Strict_pipe.(create ~name:"Network pool test" Synchronous)
      in
      let local_reader, _local_writer =
        Strict_pipe.(create ~name:"Network pool test" Synchronous)
      in
      let tf = Mocks.Transition_frontier.create [] in
      let frontier_broadcast_pipe_r, _ = Broadcast_pipe.create (Some tf) in
      let work =
        `One
          (Quickcheck.random_value ~seed:(`Deterministic "network_pool_test")
             Transaction_snark.Statement.gen)
      in
      let priced_proof =
        { Priced_proof.proof=
            One_or_two.map ~f:Ledger_proof.For_tests.mk_dummy_proof work
        ; fee=
            { fee= Currency.Fee.of_int 0
            ; prover= Signature_lib.Public_key.Compressed.empty } }
      in
      Async.Thread_safe.block_on_async_exn (fun () ->
          let network_pool =
            Mock_snark_pool.create ~config ~logger ~constraint_constants
              ~consensus_constants ~time_controller ~incoming_diffs:pool_reader
              ~local_diffs:local_reader
              ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
          in
          let%bind () = Mocks.Transition_frontier.refer_statements tf [work] in
          let command =
            Mock_snark_pool.Resource_pool.Diff.Add_solved_work
              (work, priced_proof)
          in
          don't_wait_for
            (Mock_snark_pool.apply_and_broadcast network_pool
               (Envelope.Incoming.local command)
               (Mock_snark_pool.Broadcast_callback.Local (Fn.const ()))) ;
          let%map _ =
            Linear_pipe.read (Mock_snark_pool.broadcasts network_pool)
          in
          let pool = Mock_snark_pool.resource_pool network_pool in
          match Mock_snark_pool.Resource_pool.request_proof pool work with
          | Some {proof; fee= _} ->
              assert (
                [%equal: Ledger_proof.t One_or_two.t] proof priced_proof.proof
              )
          | None ->
              failwith "There should have been a proof here" )

    let%test_unit "when creating a network, the incoming diffs and local \
                   diffs in the reader pipes will automatically get process" =
      let work_count = 10 in
      let works =
        Quickcheck.random_sequence ~seed:(`Deterministic "works")
          Transaction_snark.Statement.gen
        |> Fn.flip Sequence.take work_count
        |> Sequence.map ~f:(fun x -> `One x)
        |> Sequence.to_list
      in
      let per_reader = work_count / 2 in
      let create_work work =
        Mock_snark_pool.Resource_pool.Diff.Add_solved_work
          ( work
          , Priced_proof.
              { proof=
                  One_or_two.map ~f:Ledger_proof.For_tests.mk_dummy_proof work
              ; fee=
                  { fee= Currency.Fee.of_int 0
                  ; prover= Signature_lib.Public_key.Compressed.empty } } )
      in
      let verify_unsolved_work () =
        let pool_reader, pool_writer =
          Strict_pipe.(create ~name:"Network pool test" Synchronous)
        in
        let local_reader, local_writer =
          Strict_pipe.(create ~name:"Network pool test" Synchronous)
        in
        List.map (List.take works per_reader) ~f:create_work
        |> List.map ~f:(fun work ->
               ( Envelope.Incoming.local work
               , Mina_net2.Validation_callback.create_without_expiration () )
           )
        |> List.iter ~f:(fun diff ->
               Strict_pipe.Writer.write pool_writer diff
               |> Deferred.don't_wait_for ) ;
        List.map (List.drop works per_reader) ~f:create_work
        |> List.iter ~f:(fun diff ->
               Strict_pipe.Writer.write local_writer (diff, Fn.const ())
               |> Deferred.don't_wait_for ) ;
        let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
        let tf = Mocks.Transition_frontier.create [] in
        let frontier_broadcast_pipe_r, _ = Broadcast_pipe.create (Some tf) in
        let network_pool =
          Mock_snark_pool.create ~config ~logger ~constraint_constants
            ~consensus_constants ~time_controller ~incoming_diffs:pool_reader
            ~local_diffs:local_reader
            ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
        in
        let%bind () = Mocks.Transition_frontier.refer_statements tf works in
        don't_wait_for
        @@ Linear_pipe.iter (Mock_snark_pool.broadcasts network_pool)
             ~f:(fun work_command ->
               let work =
                 match work_command with
                 | Mock_snark_pool.Resource_pool.Diff.Add_solved_work (work, _)
                   ->
                     work
                 | Mock_snark_pool.Resource_pool.Diff.Empty ->
                     assert false
               in
               assert (
                 List.mem works work
                   ~equal:Transaction_snark_work.Statement.equal ) ;
               Deferred.unit ) ;
        Deferred.unit
      in
      verify_unsolved_work |> Async.Thread_safe.block_on_async_exn
  end )
