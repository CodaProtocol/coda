open Async_kernel
open Core_kernel
open Pipe_lib

let%test_module "network pool test" =
  ( module struct
    let trust_system = Mocks.trust_system

    module Mock_snark_pool =
      Snark_pool.Make (Mocks.Ledger_proof) (Mocks.Transaction_snark)
        (Mocks.Transaction_snark_work)
        (Mocks.Transition_frontier)

    let%test_unit "Work that gets fed into apply_and_broadcast will be \
                   received in the pool's reader" =
      let pool_reader, _pool_writer = Linear_pipe.create () in
      let frontier_broadcast_pipe_r, _ =
        Broadcast_pipe.create (Some (Mocks.Transition_frontier.create ()))
      in
      let network_pool =
        Mock_snark_pool.create ~logger:(Logger.null ()) ~trust_system
          ~incoming_diffs:pool_reader
          ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
      in
      let work = [1] in
      let priced_proof =
        { Priced_proof.proof= []
        ; fee=
            { fee= Currency.Fee.of_int 0
            ; prover= Signature_lib.Public_key.Compressed.empty } }
      in
      let command =
        Mock_snark_pool.Resource_pool.Diff.Stable.V1.Add_solved_work
          (work, priced_proof)
      in
      (fun () ->
        don't_wait_for
        @@ Linear_pipe.iter (Mock_snark_pool.broadcasts network_pool)
             ~f:(fun _ ->
               let pool = Mock_snark_pool.resource_pool network_pool in
               ( match
                   Mock_snark_pool.Resource_pool.request_proof pool work
                 with
               | Some {proof; fee= _} ->
                   assert (proof = priced_proof.proof)
               | None ->
                   failwith "There should have been a proof here" ) ;
               Deferred.unit ) ;
        Mock_snark_pool.apply_and_broadcast network_pool
          (Envelope.Incoming.local command) )
      |> Async.Thread_safe.block_on_async_exn

    let%test_unit "when creating a network, the incoming diffs in reader pipe \
                   will automatically get process" =
      let works = List.range 0 10 |> List.map ~f:(fun x -> [x]) in
      let verify_unsolved_work () =
        let work_diffs =
          List.map works ~f:(fun work ->
              Envelope.Incoming.local
                (Mock_snark_pool.Resource_pool.Diff.Stable.V1.Add_solved_work
                   ( work
                   , Priced_proof.
                       { proof= []
                       ; fee=
                           { fee= Currency.Fee.of_int 0
                           ; prover= Signature_lib.Public_key.Compressed.empty
                           } } )) )
          |> Linear_pipe.of_list
        in
        let frontier_broadcast_pipe_r, _ =
          Broadcast_pipe.create (Some (Mocks.Transition_frontier.create ()))
        in
        let network_pool =
          Mock_snark_pool.create ~logger:(Logger.null ()) ~trust_system
            ~incoming_diffs:work_diffs
            ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
        in
        don't_wait_for
        @@ Linear_pipe.iter (Mock_snark_pool.broadcasts network_pool)
             ~f:(fun work_command ->
               let work =
                 match work_command with
                 | Mock_snark_pool.Resource_pool.Diff.Stable.V1.Add_solved_work
                     (work, _) ->
                     work
               in
               assert (List.mem works work ~equal:( = )) ;
               Deferred.unit ) ;
        Deferred.unit
      in
      verify_unsolved_work |> Async.Thread_safe.block_on_async_exn
  end )
