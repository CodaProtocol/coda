open Core_kernel
open Mina_base
open Pipe_lib
open Network_pool

module State = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Pending | Included | Unknown [@@deriving equal, sexp, compare]

      let to_latest = Fn.id
    end
  end]

  let to_string = function
    | Pending ->
        "PENDING"
    | Included ->
        "INCLUDED"
    | Unknown ->
        "UNKOWN"
end

(* TODO: this is extremely expensive as implemented and needs to be replaced with an extension *)
let get_status ~(network_id : int) ~frontier_broadcast_pipe ~transaction_pool
    cmd =
  let open Or_error.Let_syntax in
  let%map check_cmd =
    Result.of_option
      (Signed_command.check ~network_id cmd)
      ~error:(Error.of_string "Invalid signature")
    |> Result.map ~f:(fun x ->
           Transaction_hash.User_command_with_valid_signature.create
             (Signed_command x))
  in
  let resource_pool = Transaction_pool.resource_pool transaction_pool in
  match Broadcast_pipe.Reader.peek frontier_broadcast_pipe with
  | None ->
      State.Unknown
  | Some transition_frontier ->
      with_return (fun { return } ->
          let best_tip_path =
            Transition_frontier.best_tip_path transition_frontier
          in
          let in_breadcrumb breadcrumb =
            List.exists (Transition_frontier.Breadcrumb.commands breadcrumb)
              ~f:(fun { data = cmd'; _ } ->
                match cmd' with
                | Snapp_command _ ->
                    false
                | Signed_command cmd' ->
                    Signed_command.equal cmd (Signed_command.forget_check cmd'))
          in
          if List.exists ~f:in_breadcrumb best_tip_path then
            return State.Included ;
          if
            List.exists ~f:in_breadcrumb
              (Transition_frontier.all_breadcrumbs transition_frontier)
          then return State.Pending ;
          if Transaction_pool.Resource_pool.member resource_pool check_cmd then
            return State.Pending ;
          State.Unknown)

let%test_module "transaction_status" =
  ( module struct
    open Async
    open Mina_numbers

    let max_length = 10

    let frontier_size = 1

    let logger = Logger.null ()

    let time_controller = Block_time.Controller.basic ~logger

    let precomputed_values = Lazy.force Precomputed_values.for_unit_tests

    let proof_level = precomputed_values.proof_level

    let constraint_constants = precomputed_values.constraint_constants

    let network_id = constraint_constants.network_id

    module Genesis_ledger = (val precomputed_values.genesis_ledger)

    let trust_system = Trust_system.null ()

    let pool_max_size = precomputed_values.genesis_constants.txpool_max_size

    let verifier =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Verifier.create ~logger ~proof_level ~constraint_constants
            ~conf_dir:None
            ~pids:(Child_processes.Termination.create_pid_table ()))

    let key_gen =
      let open Quickcheck.Generator in
      let open Quickcheck.Generator.Let_syntax in
      let keypairs = List.map (Lazy.force Genesis_ledger.accounts) ~f:fst in
      let%map random_key_opt = of_list keypairs in
      ( Genesis_ledger.largest_account_keypair_exn ()
      , Signature_lib.Keypair.of_private_key_exn
          (Option.value_exn random_key_opt) )

    let gen_frontier =
      Transition_frontier.For_tests.gen ~logger ~precomputed_values ~verifier
        ~trust_system ~max_length ~size:frontier_size ()

    let gen_user_command =
      Signed_command.Gen.payment ~network_id ~sign_type:`Real ~max_amount:100
        ~fee_range:10 ~key_gen ~nonce:(Account_nonce.of_int 1) ()

    let create_pool ~frontier_broadcast_pipe =
      let pool_reader, _ =
        Strict_pipe.(
          create ~name:"transaction_status incomming diff" Synchronous)
      in
      let local_reader, local_writer =
        Strict_pipe.(create ~name:"transaction_status local diff" Synchronous)
      in
      let config =
        Transaction_pool.Resource_pool.make_config ~trust_system ~pool_max_size
          ~verifier
      in
      let transaction_pool =
        Transaction_pool.create ~config
          ~constraint_constants:precomputed_values.constraint_constants
          ~consensus_constants:precomputed_values.consensus_constants
          ~time_controller ~incoming_diffs:pool_reader ~logger
          ~local_diffs:local_reader ~frontier_broadcast_pipe
      in
      don't_wait_for
      @@ Linear_pipe.iter (Transaction_pool.broadcasts transaction_pool)
           ~f:(fun transactions ->
             [%log trace]
               "Transactions have been applied successfully and is propagated \
                throughout the 'network'"
               ~metadata:
                 [ ( "transactions"
                   , Transaction_pool.Resource_pool.Diff.to_yojson transactions
                   )
                 ] ;
             Deferred.unit) ;
      (* Need to wait for transaction_pool to see the transition_frontier *)
      let%map () = Async.Scheduler.yield_until_no_jobs_remain () in
      (transaction_pool, local_writer)

    let%test_unit "If the transition frontier currently doesn't exist, the \
                   status of a sent transaction will be unknown" =
      Quickcheck.test ~trials:1 gen_user_command ~f:(fun user_command ->
          Backtrace.elide := false ;
          Async.Thread_safe.block_on_async_exn (fun () ->
              let frontier_broadcast_pipe, _ = Broadcast_pipe.create None in
              let%bind transaction_pool, local_diffs_writer =
                create_pool ~frontier_broadcast_pipe
              in
              let%bind () =
                Strict_pipe.Writer.write local_diffs_writer
                  ([ Signed_command user_command ], Fn.const ())
              in
              let%map () = Async.Scheduler.yield_until_no_jobs_remain () in
              [%log info] "Checking status" ;
              [%test_eq: State.t] ~equal:State.equal State.Unknown
                ( Or_error.ok_exn
                @@ get_status ~network_id ~frontier_broadcast_pipe
                     ~transaction_pool user_command )))

    let%test_unit "A pending transaction is either in the transition frontier \
                   or transaction pool, but not in the best path of the \
                   transition frontier" =
      Quickcheck.test ~trials:1
        (Quickcheck.Generator.tuple2 gen_frontier gen_user_command)
        ~f:(fun (frontier, user_command) ->
          Async.Thread_safe.block_on_async_exn (fun () ->
              let frontier_broadcast_pipe, _ =
                Broadcast_pipe.create (Some frontier)
              in
              let%bind transaction_pool, local_diffs_writer =
                create_pool ~frontier_broadcast_pipe
              in
              let%bind () =
                Strict_pipe.Writer.write local_diffs_writer
                  ([ Signed_command user_command ], Fn.const ())
              in
              let%map () = Async.Scheduler.yield_until_no_jobs_remain () in
              let status =
                Or_error.ok_exn
                @@ get_status ~network_id ~frontier_broadcast_pipe
                     ~transaction_pool user_command
              in
              [%log info] "Computing status" ;
              [%test_eq: State.t] ~equal:State.equal State.Pending status))

    let%test_unit "An unknown transaction does not appear in the transition \
                   frontier or transaction pool " =
      let user_commands_generator =
        let open Quickcheck.Generator in
        let open Let_syntax in
        let%bind head_user_command = gen_user_command in
        let%map tail_user_commands =
          Quickcheck.Generator.list_with_length 10 gen_user_command
        in
        Non_empty_list.init head_user_command tail_user_commands
      in
      Quickcheck.test ~trials:1
        (Quickcheck.Generator.tuple2 gen_frontier user_commands_generator)
        ~f:(fun (frontier, user_commands) ->
          Async.Thread_safe.block_on_async_exn (fun () ->
              let frontier_broadcast_pipe, _ =
                Broadcast_pipe.create (Some frontier)
              in
              let%bind transaction_pool, local_diffs_writer =
                create_pool ~frontier_broadcast_pipe
              in
              let unknown_user_command, pool_user_commands =
                Non_empty_list.uncons user_commands
              in
              let%bind () =
                Strict_pipe.Writer.write local_diffs_writer
                  ( List.map pool_user_commands ~f:(fun x ->
                        User_command.Signed_command x)
                  , Fn.const () )
              in
              let%map () = Async.Scheduler.yield_until_no_jobs_remain () in
              [%log info] "Computing status" ;
              [%test_eq: State.t] ~equal:State.equal State.Unknown
                ( Or_error.ok_exn
                @@ get_status ~network_id ~frontier_broadcast_pipe
                     ~transaction_pool unknown_user_command )))
  end )
