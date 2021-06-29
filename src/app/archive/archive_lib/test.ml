open Async
open Core
open Mina_base
open Pipe_lib
open Signature_lib

let%test_module "Archive node unit tests" =
  ( module struct
    let logger = Logger.create ()

    let proof_level = Genesis_constants.Proof_level.None

    let precomputed_values =
      {(Lazy.force Precomputed_values.for_unit_tests) with proof_level}

    let constraint_constants = precomputed_values.constraint_constants

    let verifier =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Verifier.create ~logger ~proof_level ~constraint_constants
            ~conf_dir:None
            ~pids:(Child_processes.Termination.create_pid_table ()) )

    module Genesis_ledger = (val Genesis_ledger.for_unit_tests)

    let archive_uri =
      Uri.of_string "postgres://admin:codarules@localhost:5432/archiver"

    let conn_lazy =
      lazy
        ( Thread_safe.block_on_async_exn
        @@ fun () ->
        match%map Caqti_async.connect archive_uri with
        | Ok conn ->
            conn
        | Error e ->
            failwith @@ Caqti_error.show e )

    let conn_pool_lazy =
      lazy
        ( match Caqti_async.connect_pool archive_uri with
        | Ok pool ->
            pool
        | Error e ->
            failwith @@ Caqti_error.show e )

    let keys = Array.init 5 ~f:(fun _ -> Keypair.create ())

    let user_command_gen =
      User_command.Gen.payment_with_random_participants ~keys ~max_amount:1000
        ~fee_range:10 ()

    let fee_transfer_gen =
      Fee_transfer.Single.Gen.with_random_receivers ~keys ~min_fee:0
        ~max_fee:10
        ~token:(Quickcheck.Generator.return Token_id.default)

    let coinbase_gen =
      Coinbase.Gen.with_random_receivers ~keys ~min_amount:20 ~max_amount:100
        ~fee_transfer:
          (Coinbase.Fee_transfer.Gen.with_random_receivers ~keys
             ~min_fee:Currency.Fee.zero)

    let%test_unit "User_command: read and write" =
      let conn = Lazy.force conn_lazy in
      Thread_safe.block_on_async_exn
      @@ fun () ->
      Async.Quickcheck.async_test ~sexp_of:[%sexp_of: User_command.t]
        user_command_gen ~f:(fun user_command ->
          let transaction_hash = Transaction_hash.hash_command user_command in
          match%map
            let open Deferred.Result.Let_syntax in
            let%bind user_command_id =
              Processor.User_command.add_if_doesn't_exist conn user_command
            in
            let%map result =
              Processor.User_command.find conn ~transaction_hash
            in
            [%test_result: int] ~expect:user_command_id
              (Option.value_exn result)
          with
          | Ok () ->
              ()
          | Error e ->
              failwith @@ Caqti_error.show e )

    let%test_unit "Fee_transfer: read and write" =
      let kind_gen =
        let open Quickcheck.Generator in
        let open Quickcheck.Generator.Let_syntax in
        let%map b = bool in
        if b then `Normal else `Via_coinbase
      in
      let conn = Lazy.force conn_lazy in
      Thread_safe.block_on_async_exn
      @@ fun () ->
      Async.Quickcheck.async_test
        ~sexp_of:[%sexp_of: [`Normal | `Via_coinbase] * Fee_transfer.Single.t]
        (Quickcheck.Generator.tuple2 kind_gen fee_transfer_gen)
        ~f:(fun (kind, fee_transfer) ->
          let transaction_hash =
            Transaction_hash.hash_fee_transfer fee_transfer
          in
          match%map
            let open Deferred.Result.Let_syntax in
            let%bind fee_transfer_id =
              Processor.Fee_transfer.add_if_doesn't_exist conn fee_transfer
                kind
            in
            let%map result =
              Processor.Internal_command.find conn ~transaction_hash
                ~typ:(Processor.Fee_transfer.Kind.to_string kind)
            in
            [%test_result: int] ~expect:fee_transfer_id
              (Option.value_exn result)
          with
          | Ok () ->
              ()
          | Error e ->
              failwith @@ Caqti_error.show e )

    let%test_unit "Coinbase: read and write" =
      let conn = Lazy.force conn_lazy in
      Thread_safe.block_on_async_exn
      @@ fun () ->
      Async.Quickcheck.async_test ~sexp_of:[%sexp_of: Coinbase.t] coinbase_gen
        ~f:(fun coinbase ->
          let transaction_hash = Transaction_hash.hash_coinbase coinbase in
          match%map
            let open Deferred.Result.Let_syntax in
            let%bind coinbase_id =
              Processor.Coinbase.add_if_doesn't_exist conn coinbase
            in
            let%map result =
              Processor.Internal_command.find conn ~transaction_hash
                ~typ:Processor.Coinbase.coinbase_typ
            in
            [%test_result: int] ~expect:coinbase_id (Option.value_exn result)
          with
          | Ok () ->
              ()
          | Error e ->
              failwith @@ Caqti_error.show e )

    let%test_unit "Block: read and write" =
      let pool = Lazy.force conn_pool_lazy in
      Quickcheck.test ~trials:20
        ( Quickcheck.Generator.with_size ~size:10
        @@ Quickcheck_lib.gen_imperative_list
             (Transition_frontier.For_tests.gen_genesis_breadcrumb
                ~precomputed_values ~verifier ())
             (Transition_frontier.Breadcrumb.For_tests.gen_non_deferred
                ?logger:None ~precomputed_values ~verifier ?trust_system:None
                ~accounts_with_secret_keys:(Lazy.force Genesis_ledger.accounts))
        )
        ~f:(fun breadcrumbs ->
          Thread_safe.block_on_async_exn
          @@ fun () ->
          let reader, writer =
            Strict_pipe.create ~name:"archive"
              (Buffered (`Capacity 100, `Overflow Crash))
          in
          let processor_deferred_computation =
            Processor.run
              ~constraint_constants:precomputed_values.constraint_constants
              pool reader ~logger ~delete_older_than:None
          in
          let diffs =
            List.map
              ~f:(fun breadcrumb ->
                Diff.Transition_frontier
                  (Diff.Builder.breadcrumb_added breadcrumb) )
              breadcrumbs
          in
          List.iter diffs ~f:(Strict_pipe.Writer.write writer) ;
          Strict_pipe.Writer.close writer ;
          let%bind () = processor_deferred_computation in
          match%map
            Processor.deferred_result_list_fold breadcrumbs ~init:()
              ~f:(fun () breadcrumb ->
                Caqti_async.Pool.use
                  (fun conn ->
                    let open Deferred.Result.Let_syntax in
                    match%bind
                      Processor.Block.find_opt conn
                        ~state_hash:
                          (Transition_frontier.Breadcrumb.state_hash breadcrumb)
                    with
                    | Some id ->
                        let%bind Processor.Block.{parent_id; _} =
                          Processor.Block.load conn ~id
                        in
                        if
                          Transition_frontier.Breadcrumb.blockchain_length
                            breadcrumb
                          > Unsigned.UInt32.of_int 1
                        then
                          Processor.For_test.assert_parent_exist ~parent_id
                            ~parent_hash:
                              (Transition_frontier.Breadcrumb.parent_hash
                                 breadcrumb)
                            conn
                        else Deferred.Result.return ()
                    | None ->
                        failwith "Failed to find saved block in database" )
                  pool )
          with
          | Ok () ->
              ()
          | Error e ->
              failwith @@ Caqti_error.show e )

    (*
    let%test_unit "Block: read and write with pruning" =
      let conn = Lazy.force conn_lazy in
      Quickcheck.test ~trials:20
        ( Quickcheck.Generator.with_size ~size:10
        @@ Quickcheck_lib.gen_imperative_list
             (Transition_frontier.For_tests.gen_genesis_breadcrumb
                ~precomputed_values ())
             (Transition_frontier.Breadcrumb.For_tests.gen_non_deferred
                ?logger:None ~precomputed_values ~verifier
                ?trust_system:None
                ~accounts_with_secret_keys:(Lazy.force Genesis_ledger.accounts))
        )
        ~f:(fun breadcrumbs ->
          Thread_safe.block_on_async_exn
          @@ fun () ->
          let reader, writer =
            Strict_pipe.create ~name:"archive"
              (Buffered (`Capacity 100, `Overflow Crash))
          in
          let delete_older_than = 7 in
          let processor_deferred_computation =
            Processor.run
              ~constraint_constants:precomputed_values.constraint_constants
              conn reader ~logger ~delete_older_than:(Some delete_older_than)
          in
          let diffs =
            List.map
              ~f:(fun breadcrumb ->
                Diff.Transition_frontier
                  (Diff.Builder.breadcrumb_added breadcrumb) )
              breadcrumbs
          in
          let max_height =
            List.fold ~init:Unsigned.UInt32.zero breadcrumbs
              ~f:(fun prev_max breadcrumb ->
                breadcrumb |> Transition_frontier.Breadcrumb.blockchain_length
                |> Unsigned.UInt32.max prev_max )
          in
          List.iter diffs ~f:(Strict_pipe.Writer.write writer) ;
          Strict_pipe.Writer.close writer ;
          let%bind () = processor_deferred_computation in
          match%map
            Processor.deferred_result_list_fold breadcrumbs ~init:()
              ~f:(fun () breadcrumb ->
                let open Deferred.Result.Let_syntax in
                let height =
                  breadcrumb
                  |> Transition_frontier.Breadcrumb.blockchain_length
                in
                match%bind
                  Processor.Block.find_opt conn
                    ~state_hash:
                      (Transition_frontier.Breadcrumb.state_hash breadcrumb)
                with
                | Some id ->
                    if
                      Unsigned.UInt32.(
                        height < sub max_height (of_int delete_older_than))
                    then
                      Error.raise
                        (Error.createf
                           !"The block with id %i was not pruned correctly: \
                             height %i < max_height %i - delete_older_than %i"
                           id
                           (Unsigned.UInt32.to_int height)
                           (Unsigned.UInt32.to_int max_height)
                           delete_older_than)
                    else
                      let%map.Async () =
                        Deferred.List.iter
                          (Transition_frontier.Breadcrumb.commands breadcrumb)
                          ~f:(fun cmd ->
                            match%map.Async
                              Processor.User_command.find conn
                                ~transaction_hash:
                                  (Transaction_hash.hash_command
                                     (User_command.forget_check cmd.data))
                            with
                            | Ok (Some _) ->
                                ()
                            | Ok None ->
                                Error.raise
                                  (Error.createf
                                     !"The user command %{sexp: \
                                       User_command.t} was pruned when it \
                                       should not have been"
                                     (User_command.forget_check cmd.data))
                            | Error e ->
                                failwith @@ Caqti_error.show e )
                      in
                      Ok ()
                | None ->
                    if
                      Unsigned.UInt32.(
                        height >= sub max_height (of_int delete_older_than))
                    then
                      Error.raise
                        (Error.createf
                           !"A block was pruned incorrectly: height %i >= \
                             max_height %i - delete_older_than %i "
                           (Unsigned.UInt32.to_int height)
                           (Unsigned.UInt32.to_int max_height)
                           delete_older_than)
                    else
                      let%map.Async () =
                        Deferred.List.iter
                          (Transition_frontier.Breadcrumb.commands breadcrumb)
                          ~f:(fun cmd ->
                            match%map.Async
                              Processor.User_command.find conn
                                ~transaction_hash:
                                  (Transaction_hash.hash_command
                                     (User_command.forget_check cmd.data))
                            with
                            | Ok None ->
                                ()
                            | Ok (Some _) ->
                                Error.raise
                                  (Error.createf
                                     !"The user command %{sexp: \
                                       User_command.t} was not pruned when it \
                                       should have been"
                                     (User_command.forget_check cmd.data))
                            | Error e ->
                                failwith @@ Caqti_error.show e )
                      in
                      Ok () )
          with
          | Ok () ->
              ()
          | Error e ->
              failwith @@ Caqti_error.show e )
    *)
  end )
