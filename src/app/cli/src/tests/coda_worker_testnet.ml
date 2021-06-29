open Core
open Async
open Signature_lib
open Mina_base
open Pipe_lib

module Api = struct
  type user_cmd_status = {expected_deadline: int; passed_root: unit Ivar.t}

  type user_cmds_under_inspection = (User_command.t, user_cmd_status) Hashtbl.t

  type restart_type = [`Catchup | `Bootstrap]

  (* TODO: remove status #2336 *)
  type t =
    { workers: Coda_process.t Array.t
    ; configs: Coda_worker.Input.t list
    ; start_writer:
        (int * Coda_worker.Input.t * (unit -> unit) * (unit -> unit))
        Linear_pipe.Writer.t
    ; status:
        [`On of [`Synced of user_cmds_under_inspection | `Catchup] | `Off]
        Array.t
    ; locks: (int ref * unit Condition.t) Array.t
          (** The int counts the number of ongoing RPCs. when it is 0, it is safe to take the worker offline.
        [stop] below will set the status to `Off, and we only try doing an RPC if the status is `On,
        so eventually the counter _must_ become 0, ensuring progress. *)
    ; root_lengths: int Array.t
    ; restart_signals: (restart_type * unit Ivar.t) Option.t Array.t
    ; precomputed_values: Genesis_proof.Inputs.t }

  let create ~precomputed_values configs workers start_writer =
    let status =
      Array.init (Array.length workers) ~f:(fun _ ->
          let user_cmds_under_inspection =
            Hashtbl.create (module User_command)
          in
          `On (`Synced user_cmds_under_inspection) )
    in
    let locks =
      Array.init (Array.length workers) ~f:(fun _ ->
          (ref 0, Condition.create ()) )
    in
    let root_lengths = Array.init (Array.length workers) ~f:(fun _ -> 0) in
    let restart_signals =
      Array.init (Array.length workers) ~f:(fun _ -> None)
    in
    { workers
    ; configs
    ; start_writer
    ; status
    ; locks
    ; root_lengths
    ; restart_signals
    ; precomputed_values }

  let online t i = match t.status.(i) with `On _ -> true | `Off -> false

  let synced t i =
    match t.status.(i) with
    | `On (`Synced _) ->
        true
    | `On `Catchup ->
        false
    | `Off ->
        false

  let run_online_worker ~f t i =
    let worker = t.workers.(i) in
    if online t i then (
      let ongoing_rpcs, cond = t.locks.(i) in
      incr ongoing_rpcs ;
      let%map res = f worker in
      decr ongoing_rpcs ;
      if !ongoing_rpcs = 0 then Condition.broadcast cond () ;
      Some res )
    else return None

  let get_balance t i pk =
    run_online_worker
      ~f:(fun worker -> Coda_process.get_balance_exn worker pk)
      t i

  let get_nonce t i pk =
    run_online_worker
      ~f:(fun worker -> Coda_process.get_nonce_exn worker pk)
      t i

  let best_path t i =
    run_online_worker ~f:(fun worker -> Coda_process.best_path worker) t i

  let sync_status =
    run_online_worker ~f:(fun worker -> Coda_process.sync_status_exn worker)

  let new_user_command t i public_key =
    run_online_worker
      ~f:(fun worker -> Coda_process.new_user_command_exn worker public_key)
      t i

  let start t i =
    Linear_pipe.write t.start_writer
      ( i
      , List.nth_exn t.configs i
      , (fun () -> t.status.(i) <- `On `Catchup)
      , fun () ->
          let user_cmds_under_inspection =
            Hashtbl.create (module User_command)
          in
          t.status.(i) <- `On (`Synced user_cmds_under_inspection) )

  let stop t i ~logger =
    ( match t.status.(i) with
    | `On (`Synced user_cmds_under_inspection) ->
        Hashtbl.iter user_cmds_under_inspection ~f:(fun {passed_root; _} ->
            Ivar.fill passed_root () )
    | _ ->
        () ) ;
    t.status.(i) <- `Off ;
    let ongoing_rpcs, lock = t.locks.(i) in
    let rec wait_for_no_rpcs () =
      if !ongoing_rpcs = 0 then Deferred.unit
      else Deferred.bind (Condition.wait lock) ~f:wait_for_no_rpcs
    in
    let%bind () = wait_for_no_rpcs () in
    Coda_process.disconnect t.workers.(i) ~logger

  let run_user_command ~memo t i (sk : Private_key.t) fee valid_until ~body =
    let open Deferred.Option.Let_syntax in
    let worker = t.workers.(i) in
    let pk_of_sk = Public_key.of_private_key_exn sk |> Public_key.compress in
    let user_command_input =
      User_command_input.create ~signer:pk_of_sk ~fee
        ~fee_token:Token_id.default ~fee_payer_pk:pk_of_sk ~memo ~valid_until
        ~body
        ~sign_choice:
          (User_command_input.Sign_choice.Keypair
             (Keypair.of_private_key_exn sk))
        ()
    in
    let%map user_cmd =
      Coda_process.process_user_command_exn worker user_command_input
      |> Deferred.map ~f:Or_error.ok
    in
    user_cmd

  let delegate_stake t i delegator_sk delegate_pk fee valid_until =
    let delegator =
      Public_key.compress @@ Public_key.of_private_key_exn delegator_sk
    in
    run_user_command
      ~memo:(Signed_command_memo.create_from_string_exn (sprintf "sd%i" i))
      t i delegator_sk fee valid_until
      ~body:
        (Stake_delegation (Set_delegate {delegator; new_delegate= delegate_pk}))

  let send_payment t i sender_sk receiver_pk amount fee valid_until =
    let source_pk =
      Public_key.compress @@ Public_key.of_private_key_exn sender_sk
    in
    run_user_command
      ~memo:(Signed_command_memo.create_from_string_exn (sprintf "pay%i" i))
      t i sender_sk fee valid_until
      ~body:
        (Payment {source_pk; receiver_pk; token_id= Token_id.default; amount})

  let new_block t i key =
    run_online_worker
      ~f:(fun worker -> Coda_process.new_block_exn worker key)
      t i

  let replace_snark_worker_key t i key =
    run_online_worker
      ~f:(fun worker -> Coda_process.replace_snark_worker_key worker key)
      t i

  let validated_transitions_keyswaptest t i =
    run_online_worker
      ~f:(fun worker ->
        Coda_process.validated_transitions_keyswaptest_exn worker )
      t i

  let new_user_command_and_subscribe t i key =
    ignore @@ new_block t i key ;
    new_user_command t i key

  let teardown t ~logger =
    Deferred.Array.iteri ~how:`Parallel t.workers ~f:(fun i _ ->
        stop t i ~logger )

  let setup_bootstrap_signal t i =
    let signal = Ivar.create () in
    t.restart_signals.(i) <- Some (`Bootstrap, signal) ;
    signal

  let setup_catchup_signal t i =
    let signal = Ivar.create () in
    t.restart_signals.(i) <- Some (`Catchup, signal) ;
    signal
end

(** the prefix check keeps track of the "best path" for each worker. the
    best path being the list of state hashes from the root to the best tip.
    the check is satisfied as long as the paths are not disjoint, ie, overlap
    on some node (in the worst case, this will be the root).

    the check will time out and fail if c-1 slots pass without a new block. *)
let start_prefix_check logger workers events testnet ~acceptable_delay =
  let all_transitions_r, all_transitions_w = Linear_pipe.create () in
  let%map chains =
    Deferred.Array.init (Array.length workers) ~f:(fun i ->
        Coda_process.best_path workers.(i) )
  in
  let check_chains (chains : State_hash.Stable.Latest.t list array) =
    let online_chains =
      Array.filteri chains ~f:(fun i el ->
          match el with
          | [] ->
              false
          | [state_hash]
            when State_hash.equal state_hash
                   testnet.Api.precomputed_values.protocol_state_with_hash.hash
            ->
              (* Knowing only the genesis transition doesn't indicate an online
                 chain.
              *)
              false
          | _ ->
              Api.synced testnet i )
    in
    let chain_sets =
      Array.map online_chains
        ~f:(Hash_set.of_list (module State_hash.Stable.Latest))
    in
    let chains_json () =
      `List
        ( Array.to_list online_chains
        |> List.map ~f:(fun chain ->
               `List (List.map ~f:State_hash.Stable.Latest.to_yojson chain) )
        )
    in
    match
      Array.fold ~init:None
        ~f:(fun acc chain ->
          match acc with
          | None ->
              Some chain
          | Some acc ->
              Some (Hash_set.inter acc chain) )
        chain_sets
    with
    | Some hashes_in_common ->
        if Hash_set.is_empty hashes_in_common then
          (let%bind tfs =
             Deferred.Array.map workers ~f:Coda_process.dump_tf
             >>| Array.to_list
           in
           [%log fatal]
             "Best paths have diverged completely, network is forked"
             ~metadata:
               [ ("chains", chains_json ())
               ; ("tf_vizs", `List (List.map ~f:(fun s -> `String s) tfs)) ] ;
           exit 7)
          |> don't_wait_for
        else
          [%log info] "Chains are OK, they have hashes $hashes in common"
            ~metadata:
              [ ( "hashes"
                , `List
                    ( Hash_set.to_list hashes_in_common
                    |> List.map ~f:State_hash.Stable.Latest.to_yojson ) )
              ; ("chains", chains_json ())
              ; ( "root_lengths"
                , `List
                    ( List.map ~f:(fun l -> `Int l)
                    @@ Array.to_list testnet.root_lengths ) ) ]
    | None ->
        [%log warn]
          "Empty list of online chains, OK if we're still starting the network"
          ~metadata:[("chains", chains_json ())]
  in
  let last_time = ref (Time.now ()) in
  don't_wait_for
    (let epsilon = 1.0 in
     let rec go () =
       let diff = Time.diff (Time.now ()) !last_time in
       let diff = Time.Span.to_sec diff in
       if
         Array.existsi testnet.status ~f:(fun i _ -> Api.synced testnet i)
         && not
              Float.(
                diff
                < Time.Span.to_sec acceptable_delay
                  +. epsilon
                  +. Int.to_float
                       Int.(
                         (8 - 1)
                         * testnet.precomputed_values.constraint_constants
                             .block_window_duration_ms)
                     /. 1000.)
       then (
         [%log fatal] "No recent blocks" ;
         ignore (exit 8) ) ;
       let%bind () = after (Time.Span.of_sec 1.0) in
       go ()
     in
     go ()) ;
  don't_wait_for
    (Deferred.ignore_m
       (Linear_pipe.fold ~init:chains all_transitions_r
          ~f:(fun chains (_, _, i) ->
            let%map path = Api.best_path testnet i in
            Option.value_map path ~default:chains ~f:(fun path ->
                chains.(i) <- path ;
                last_time := Time.now () ;
                check_chains chains ;
                chains ) ))) ;
  don't_wait_for
    (Linear_pipe.iter events ~f:(function `Transition (i, (prev, curr)) ->
         Linear_pipe.write all_transitions_w (prev, curr, i) ))

type user_cmd_status =
  {snapshots: int option array; passed_root: bool array; result: unit Ivar.t}

let start_payment_check logger root_pipe (testnet : Api.t) =
  don't_wait_for
    (Linear_pipe.iter root_pipe ~f:(function
         | `Root (worker_id, ({commands; root_length} : Mina_lib.Root_diff.t))
         ->
         ( match testnet.status.(worker_id) with
         | `On (`Synced user_cmds_under_inspection) ->
             testnet.root_lengths.(worker_id) <- root_length ;
             Array.iteri testnet.restart_signals ~f:(fun i -> function
               | None ->
                   ()
               | Some (`Bootstrap, signal) ->
                   if
                     testnet.root_lengths.(i)
                     + 2
                       * Unsigned.UInt32.to_int
                           testnet.precomputed_values.consensus_constants.k
                     + ( Unsigned.UInt32.to_int
                           testnet.precomputed_values.consensus_constants.delta
                       + 1 )
                     < root_length - 2
                   then (
                     Ivar.fill signal () ;
                     testnet.restart_signals.(i) <- None )
                   else ()
               | Some (`Catchup, signal) ->
                   if
                     testnet.root_lengths.(i)
                     + Unsigned.UInt32.to_int
                         testnet.precomputed_values.consensus_constants.k
                       / 2
                     < root_length - 1
                   then (
                     [%log info] "Filled catchup ivar" ;
                     Ivar.fill signal () ;
                     testnet.restart_signals.(i) <- None )
                   else () ) ;
             let earliest_user_cmd =
               List.min_elt (Hashtbl.to_alist user_cmds_under_inspection)
                 ~compare:(fun (_user_cmd1, status1) (_user_cmd2, status2) ->
                   Int.compare status1.expected_deadline
                     status2.expected_deadline )
             in
             Option.iter earliest_user_cmd
               ~f:(fun (user_cmd, {expected_deadline; _}) ->
                 if expected_deadline < root_length then (
                   [%log fatal]
                     ~metadata:
                       [ ("worker_id", `Int worker_id)
                       ; ("user_cmd", User_command.to_yojson user_cmd) ]
                     "Transaction $user_cmd took too long to get into the \
                      root of node $worker_id. Length expected: %d got: %d"
                     expected_deadline root_length ;
                   exit 9 |> ignore ) ) ;
             List.iter commands ~f:(fun user_cmd ->
                 Hashtbl.change user_cmds_under_inspection user_cmd.data
                   ~f:(function
                   | Some {passed_root; _} ->
                       Ivar.fill passed_root () ;
                       [%log info]
                         ~metadata:
                           [ ("user_cmd", User_command.to_yojson user_cmd.data)
                           ; ("worker_id", `Int worker_id)
                           ; ("length", `Int root_length) ]
                         "Transaction $user_cmd finally gets into the root of \
                          node $worker_id, when root length is $length" ;
                       None
                   | None ->
                       None ) ) ;
             Deferred.unit
         | _ ->
             Deferred.unit ) ))

let events ~(precomputed_values : Genesis_proof.Inputs.t) workers start_reader
    =
  let event_r, event_w = Linear_pipe.create () in
  let root_r, root_w = Linear_pipe.create () in
  let connect_worker i worker =
    let%bind transitions = Coda_process.verified_transitions_exn worker in
    let%bind roots = Coda_process.root_diff_exn worker in
    Linear_pipe.transfer transitions event_w ~f:(fun t -> `Transition (i, t))
    |> don't_wait_for ;
    Linear_pipe.transfer roots root_w ~f:(fun r -> `Root (i, r))
  in
  don't_wait_for
    (Linear_pipe.iter start_reader ~f:(fun (i, config, started, synced) ->
         don't_wait_for
           (let%bind worker = Coda_process.spawn_exn config in
            let%bind () = Coda_process.start_exn worker in
            workers.(i) <- worker ;
            started () ;
            don't_wait_for
              (let%bind () =
                 Coda_process.initialization_finish_signal_exn worker
                 >>= Linear_pipe.read >>| ignore
               in
               let ms_to_sync =
                 ( Unsigned.UInt32.to_int
                     precomputed_values.consensus_constants.delta
                 + 1 )
                 * precomputed_values.constraint_constants
                     .block_window_duration_ms
                 + 6_000
                 |> Float.of_int
               in
               let%map () = after (Time.Span.of_ms ms_to_sync) in
               synced ()) ;
            connect_worker i worker) ;
         Deferred.unit )) ;
  Array.iteri workers ~f:(fun i w -> don't_wait_for (connect_worker i w)) ;
  (event_r, root_r)

let start_checks logger (workers : Coda_process.t array) start_reader
    (testnet : Api.t) ~acceptable_delay =
  let event_reader, root_reader =
    events ~precomputed_values:testnet.precomputed_values workers start_reader
  in
  let%bind initialization_finish_signals =
    Deferred.Array.map workers ~f:(fun worker ->
        Coda_process.initialization_finish_signal_exn worker )
  in
  [%log info] "downloaded initialization signal" ;
  let%map () =
    Deferred.all_unit
      (List.map (Array.to_list initialization_finish_signals) ~f:(fun p ->
           Linear_pipe.read p >>| ignore ))
  in
  [%log info] "initialization finishes, start check" ;
  don't_wait_for
    (start_prefix_check logger workers event_reader testnet ~acceptable_delay) ;
  start_payment_check logger root_reader testnet

(* note: this is very declarative, maybe this should be more imperative? *)
(* next steps:
   *   add more powerful api hooks to enable sending payments on certain conditions
   *   implement stop/start
   *   change live whether nodes are producing, snark producing
   *   change network connectivity *)
let test ?archive_process_location ?is_archive_rocksdb ~name logger n
    block_production_keys snark_work_public_keys work_selection_method
    ~max_concurrent_connections ~(precomputed_values : Genesis_proof.Inputs.t)
    =
  let logger = Logger.extend logger [("worker_testnet", `Bool true)] in
  let block_production_interval =
    precomputed_values.constraint_constants.block_window_duration_ms
  in
  let acceptable_delay =
    Time.Span.of_ms
      ( block_production_interval
        * ( Unsigned.UInt32.to_int precomputed_values.consensus_constants.delta
          + 1 )
      |> Float.of_int )
  in
  let%bind program_dir = Unix.getcwd () in
  Coda_processes.init () ;
  let runtime_config = precomputed_values.runtime_config in
  let%bind configs =
    Coda_processes.local_configs n ~block_production_interval ~program_dir
      ~block_production_keys ~acceptable_delay ~chain_id:name
      ~snark_worker_public_keys:(Some (List.init n ~f:snark_work_public_keys))
      ~work_selection_method
      ~trace_dir:(Unix.getenv "CODA_TRACING")
      ~max_concurrent_connections ?is_archive_rocksdb ?archive_process_location
      ~runtime_config
  in
  let%bind workers = Coda_processes.spawn_local_processes_exn configs in
  let workers = List.to_array workers in
  let start_reader, start_writer = Linear_pipe.create () in
  let testnet = Api.create ~precomputed_values configs workers start_writer in
  let%map () =
    start_checks logger workers start_reader testnet ~acceptable_delay
  in
  testnet

module Delegation : sig
  val delegate_stake :
       ?acceptable_delay:int
    -> Api.t
    -> node:int
    -> delegator:Private_key.t
    -> delegatee:Account.key
    -> unit Deferred.t
end = struct
  let delegate_stake ?acceptable_delay:(delay = 7) (testnet : Api.t) ~node
      ~delegator ~delegatee =
    let valid_until = None in
    let fee = Signed_command.minimum_fee in
    let worker = testnet.workers.(node) in
    let%bind _ =
      let open Deferred.Option.Let_syntax in
      let%bind user_cmd =
        Api.delegate_stake testnet node delegator delegatee fee valid_until
      in
      let%map (all_passed_root : unit Ivar.t list) =
        let open Deferred.Let_syntax in
        Deferred.List.filter_map (testnet.status |> Array.to_list) ~f:(function
          | `On (`Synced user_cmds_under_inspection) ->
              let%map root_length = Coda_process.root_length_exn worker in
              let passed_root = Ivar.create () in
              Hashtbl.add_exn user_cmds_under_inspection
                ~key:(Signed_command user_cmd)
                ~data:
                  { expected_deadline=
                      root_length
                      + Unsigned.UInt32.to_int
                          testnet.precomputed_values.consensus_constants.k
                      + delay
                  ; passed_root } ;
              Option.return passed_root
          | _ ->
              return None )
        >>| Option.return
      in
      Deferred.List.iter all_passed_root ~f:Ivar.read
    in
    Deferred.unit
end

module Payments : sig
  val send_several_payments :
       ?acceptable_delay:int
    -> Api.t
    -> node:int
    -> keypairs:Keypair.t list
    -> n:int
    -> unit Deferred.t

  val send_batch_consecutive_payments :
       Api.t
    -> node:int
    -> sender:Private_key.t
    -> keypairs:Keypair.t list
    -> n:int
    -> Signed_command.t list Deferred.t
end = struct
  let send_several_payments ?acceptable_delay:(delay = 7) (testnet : Api.t)
      ~node ~keypairs ~n =
    let amount = Currency.Amount.of_int 10 in
    let valid_until = None in
    let fee = Signed_command.minimum_fee in
    let%bind (_ : unit option list) =
      Deferred.List.init n ~f:(fun _ ->
          let open Deferred.Option.Let_syntax in
          let%bind all_passed_root's =
            List.map
              (keypairs : Keypair.t list)
              ~f:(fun sender_keypair ->
                let receiver_keypair = List.random_element_exn keypairs in
                let sender_sk = sender_keypair.private_key in
                let receiver_pk =
                  receiver_keypair.public_key |> Public_key.compress
                in
                let worker = testnet.workers.(node) in
                let%bind user_cmd =
                  Api.send_payment testnet node sender_sk receiver_pk amount
                    fee valid_until
                in
                let%map (all_passed_root : unit Ivar.t list) =
                  let open Deferred.Let_syntax in
                  Deferred.List.filter_map (testnet.status |> Array.to_list)
                    ~f:(function
                    | `On (`Synced user_cmds_under_inspection) ->
                        let%map root_length =
                          Coda_process.root_length_exn worker
                        in
                        let passed_root = Ivar.create () in
                        (* since amount, fee, valid_until fixed for all commands,
                           might have duplicate commands if there are key duplicates
                        *)
                        ignore
                          (Hashtbl.add user_cmds_under_inspection
                             ~key:(Signed_command user_cmd)
                             ~data:
                               { expected_deadline=
                                   root_length
                                   + Unsigned.UInt32.to_int
                                       testnet.precomputed_values
                                         .consensus_constants
                                         .k
                                   + delay
                               ; passed_root }) ;
                        Option.return passed_root
                    | _ ->
                        return None )
                  >>| Option.return
                in
                all_passed_root )
            |> Deferred.Option.all
          in
          Deferred.map
            (Deferred.List.iter (List.concat all_passed_root's) ~f:Ivar.read)
            ~f:(Fn.const (Some ())) )
    in
    Deferred.unit

  (* TODO: code should be flexible enough even when bootstrapping.
     This is most appropriate todo when #2336 is completed *)
  let send_batch_consecutive_payments (testnet : Api.t) ~node ~sender
      ~(keypairs : Keypair.t list) ~n =
    let amount = Currency.Amount.of_int 10 in
    let fee = Signed_command.minimum_fee in
    let valid_until = None in
    let%bind new_payment_readers =
      Deferred.List.init (Array.length testnet.workers) ~f:(fun i ->
          let pk = Public_key.(compress @@ of_private_key_exn sender) in
          let%map pipe = Api.new_user_command_and_subscribe testnet i pk in
          Option.value_exn pipe )
    in
    Deferred.List.init ~how:`Sequential n ~f:(fun _i ->
        let receiver_keypair = List.random_element_exn keypairs in
        let receiver_pk = receiver_keypair.public_key |> Public_key.compress in
        (* Everybody will be watching for a payment *)
        let%bind user_command =
          let%map payment =
            Api.send_payment testnet node sender receiver_pk amount fee
              valid_until
          in
          Option.value_exn payment
        in
        let rec read_until_match reader =
          match%bind Pipe.read reader with
          | `Eof ->
              Deferred.return false
          | `Ok matching_user_command
            when Signed_command.equal matching_user_command user_command ->
              Deferred.return true
          | `Ok _bad_user_command ->
              read_until_match reader
        in
        let%map result =
          Deferred.List.for_all new_payment_readers ~f:read_until_match
        in
        assert result ;
        user_command )
end

module Restarts : sig
  val restart_node :
       Api.t
    -> logger:Logger.t
    -> node:int
    -> duration:Time.Span.t
    -> unit Deferred.t

  val trigger_catchup : Api.t -> logger:Logger.t -> node:int -> unit Deferred.t

  val trigger_bootstrap :
    Api.t -> logger:Logger.t -> node:int -> unit Deferred.t
end = struct
  let restart_node testnet ~logger ~node ~duration =
    let%bind () = after (Time.Span.of_sec 5.) in
    [%log info] "Stopping node %d" node ;
    let%bind () = Api.stop testnet node ~logger in
    let%bind () = after duration in
    [%log info] "Triggering restart on %d" node ;
    Api.start testnet node

  let trigger_catchup testnet ~logger ~node =
    let%bind () = after (Time.Span.of_sec 5.) in
    [%log info] "Stopping node %d" node ;
    let%bind () = Api.stop testnet node ~logger in
    let signal = Api.setup_catchup_signal testnet node in
    let%bind () = Ivar.read signal in
    [%log info] "Triggering catchup on %d" node ;
    Api.start testnet node

  let trigger_bootstrap testnet ~logger ~node =
    let%bind () = after (Time.Span.of_sec 5.) in
    [%log info] "Stopping node %d" node ;
    let%bind () = Api.stop testnet node ~logger in
    let signal = Api.setup_bootstrap_signal testnet node in
    let%bind () = Ivar.read signal in
    [%log info] "Triggering bootstrap on node %d" node ;
    Api.start testnet node
end
