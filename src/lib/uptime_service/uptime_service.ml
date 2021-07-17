(* uptime_service.ml -- proof of uptime for Mina delegation program *)

open Core_kernel
open Async
open Mina_base
open Mina_transition
open Pipe_lib
open Signature_lib

module Blake2 = Blake2.Make ()

module Uptime_snark_worker = Uptime_snark_worker

type block_data =
  { block: string
  ; created_at: string
  ; peer_id: string
  ; snark_work: string option [@default None] }
[@@deriving to_yojson]

module Proof_data = struct
  (* NB: this type is unversioned, so the verifier on the backend
     will need to be using the same code
  *)
  type t =
    { proof: Ledger_proof.Stable.Latest.t
    ; proof_time: Core_kernel.Time.Span.t
    ; snark_work_fee: Currency.Fee.Stable.Latest.t }
  [@@deriving bin_io_unversioned]
end

let external_transition_of_breadcrumb breadcrumb =
  let {With_hash.data= external_transition; _}, _ =
    Transition_frontier.Breadcrumb.validated_transition breadcrumb
    |> External_transition.Validated.erase
  in
  external_transition

let get_rfc3339_time () =
  let tm = Unix.gettimeofday () in
  match Ptime.of_float_s tm with
  | None ->
      (* should never occur *)
      failwith "In uptime service, could not convert current time to Ptime.t"
  | Some ptime ->
      Ptime.to_rfc3339 ~tz_offset_s:0 ptime

let sign_blake2_hash ~private_key s =
  let module Field = Snark_params.Tick.Field in
  let blake2 = Blake2.digest_string s in
  let field_elements = [||] in
  let bitstrings =
    [|Blake2.to_raw_string blake2 |> Blake2.string_to_bits |> Array.to_list|]
  in
  let input : (Field.t, bool) Random_oracle.Input.t =
    {field_elements; bitstrings}
  in
  Schnorr.sign private_key input

let send_uptime_data ~logger ~interruptor ~(submitter_keypair : Keypair.t) ~url
    ~state_hash ~produced block_data =
  let open Interruptible.Let_syntax in
  let make_interruptible f = Interruptible.lift f interruptor in
  let block_data_json = block_data_to_yojson block_data in
  let block_data_string = Yojson.Safe.to_string block_data_json in
  let signature =
    sign_blake2_hash ~private_key:submitter_keypair.private_key
      block_data_string
  in
  let json =
    (* JSON structure in issue #9110 *)
    `Assoc
      [ ("data", block_data_json)
      ; ("signature", Signature.to_yojson signature)
      ; ("submitter", Public_key.to_yojson submitter_keypair.public_key) ]
  in
  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
  match%map
    make_interruptible
      (Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
           let interruptible =
             match%map
               make_interruptible
                 (Cohttp_async.Client.post ~headers
                    ~body:
                      ( Yojson.Safe.to_string json
                      |> Cohttp_async.Body.of_string )
                    url)
             with
             | {status; _}, body ->
                 if Cohttp.Code.code_of_status status = 200 then
                   [%log info]
                     "Sent block with state hash $state_hash to uptime \
                      service at URL $url"
                     ~metadata:
                       [ ("state_hash", State_hash.to_yojson state_hash)
                       ; ( "includes_snark_work"
                         , `Bool (Option.is_some block_data.snark_work) )
                       ; ("is_produced_block", `Bool produced)
                       ; ("url", `String (Uri.to_string url)) ]
                 else
                   let base_metadata =
                     [ ("state_hash", State_hash.to_yojson state_hash)
                     ; ("url", `String (Uri.to_string url))
                     ; ("http_code", `Int (Cohttp.Code.code_of_status status))
                     ; ( "http_error"
                       , `String (Cohttp.Code.string_of_status status) )
                     ; ("payload", json) ]
                   in
                   let extra_metadata =
                     match body with
                     | `String s ->
                         [("error", `String s)]
                     | `Strings ss ->
                         [ ( "error"
                           , `List (List.map ss ~f:(fun s -> `String s)) ) ]
                     | `Empty | `Pipe _ ->
                         []
                   in
                   let metadata = base_metadata @ extra_metadata in
                   [%log error]
                     "Failure when sending block with state hash $state_hash \
                      to uptime service at URL $url"
                     ~metadata
           in
           match%map.Deferred.Let_syntax Interruptible.force interruptible with
           | Ok () ->
               ()
           | Error _ ->
               [%log error]
                 "In uptime service, POST of uptime data was interrupted" ))
  with
  | Ok () ->
      ()
  | Error exn ->
      [%log error]
        "Error when sending block with state hash $state_hash to uptime \
         service at URL $url"
        ~metadata:
          [ ("state_hash", State_hash.to_yojson state_hash)
          ; ("url", `String (Uri.to_string url))
          ; ("payload", json)
          ; ("error", `String (Exn.to_string exn)) ]

let block_base64_of_breadcrumb ~logger breadcrumb =
  let external_transition = external_transition_of_breadcrumb breadcrumb in
  let block_string =
    Binable.to_string
      (module External_transition.Stable.Latest)
      external_transition
  in
  let block_base64_result = Base64.encode block_string in
  match block_base64_result with
  | Ok block_base64 ->
      Some block_base64
  | Error (`Msg err) ->
      [%log error]
        "Could not Base64-encode block with state hash $state_hash for uptime \
         service"
        ~metadata:
          [ ( "state_hash"
            , State_hash.to_yojson
              @@ External_transition.state_hash external_transition )
          ; ("error", `String err) ] ;
      None

let send_produced_block_at ~logger ~interruptor ~url ~peer_id
    ~(submitter_keypair : Keypair.t) ~block_produced_bvar tm =
  let open Interruptible.Let_syntax in
  let make_interruptible f = Interruptible.lift f interruptor in
  let timeout_min = 3.0 in
  let%bind () = make_interruptible (at tm) in
  match%bind
    make_interruptible
      (with_timeout
         (Time.Span.of_min timeout_min)
         (Bvar.wait block_produced_bvar))
  with
  | `Timeout ->
      [%log error]
        "Uptime service did not get a produced block within %0.1f minutes \
         after scheduled time"
        timeout_min ;
      return ()
  | `Result breadcrumb -> (
    match block_base64_of_breadcrumb ~logger breadcrumb with
    | Some block_base64 ->
        let state_hash =
          Transition_frontier.Breadcrumb.state_hash breadcrumb
        in
        let block_data =
          { block= block_base64
          ; created_at= get_rfc3339_time ()
          ; peer_id
          ; snark_work= None }
        in
        send_uptime_data ~logger ~interruptor ~submitter_keypair ~url
          ~state_hash ~produced:true block_data
    | None ->
        return () )

let send_block_and_transaction_snark ~logger ~interruptor ~url ~snark_worker
    ~transition_frontier ~peer_id ~(submitter_keypair : Keypair.t)
    ~snark_work_fee =
  match Broadcast_pipe.Reader.peek transition_frontier with
  | None ->
      (* expected during daemon boot, so not logging as error *)
      [%log info]
        "Transition frontier not available to send a block to uptime service" ;
      Interruptible.return ()
  | Some tf -> (
      let make_interruptible f = Interruptible.lift f interruptor in
      let breadcrumb = Transition_frontier.best_tip tf in
      match block_base64_of_breadcrumb ~logger breadcrumb with
      | None ->
          [%log info]
            "Could not Base64-encode block, not sending block or SNARK work \
             to uptime service" ;
          Interruptible.return ()
      | Some block_base64 -> (
          let open Interruptible.Let_syntax in
          let message =
            Sok_message.create ~fee:snark_work_fee
              ~prover:(Public_key.compress submitter_keypair.public_key)
          in
          let best_tip = Transition_frontier.best_tip tf in
          let external_transition =
            Transition_frontier.Breadcrumb.validated_transition best_tip
            |> External_transition.Validation.forget_validation
          in
          if
            List.is_empty
              (External_transition.transactions
                 ~constraint_constants:
                   Genesis_constants.Constraint_constants.compiled
                 external_transition)
          then (
            [%log info]
              "No transactions in block, sending block without SNARK work to \
               uptime service" ;
            let state_hash =
              Transition_frontier.Breadcrumb.state_hash best_tip
            in
            let block_data =
              { block= block_base64
              ; created_at= get_rfc3339_time ()
              ; peer_id
              ; snark_work= None }
            in
            send_uptime_data ~logger ~interruptor ~submitter_keypair ~url
              ~state_hash ~produced:false block_data )
          else
            let best_tip_staged_ledger =
              Transition_frontier.Breadcrumb.staged_ledger best_tip
            in
            match
              Staged_ledger.all_work_pairs best_tip_staged_ledger
                ~get_state:(fun state_hash ->
                  match
                    Transition_frontier.find_protocol_state tf state_hash
                  with
                  | None ->
                      Error
                        (Error.createf
                           "Could not find state_hash %s in transition \
                            frontier for uptime service"
                           (State_hash.to_base58_check state_hash))
                  | Some protocol_state ->
                      Ok protocol_state )
            with
            | Error e ->
                [%log error]
                  "Could not get SNARK work from best tip staged ledger for \
                   uptime service"
                  ~metadata:[("error", Error_json.error_to_yojson e)] ;
                Interruptible.return ()
            | Ok [] ->
                [%log info]
                  "No SNARK jobs available for uptime service, sending just \
                   the block" ;
                let state_hash =
                  Transition_frontier.Breadcrumb.state_hash best_tip
                in
                let block_data =
                  { block= block_base64
                  ; created_at= get_rfc3339_time ()
                  ; peer_id
                  ; snark_work= None }
                in
                send_uptime_data ~logger ~interruptor ~submitter_keypair ~url
                  ~state_hash ~produced:false block_data
            | Ok job_one_or_twos -> (
                let transitions =
                  List.concat_map job_one_or_twos ~f:One_or_two.to_list
                  |> List.filter ~f:(function
                       | Snark_work_lib.Work.Single.Spec.Transition _ ->
                           true
                       | Merge _ ->
                           false )
                in
                let staged_ledger_hash =
                  Transition_frontier.Breadcrumb.blockchain_state best_tip
                  |> Mina_state.Blockchain_state.staged_ledger_hash
                in
                match
                  List.find transitions ~f:(fun transition ->
                      match transition with
                      | Snark_work_lib.Work.Single.Spec.Transition
                          ({target; _}, _, _) ->
                          Marlin_plonk_bindings_pasta_fp.equal target
                            (Staged_ledger_hash.ledger_hash staged_ledger_hash)
                      | Merge _ ->
                          (* unreachable *)
                          failwith "Expected Transition work, not Merge" )
                with
                | None ->
                    [%log info]
                      "No transactions in block match staged ledger hash, \
                       sending block without SNARK work" ;
                    let state_hash =
                      Transition_frontier.Breadcrumb.state_hash best_tip
                    in
                    let block_data =
                      { block= block_base64
                      ; created_at= get_rfc3339_time ()
                      ; peer_id
                      ; snark_work= None }
                    in
                    send_uptime_data ~logger ~interruptor ~submitter_keypair
                      ~url ~state_hash ~produced:false block_data
                | Some single_spec -> (
                    match%bind
                      make_interruptible
                        (Uptime_snark_worker.perform_single snark_worker
                           (message, single_spec))
                    with
                    | Error e ->
                        (* error in submitting to process *)
                        [%log error]
                          "Error when running uptime service SNARK worker on \
                           a transaction"
                          ~metadata:[("error", Error_json.error_to_yojson e)] ;
                        Interruptible.return ()
                    | Ok (Error e) ->
                        (* error in creating the SNARK work *)
                        [%log error]
                          "Error computing SNARK work for uptime service"
                          ~metadata:[("error", Error_json.error_to_yojson e)] ;
                        Interruptible.return ()
                    | Ok (Ok (proof, proof_time)) -> (
                        let proof_data : Proof_data.t =
                          {proof; proof_time; snark_work_fee}
                        in
                        let proof_string =
                          Binable.to_string (module Proof_data) proof_data
                        in
                        match Base64.encode proof_string with
                        | Error (`Msg err) ->
                            [%log error]
                              "Could not Base64-encode SNARK work for uptime \
                               service"
                              ~metadata:[("error", `String err)] ;
                            Interruptible.return ()
                        | Ok snark_work_base64 ->
                            let state_hash =
                              Transition_frontier.Breadcrumb.state_hash
                                best_tip
                            in
                            let block_data =
                              { block= block_base64
                              ; created_at= get_rfc3339_time ()
                              ; peer_id
                              ; snark_work= Some snark_work_base64 }
                            in
                            send_uptime_data ~logger ~interruptor
                              ~submitter_keypair ~url ~state_hash
                              ~produced:false block_data ) ) ) ) )

let start ~logger ~uptime_url ~snark_worker_opt ~transition_frontier
    ~time_controller ~block_produced_bvar ~uptime_submitter_keypair
    ~get_next_producer_timing ~get_snark_work_fee ~get_peer =
  match uptime_url with
  | None ->
      [%log info] "Not running uptime service, no URL given" ;
      ()
  | Some url ->
      [%log info] "Starting uptime service using URL $url"
        ~metadata:[("url", `String (Uri.to_string url))] ;
      let snark_worker : Uptime_snark_worker.t =
        Option.value_exn snark_worker_opt
      in
      let slot_duration_ms =
        Consensus.Configuration.t
          ~constraint_constants:Genesis_constants.Constraint_constants.compiled
          ~protocol_constants:Genesis_constants.compiled.protocol
        |> Consensus.Configuration.slot_duration |> Float.of_int
      in
      let make_slots_span min =
        Block_time.Span.of_time_span
          (Time.Span.of_ms (slot_duration_ms *. min))
      in
      let five_slots_span = make_slots_span 5.0 in
      let four_slots_span = make_slots_span 4.0 in
      let wait_until_iteration_start block_tm =
        let now = Block_time.now time_controller in
        if Block_time.( < ) now block_tm then at (Block_time.to_time block_tm)
        else (
          [%log warn]
            "In uptime service, current block time is past desired start of \
             iteration" ;
          return () )
      in
      let register_iteration =
        let interrupt_ivar = ref (Ivar.create ()) in
        fun () ->
          (* terminate any Interruptible code from previous iteration *)
          Ivar.fill_if_empty !interrupt_ivar () ;
          Deferred.create (fun ivar -> interrupt_ivar := ivar)
      in
      let run_iteration next_block_tm : Block_time.t Deferred.t =
        let get_next_producer_time_opt () =
          match get_next_producer_timing () with
          | None ->
              [%log trace] "Next producer timing not set for uptime service" ;
              None
          | Some timing -> (
              let open Daemon_rpcs.Types.Status.Next_producer_timing in
              match timing.timing with
              | Check_again _tm ->
                  [%log trace]
                    "Next producer timing not available for uptime service" ;
                  None
              | Produce prod_tm | Produce_now prod_tm ->
                  Some (Block_time.to_time prod_tm.time) )
        in
        [%log trace]
          "Waiting for next 5-slot boundary to start work in uptime service"
          ~metadata:
            [ ("boundary_block_time", Block_time.to_yojson next_block_tm)
            ; ( "boundary_time"
              , `String (Block_time.to_time next_block_tm |> Time.to_string) )
            ] ;
        (* wait in Async.Deferred monad *)
        let%bind () = wait_until_iteration_start next_block_tm in
        (* interrupt previous iteration *)
        let interruptor = register_iteration () in
        (* work in Interruptible monad in "background" *)
        Interruptible.don't_wait_for
          ( [%log trace] "Determining which action to take in uptime service" ;
            match get_peer () with
            | None ->
                [%log warn]
                  "Daemon is not yet a peer in the gossip network, uptime \
                   service not sending a produced block" ;
                Interruptible.return ()
            | Some ({peer_id; _} : Network_peer.Peer.t) -> (
                let submitter_keypair =
                  (* daemon startup checked that a keypair was given if URL given *)
                  Option.value_exn uptime_submitter_keypair
                in
                let send_just_block next_producer_time =
                  [%log info]
                    "Uptime service will attempt to send the next produced \
                     block" ;
                  send_produced_block_at ~logger ~interruptor ~url ~peer_id
                    ~submitter_keypair ~block_produced_bvar next_producer_time
                in
                let send_block_and_snark_work () =
                  [%log info]
                    "Uptime service will attempt to send a block and SNARK work" ;
                  let snark_work_fee = get_snark_work_fee () in
                  send_block_and_transaction_snark ~logger ~interruptor ~url
                    ~snark_worker ~transition_frontier ~peer_id
                    ~submitter_keypair ~snark_work_fee
                in
                match get_next_producer_time_opt () with
                | None ->
                    send_block_and_snark_work ()
                | Some next_producer_time ->
                    (* we look for block production within 4 slots of the *desired*
                       iteration start time, so a late iteration won't affect the
                       time bound on block production
                      that leaves a full slot to produce the block
                   *)
                    let four_slots_from_start =
                      Block_time.add next_block_tm four_slots_span
                      |> Block_time.to_time
                    in
                    if Time.( <= ) next_producer_time four_slots_from_start
                    then send_just_block next_producer_time
                    else send_block_and_snark_work () ) ) ;
        Deferred.return (Block_time.add next_block_tm five_slots_span)
      in
      (* sync to slot boundary *)
      let next_slot_block_time =
        let block_time_ms_int64 =
          Block_time.now time_controller |> Block_time.to_int64
        in
        let slot_duration_ms_int64 = Float.to_int64 slot_duration_ms in
        let next_slot_ms =
          if
            Int64.equal Int64.zero
              (Int64.(rem) block_time_ms_int64 slot_duration_ms_int64)
          then block_time_ms_int64
          else
            let last_slot_no =
              Int64.( / ) block_time_ms_int64 slot_duration_ms_int64
            in
            Int64.( * ) (Int64.succ last_slot_no) slot_duration_ms_int64
        in
        Block_time.of_int64 next_slot_ms
      in
      Async.Deferred.forever next_slot_block_time run_iteration
