open Core_kernel
open Async_kernel
open Pipe_lib
open Mina_base
open Signature_lib
open O1trace

type 'a reader_and_writer = 'a Pipe.Reader.t * 'a Pipe.Writer.t

module Optional_public_key = struct
  module T = struct
    type t = Public_key.Compressed.t option [@@deriving hash, compare, sexp]
  end

  include Hashable.Make (T)
end

type t =
  { subscribed_payment_users :
      Signed_command.t reader_and_writer Public_key.Compressed.Table.t
  ; subscribed_block_users :
      (Filtered_external_transition.t, State_hash.t) With_hash.t
      reader_and_writer
      list
      Optional_public_key.Table.t
  ; mutable reorganization_subscription : [ `Changed ] reader_and_writer list
  }

(* idempotent *)
let add_new_subscription (t : t) ~pk =
  (* add a new subscribed block user for this pk if we're not already tracking it *)
  ignore
    ( Optional_public_key.Table.find_or_add t.subscribed_block_users (Some pk)
        ~default:(fun () -> [ Pipe.create () ])
      : (Filtered_external_transition.t, State_hash.t) With_hash.t
        reader_and_writer
        list ) ;
  (* add a new payment user if we're not already tracking it *)
  ignore
    ( Public_key.Compressed.Table.find_or_add t.subscribed_payment_users pk
        ~default:Pipe.create
      : Signed_command.t reader_and_writer )

let create ~logger ~constraint_constants ~wallets ~new_blocks
    ~transition_frontier ~is_storing_all ~time_controller
    ~upload_blocks_to_gcloud ~precomputed_block_writer =
  let subscribed_block_users =
    Optional_public_key.Table.of_alist_multi
    @@ List.map (Secrets.Wallets.pks wallets) ~f:(fun wallet ->
           let reader, writer = Pipe.create () in
           (Some wallet, (reader, writer)))
  in
  let subscribed_payment_users =
    Public_key.Compressed.Table.of_alist_exn
    @@ List.map (Secrets.Wallets.pks wallets) ~f:(fun wallet ->
           let reader, writer = Pipe.create () in
           (wallet, (reader, writer)))
  in
  let update_payment_subscriptions filtered_external_transition participants =
    Set.iter participants ~f:(fun participant ->
        Hashtbl.find_and_call subscribed_payment_users participant
          ~if_not_found:ignore ~if_found:(fun (_, writer) ->
            let user_commands =
              filtered_external_transition
              |> Filtered_external_transition.commands
              |> List.map ~f:(fun { With_status.data; _ } -> data.data)
              |> List.filter_map ~f:(function
                   | User_command.Signed_command c ->
                       Some c
                   | Parties _ ->
                       None)
              |> Fn.flip Signed_command.filter_by_participant participant
            in
            List.iter user_commands ~f:(fun user_command ->
                Pipe.write_without_pushback_if_open writer user_command)))
  in
  let update_block_subscriptions { With_hash.data = external_transition; hash }
      transactions participants =
    Set.iter participants ~f:(fun participant ->
        Hashtbl.find_and_call subscribed_block_users (Some participant)
          ~if_found:(fun pipes ->
            List.iter pipes ~f:(fun (_, writer) ->
                let data =
                  Filtered_external_transition.of_transition external_transition
                    (`Some (Public_key.Compressed.Set.singleton participant))
                    transactions
                in
                Pipe.write_without_pushback_if_open writer
                  { With_hash.data; hash }))
          ~if_not_found:ignore) ;
    Hashtbl.find_and_call subscribed_block_users None
      ~if_found:(fun pipes ->
        List.iter pipes ~f:(fun (_, writer) ->
            let data =
              Filtered_external_transition.of_transition external_transition
                `All transactions
            in
            if not (Pipe.is_closed writer) then
              Pipe.write_without_pushback writer { With_hash.data; hash }))
      ~if_not_found:ignore
  in
  let gcloud_keyfile =
    match Core.Sys.getenv "GCLOUD_KEYFILE" with
    | Some keyfile ->
        Some keyfile
    | _ ->
        [%log warn]
          "GCLOUD_KEYFILE environment variable not set. Must be set to use \
           upload_blocks_to_gcloud" ;
        None
  in
  Option.iter gcloud_keyfile ~f:(fun path ->
      ignore
        ( Core.Sys.command
            (sprintf "gcloud auth activate-service-account --key-file=%s" path)
          : int )) ;
  trace_task "subscriptions new block loop" (fun () ->
      Strict_pipe.Reader.iter new_blocks ~f:(fun new_block ->
          let hash =
            new_block
            |> Mina_transition.External_transition.Validated.state_hash
          in
          (let path, log = !precomputed_block_writer in
           let precomputed_block =
             let open Mina_transition in
             lazy
               (let scheduled_time = Block_time.now time_controller in
                let precomputed_block =
                  new_block |> External_transition.Validated.erase |> fst
                  |> With_hash.data
                  |> External_transition.Precomputed_block
                     .of_external_transition ~scheduled_time
                in
                External_transition.Precomputed_block.to_yojson
                  precomputed_block)
           in
           if upload_blocks_to_gcloud then (
             [%log info] "log" ;
             let json = Yojson.Safe.to_string (Lazy.force precomputed_block) in
             let network =
               match Core.Sys.getenv "NETWORK_NAME" with
               | Some network ->
                   Some network
               | _ ->
                   [%log warn]
                     "NETWORK_NAME environment variable not set. Must be set \
                      to use upload_blocks_to_gcloud" ;
                   None
             in
             let bucket =
               match Core.Sys.getenv "GCLOUD_BLOCK_UPLOAD_BUCKET" with
               | Some bucket ->
                   Some bucket
               | _ ->
                   [%log warn]
                     "GCLOUD_BLOCK_UPLOAD_BUCKET environment variable not set. \
                      Must be set to use upload_blocks_to_gcloud" ;
                   None
             in
             match (gcloud_keyfile, network, bucket) with
             | Some _, Some network, Some bucket ->
                 let hash_string = State_hash.to_string hash in
                 let height =
                   Mina_transition.External_transition.Validated
                   .blockchain_length new_block
                   |> Mina_numbers.Length.to_string
                 in
                 let name =
                   sprintf "%s-%s-%s.json" network height hash_string
                 in
                 (* TODO: Use a pipe to queue this if these are building up *)
                 don't_wait_for
                   ( Mina_metrics.(
                       Gauge.inc_one
                         Block_latency.Upload_to_gcloud.upload_to_gcloud_blocks) ;
                     let tmp_file =
                       Core.Filename.temp_file ~in_dir:"/tmp"
                         "upload_block_file" ""
                     in
                     let f = Stdlib.open_out tmp_file in
                     fprintf f "%s" json ;
                     Stdlib.close_out f ;
                     let command =
                       Printf.sprintf "gsutil cp -n %s gs://%s/%s" tmp_file
                         bucket name
                     in
                     let%map output =
                       (* This double-wrapping of [try_with]s is protection
                          against both immediate exceptions in process setup
                          and exceptions in the 'deferred' part of setup.
                          We also attach 'tags' to the errors below, so that we
                          we have information about which of these different
                          kinds of exception were seen, if any.
                       *)
                       Deferred.Or_error.try_with_join ~here:[%here] (fun () ->
                           Or_error.try_with (fun () ->
                               Async.Process.run () ~prog:"bash"
                                 ~args:[ "-c"; command ]
                               |> Deferred.Result.map_error
                                    ~f:(Error.tag ~tag:__LOC__))
                           |> Result.map_error ~f:(Error.tag ~tag:__LOC__)
                           |> Deferred.return |> Deferred.Or_error.join)
                     in
                     ( match output with
                     | Ok _result ->
                         ()
                     | Error e ->
                         [%log warn]
                           ~metadata:
                             [ ("error", Error_json.error_to_yojson e)
                             ; ("command", `String command)
                             ]
                           "Uploading block to gcloud with command $command \
                            failed: $error" ) ;
                     Sys.remove tmp_file ;
                     Mina_metrics.(
                       Gauge.dec_one
                         Block_latency.Upload_to_gcloud.upload_to_gcloud_blocks)
                   )
             | _ ->
                 () ) ;
           Option.iter path ~f:(fun (`Path path) ->
               Out_channel.with_file ~append:true path ~f:(fun out_channel ->
                   Out_channel.output_lines out_channel
                     [ Yojson.Safe.to_string (Lazy.force precomputed_block) ])) ;
           [%log info] "Saw block with state hash $state_hash"
             ~metadata:
               (let state_hash_data =
                  [ ("state_hash", `String (State_hash.to_base58_check hash)) ]
                in
                if is_some log then
                  state_hash_data
                  @ [ ("precomputed_block", Lazy.force precomputed_block) ]
                else state_hash_data)) ;
          match
            Filtered_external_transition.validate_transactions
              ~constraint_constants new_block
          with
          | Ok verified_transactions ->
              let unfiltered_external_transition =
                lazy
                  (Filtered_external_transition.of_transition new_block `All
                     verified_transactions)
              in
              let filtered_external_transition =
                if is_storing_all then Lazy.force unfiltered_external_transition
                else
                  Filtered_external_transition.of_transition new_block
                    (`Some
                      ( Public_key.Compressed.Set.of_list
                      @@ List.filter_opt (Hashtbl.keys subscribed_block_users)
                      ))
                    verified_transactions
              in
              let participants =
                Filtered_external_transition.participant_pks
                  filtered_external_transition
              in
              update_payment_subscriptions filtered_external_transition
                participants ;
              update_block_subscriptions
                { With_hash.data = new_block; hash }
                verified_transactions participants ;
              Deferred.unit
          | Error e ->
              [%log error]
                ~metadata:
                  [ ( "error"
                    , `String (Staged_ledger.Pre_diff_info.Error.to_string e) )
                  ; ("state_hash", State_hash.to_yojson hash)
                  ]
                "Staged ledger had error with transactions in block for state \
                 $state_hash: $error" ;
              Deferred.unit)) ;
  let reorganization_subscription = [] in
  let reader, writer =
    Strict_pipe.create ~name:"Reorganization subscription"
      Strict_pipe.(Buffered (`Capacity 1, `Overflow Drop_head))
  in
  let t =
    { subscribed_payment_users
    ; subscribed_block_users
    ; reorganization_subscription
    }
  in
  don't_wait_for
  @@ Broadcast_pipe.Reader.iter transition_frontier
       ~f:
         (Option.value_map ~default:Deferred.unit ~f:(fun transition_frontier ->
              let best_tip_diff_pipe =
                Transition_frontier.(
                  Extensions.(
                    get_view_pipe (extensions transition_frontier) Best_tip_diff))
              in
              Broadcast_pipe.Reader.iter best_tip_diff_pipe
                ~f:(fun { reorg_best_tip; _ } ->
                  if reorg_best_tip then Strict_pipe.Writer.write writer () ;
                  Deferred.unit))) ;
  Strict_pipe.Reader.iter reader ~f:(fun () ->
      List.iter t.reorganization_subscription ~f:(fun (_, writer) ->
          if not (Pipe.is_closed writer) then
            Pipe.write_without_pushback writer `Changed) ;
      Deferred.unit)
  |> don't_wait_for ;
  t

(* When you subscribe to a block, you also subscribe to its payments *)
let add_block_subscriber t public_key =
  let block_reader, block_writer = Pipe.create () in
  let rw_pair = (block_reader, block_writer) in
  Hashtbl.add_multi t.subscribed_block_users ~key:public_key ~data:rw_pair ;
  don't_wait_for
  @@ ( Pipe.closed block_reader
     >>| fun () ->
     Hashtbl.change t.subscribed_block_users public_key ~f:(function
       | None ->
           None
       | Some pipes -> (
           match
             List.filter pipes ~f:(fun rw_pair' ->
                 (* Intentionally using pointer equality *)
                 not
                 @@ Tuple2.equal ~eq1:Pipe.equal ~eq2:Pipe.equal rw_pair
                      rw_pair')
           with
           | [] ->
               None
           | l ->
               Some l )) ) ;
  block_reader

let add_payment_subscriber t public_key =
  let payment_reader, payment_writer = Pipe.create () in
  Hashtbl.set t.subscribed_payment_users ~key:public_key
    ~data:(payment_reader, payment_writer) ;
  payment_reader

let add_reorganization_subscriber t =
  let reader, writer = Pipe.create () in
  t.reorganization_subscription <-
    (reader, writer) :: t.reorganization_subscription ;
  reader
