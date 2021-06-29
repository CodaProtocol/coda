open Async
open Core
open Integration_test_lib.Util
open Integration_test_lib
module Timeout = Timeout_lib.Core_time
module Node = Kubernetes_network.Node

(** This implements Log_engine_intf for stack driver logs for integration tests
    Assumptions:
      1. gcloud is installed and authorized to perform logging and pubsub related changes
      2. gcloud API key is set in the environment variable GCLOUD_API_KEY*)

(*Project Id is required for creating topic, sinks, and subscriptions*)
let project_id = "o1labs-192920"

let prog = "gcloud"

let yojson_from_string json_str =
  Or_error.try_with (fun () -> Yojson.Safe.from_string json_str)

let or_error_list_fold ls ~init ~f =
  let open Or_error.Let_syntax in
  List.fold ls ~init:(return init) ~f:(fun acc_or_error el ->
      let%bind acc = acc_or_error in
      f acc el )

let or_error_list_map ls ~f =
  let open Or_error.Let_syntax in
  or_error_list_fold ls ~init:[] ~f:(fun t el ->
      let%map h = f el in
      h :: t )

let log_filter_of_event_type =
  let open Event_type in
  function
  | Event_type Log_error ->
      [ "jsonPayload.level=(\"Warn\" OR \"Error\" OR \"Faulty_peer\" OR \
         \"Fatal\")" ]
  | Event_type t ->
      let event_id =
        to_structured_event_id (Event_type t)
        |> Option.value_exn
             ~message:
               "could not convert event type into log filter; no structured \
                event id configured"
      in
      let filter =
        Printf.sprintf "jsonPayload.event_id=\"%s\""
          (Structured_log_events.string_of_id event_id)
      in
      [filter]

let all_event_types_log_filter =
  let event_filters =
    List.map Event_type.all_event_types ~f:log_filter_of_event_type
  in
  let nest s = "(" ^ s ^ ")" in
  let disjunction =
    event_filters
    |> List.map ~f:(fun filter ->
           nest (filter |> List.map ~f:nest |> String.concat ~sep:" AND ") )
    |> String.concat ~sep:" OR "
  in
  [disjunction]

module Subscription = struct
  type t = {name: string; topic: string; sink: string}

  (*Using the api endpoint to create a sink instead of the gcloud command
  because the cli doesn't allow setting the writerIdentity account for the sink
  and instead generates an account that doesn't have permissions to publish
  logs to the topic. The account needs to be given permissions explicitly and
  then there's this from the documentation:
    There is a delay between creating the sink and using the sink's new service
     account to authorize writing to the export destination. During the first 24
     hours after sink creation, you might see permission-related error messages
     from the sink on your project's Activity page; you can ignore them.
  *)
  let create_sink ~topic ~filter ~key ~logger name =
    let open Deferred.Or_error.Let_syntax in
    let url =
      "https://logging.googleapis.com/v2/projects/o1labs-192920/sinks?key="
      ^ key
    in
    let%bind authorization =
      let%map token =
        run_cmd_or_error "." prog ["auth"; "print-access-token"]
      in
      let token = String.strip token in
      String.concat ["Authorization: Bearer "; token]
    in
    let req_type = "Accept: application/json" in
    let content_type = "Content-Type: application/json" in
    let destination =
      String.concat ~sep:"/"
        ["pubsub.googleapis.com"; "projects"; project_id; "topics"; topic]
    in
    let header = "--header" in
    let data =
      `Assoc
        [ ("name", `String name)
        ; ("description", `String "Sink for tests")
        ; ("destination", `String destination)
        ; ("filter", `String filter) ]
      |> Yojson.Safe.to_string
    in
    let%bind response =
      run_cmd_or_error "." "curl"
        [ "--silent"
        ; "--show-error"
        ; "--request"
        ; "POST"
        ; url
        ; header
        ; authorization
        ; header
        ; req_type
        ; header
        ; content_type
        ; "--data"
        ; data
        ; "--compressed" ]
    in
    [%log spam] "Create sink response: $response"
      ~metadata:[("response", `String response)] ;
    let%bind response_json = Deferred.return (yojson_from_string response) in
    match
      Yojson.Safe.Util.(to_option Fn.id (member "error" response_json))
    with
    | Some _ ->
        Deferred.Or_error.errorf "Error when creating sink: %s" response
    | None ->
        Deferred.Or_error.ok_unit

  let resource_names name : t =
    let topic = name ^ "_topic" in
    let sink = name ^ "_sink" in
    {name; topic; sink}

  let create ~name ~filter ~logger =
    let open Deferred.Or_error.Let_syntax in
    let gcloud_key_file_env = "GCLOUD_API_KEY" in
    let%bind key =
      match Sys.getenv gcloud_key_file_env with
      | Some key ->
          return key
      | None ->
          Deferred.Or_error.errorf
            "Set environment variable %s with the service account key to use \
             Stackdriver logging"
            gcloud_key_file_env
    in
    let create_topic name =
      run_cmd_or_error "." prog ["pubsub"; "topics"; "create"; name]
    in
    let create_subscription name topic =
      run_cmd_or_error "." prog
        [ "pubsub"
        ; "subscriptions"
        ; "create"
        ; name
        ; "--topic"
        ; topic
        ; "--topic-project"
        ; project_id ]
    in
    let t = resource_names name in
    let%bind _ = create_topic t.topic in
    let%bind _ = create_sink ~topic:t.topic ~filter ~key ~logger t.sink in
    let%map _ = create_subscription name t.topic in
    [%log debug]
      "Succesfully created subscription \"$name\" to topic \"$topic\""
      ~metadata:[("name", `String name); ("topic", `String t.topic)] ;
    t

  let delete t =
    let open Deferred.Let_syntax in
    let%bind delete_subscription_res =
      run_cmd_or_error "." prog
        ["pubsub"; "subscriptions"; "delete"; t.name; "--project"; project_id]
    in
    let%bind delete_sink_res =
      run_cmd_or_error "." prog
        ["logging"; "sinks"; "delete"; t.sink; "--project"; project_id]
    in
    let%map delete_topic_res =
      run_cmd_or_error "." prog
        ["pubsub"; "topics"; "delete"; t.topic; "--project"; project_id]
    in
    Or_error.combine_errors
      [delete_subscription_res; delete_sink_res; delete_topic_res]
    |> Or_error.map ~f:(Fn.const ())

  let cleanup name =
    let t = resource_names name in
    delete t

  let create_with_retry ~name ~filter ~logger =
    let open Deferred.Let_syntax in
    let create () = create ~logger ~name ~filter in
    match%bind create () with
    | Error e ->
        [%log error]
          "Failed to created stackdriver subscription: $error. Cleaning up \
           existing pubsub resources (topic, subscription, sink) and \
           retrying.."
          ~metadata:[("error", `String (Error.to_string_hum e))] ;
        let%bind _ = cleanup name in
        create ()
    | Ok res ->
        Deferred.Or_error.return res

  let pull ~logger t =
    let open Deferred.Or_error.Let_syntax in
    let subscription_id =
      String.concat ~sep:"/" ["projects"; project_id; "subscriptions"; t.name]
    in
    (* The limit for messages we pull on each interval is currently not configurable. For now, it's set to 5 (which will hopefully be a sane for a while). *)
    let%bind result =
      run_cmd_or_error "." prog
        [ "pubsub"
        ; "subscriptions"
        ; "pull"
        ; subscription_id
        ; "--auto-ack"
        ; "--limit"
        ; string_of_int 5
        ; "--format"
        ; "table(DATA)" ]
    in
    [%log spam] "Pull result from stackdriver: $result"
      ~metadata:[("result", `String result)] ;
    match String.split_lines result with
    | [] | ["DATA"] ->
        return []
    | "DATA" :: data ->
        Deferred.return (or_error_list_map data ~f:yojson_from_string)
    | _ ->
        Deferred.Or_error.errorf "Invalid subscription pull result: %s" result
end

type t =
  { logger: Logger.t
  ; subscription: Subscription.t
  ; event_writer: (Node.t * Event_type.event) Pipe.Writer.t
  ; event_reader: (Node.t * Event_type.event) Pipe.Reader.t
  ; background_job: unit Deferred.t }

let event_reader {event_reader; _} = event_reader

let parse_event_from_log_entry ~network log_entry =
  let open Or_error.Let_syntax in
  let open Json_parsing in
  let%bind app_id = find string log_entry ["labels"; "k8s-pod/app"] in
  let%bind node =
    Kubernetes_network.lookup_node_by_app_id network app_id
    |> Option.value_map ~f:Or_error.return
         ~default:
           (Or_error.errorf "failed to find node by pod app id \"%s\"" app_id)
  in
  let%bind log =
    find
      (parser_from_of_yojson Logger.Message.of_yojson)
      log_entry ["jsonPayload"]
  in
  let%map event = Event_type.parse_event log in
  (node, event)

let rec pull_subscription_in_background ~logger ~network ~event_writer
    ~subscription =
  if not (Pipe.is_closed event_writer) then (
    [%log spam] "Pulling StackDriver subscription" ;
    let%bind log_entries =
      Deferred.map (Subscription.pull ~logger subscription) ~f:Or_error.ok_exn
    in
    if List.length log_entries > 0 then
      [%log spam] "Parsing events from $n logs"
        ~metadata:[("n", `Int (List.length log_entries))]
    else [%log spam] "No logs were pulled" ;
    let%bind () =
      Deferred.List.iter ~how:`Sequential log_entries ~f:(fun log_entry ->
          log_entry
          |> parse_event_from_log_entry ~network
          |> Or_error.ok_exn
          |> Pipe.write_without_pushback_if_open event_writer ;
          Deferred.unit )
    in
    let%bind () = after (Time.Span.of_ms 10000.0) in
    pull_subscription_in_background ~logger ~network ~event_writer
      ~subscription )
  else Deferred.unit

let create ~logger ~(network : Kubernetes_network.t) =
  let open Deferred.Or_error.Let_syntax in
  let log_filter =
    let coda_container_filter = "resource.labels.container_name=\"coda\"" in
    let filters =
      [network.testnet_log_filter; coda_container_filter]
      @ all_event_types_log_filter
    in
    String.concat filters ~sep:"\n"
  in
  let%map subscription =
    Subscription.create_with_retry ~logger ~name:network.namespace
      ~filter:log_filter
  in
  [%log info] "Event subscription created" ;
  let event_reader, event_writer = Pipe.create () in
  let background_job =
    pull_subscription_in_background ~logger ~network ~event_writer
      ~subscription
  in
  {logger; subscription; event_reader; event_writer; background_job}

let destroy t : unit Deferred.Or_error.t =
  let open Deferred.Or_error.Let_syntax in
  let {logger; subscription; event_reader= _; event_writer; background_job} =
    t
  in
  Pipe.close event_writer ;
  let%bind () = Deferred.map background_job ~f:Or_error.return in
  let%map () = Subscription.delete subscription in
  [%log debug] "subscription deleted" ;
  ()

(*TODO: unit tests without conencting to gcloud. The following test connects to joyous-occasion*)
(*let%test_module "Log tests" =
  ( module struct
    let logger = Logger.create ()

    let testnet : Kubernetes_network.t =
      let k8 = "k8s_container" in
      let location = "us-east1" in
      let cluster_name = "coda-infra-east" in
      let testnet_name = "joyous-occasion" in
      { block_producers= []
      ; snark_coordinators= []
      ; archive_nodes= []
      ; testnet_log_filter=
          String.concat ~sep:" "
            [ "resource.type="
            ; k8
            ; "resource.labels.location="
            ; location
            ; "resource.labels.cluster_name="
            ; cluster_name
            ; "resource.labels.namespace_name="
            ; testnet_name ] }

    let wait_for_block_height () =
      let open Deferred.Or_error.Let_syntax in
      let%bind log_engine = create ~logger testnet in
      let%bind _ = wait_for ~blocks:2500 log_engine in
      delete log_engine

    let wait_for_slot_timeout () =
      let open Deferred.Or_error.Let_syntax in
      let%bind log_engine = create ~logger testnet in
      let%bind _ = wait_for ~timeout:(`Slots 2) log_engine in
      delete log_engine

    let wait_for_epoch () =
      let open Deferred.Or_error.Let_syntax in
      let%bind log_engine = create ~logger testnet in
      let%bind _ = wait_for ~epoch_reached:16 log_engine in
      delete log_engine

    let test_exn f () =
      let%map res = f () in
      Or_error.ok_exn res

    let%test_unit "joyous-occasion - wait_for_block_height" =
      Async.Thread_safe.block_on_async_exn (test_exn wait_for_block_height)

    let%test_unit "joyous-occasion - wait_for_slot_timeout" =
      Async.Thread_safe.block_on_async_exn (test_exn wait_for_slot_timeout)

    let%test_unit "joyous-occasion - wait_for_epoch" =
      Async.Thread_safe.block_on_async_exn (test_exn wait_for_epoch)
  end )*)
