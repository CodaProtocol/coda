open Core
open Async

module type Config_intf = sig
  val headers : string String.Map.t

  val preprocess_variables_string : string -> string
end

let make_local_uri port address =
  Uri.of_string ("http://localhost:" ^ string_of_int port ^/ address)

module type S = sig
  val query_or_error :
       < parse: Yojson.Basic.t -> 'response
       ; query: string
       ; variables: Yojson.Basic.t >
    -> int
    -> 'response Deferred.Or_error.t

  val query :
       < parse: Yojson.Basic.t -> 'response
       ; query: string
       ; variables: Yojson.Basic.t >
    -> int
    -> 'response Deferred.t
end

let graphql_error_to_string e =
  let error_obj_to_string obj =
    let open Yojson.Basic in
    let obj_message =
      let open Option.Let_syntax in
      let%bind message = Util.to_option (Util.member "message") obj in
      let%map path = Util.to_option (Util.member "path") obj in
      let message =
        Util.to_string_option message
        |> Option.value ~default:(Yojson.Basic.to_string message)
      in
      Printf.sprintf "%s (in %s)" message (Yojson.Basic.to_string path)
    in
    match obj_message with Some m -> m | None -> to_string obj
  in
  match e with
  | `List l ->
      List.map ~f:error_obj_to_string l |> String.concat ~sep:"\n"
  | e ->
      error_obj_to_string e

module Connection_error = struct
  type t = [`Failed_request of Error.t | `Graphql_error of Error.t]

  let ok_exn = function
    | `Failed_request e ->
        eprintf "❌ Error connecting to graphql server. Error message: %s\n%!"
          e ;
        exit 17
    | `Graphql_error e ->
        eprintf "❌ Error: %s\n" e ;
        exit 17

  let to_error = function
    | `Failed_request e ->
        Error.createf !"Failed Request: %s" e
    | `Graphql_error e ->
        Error.createf !"Graphql error: %s" e
end

module Make (Config : Config_intf) = struct
  let query query_obj uri =
    let variables_string =
      Config.preprocess_variables_string
      @@ Yojson.Basic.to_string query_obj#variables
    in
    let body_string =
      Printf.sprintf {|{"query": "%s", "variables": %s}|} query_obj#query
        variables_string
    in
    let open Deferred.Result.Let_syntax in
    let headers =
      List.fold ~init:(Cohttp.Header.init ())
        ( ("Accept", "application/json")
        :: ("Content-Type", "application/json")
        :: Map.to_alist Config.headers ) ~f:(fun header (key, value) ->
          Cohttp.Header.add header key value )
    in
    let%bind response, body =
      Deferred.Or_error.try_with ~here:[%here] ~extract_exn:true (fun () ->
          Cohttp_async.Client.post ~headers
            ~body:(Cohttp_async.Body.of_string body_string)
            uri )
      |> Deferred.Result.map_error ~f:(fun e ->
             `Failed_request (Error.to_string_hum e) )
    in
    let%bind body_str =
      Cohttp_async.Body.to_string body |> Deferred.map ~f:Result.return
    in
    let%bind body_json =
      match
        Cohttp.Code.code_of_status (Cohttp_async.Response.status response)
      with
      | 200 ->
          Deferred.return (Ok (Yojson.Basic.from_string body_str))
      | code ->
          Deferred.return
            (Error
               (`Failed_request
                 (Printf.sprintf "Status code %d -- %s" code body_str)))
    in
    let open Yojson.Basic.Util in
    ( match (member "errors" body_json, member "data" body_json) with
    | `Null, `Null ->
        Error (`Graphql_error "Empty response from graphql query")
    | error, `Null ->
        Error (`Graphql_error (graphql_error_to_string error))
    | _, raw_json ->
        Result.try_with (fun () -> query_obj#parse raw_json)
        |> Result.map_error ~f:(fun e ->
               `Graphql_error
                 (Printf.sprintf
                    "Problem parsing graphql response\nError message: %s"
                    (Exn.to_string e)) ) )
    |> Deferred.return

  let query_exn query_obj port =
    let open Deferred.Let_syntax in
    match%bind query query_obj port with
    | Ok r ->
        Deferred.return r
    | Error error ->
        Connection_error.ok_exn error
end
