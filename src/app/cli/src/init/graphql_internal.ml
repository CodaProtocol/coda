(* Adapted from from https://github.com/andreas/ocaml-graphql-server to
 * change the status code of error responses from 500 to 200.
 *
 * Copyright (c) 2016 Andreas Garnæs
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *)

module type HttpBody = sig
  type t

  type +'a io

  val to_string : t -> string io

  val of_string : string -> t
end

module type S = sig
  module IO : Cohttp.S.IO

  type body

  type 'ctx schema

  type response_action =
    [ `Expert of Cohttp.Response.t * (IO.ic -> IO.oc -> unit IO.t)
    | `Response of Cohttp.Response.t * body ]

  type 'conn callback =
    'conn -> Cohttp.Request.t -> body -> response_action IO.t

  val execute_request :
    'ctx schema -> 'ctx -> Cohttp.Request.t -> body -> response_action IO.t

  val make_callback :
    (Cohttp.Request.t -> 'ctx) -> 'ctx schema -> 'conn callback
end

module Option = struct
  let bind t ~f = match t with None -> None | Some x -> f x

  let map t ~f = bind t ~f:(fun x -> Some (f x))

  let first_some t t' = match t with None -> t' | Some _ -> t
end

module Params = struct
  type t =
    { query : string option
    ; variables : (string * Yojson.Basic.t) list option
    ; operation_name : string option
    }

  let empty = { query = None; variables = None; operation_name = None }

  let of_uri_exn uri =
    let variables =
      Uri.get_query_param uri "variables"
      |> Option.map ~f:Yojson.Basic.from_string
      |> Option.map ~f:Yojson.Basic.Util.to_assoc
    in
    { query = Uri.get_query_param uri "query"
    ; variables
    ; operation_name = Uri.get_query_param uri "operationName"
    }

  let of_json_body_exn body =
    if body = "" then empty
    else
      let json = Yojson.Basic.from_string body in
      { query =
          Yojson.Basic.Util.(json |> member "query" |> to_option to_string)
      ; variables =
          Yojson.Basic.Util.(json |> member "variables" |> to_option to_assoc)
      ; operation_name =
          Yojson.Basic.Util.(
            json |> member "operationName" |> to_option to_string)
      }

  let of_graphql_body body =
    { query = Some body; variables = None; operation_name = None }

  let merge t t' =
    { query = Option.first_some t.query t'.query
    ; variables = Option.first_some t.variables t'.variables
    ; operation_name = Option.first_some t.operation_name t'.operation_name
    }

  let post_params_exn req body =
    let headers = Cohttp.Request.headers req in
    match Cohttp.Header.get headers "Content-Type" with
    | Some "application/graphql" ->
        of_graphql_body body
    | Some "application/json" ->
        of_json_body_exn body
    | _ ->
        empty

  let of_req_exn req body =
    let get_params = of_uri_exn (Cohttp.Request.uri req) in
    let post_params = post_params_exn req body in
    merge get_params post_params

  let extract req body =
    try
      let params = of_req_exn req body in
      match params.query with
      | Some query ->
          Ok
            ( query
            , ( params.variables
                :> (string * Graphql_parser.const_value) list option )
            , params.operation_name )
      | None ->
          Error "Must provide query string"
    with Yojson.Json_error msg -> Error msg
end

module Make
    (Schema : Graphql_intf.Schema)
    (Io : Cohttp.S.IO with type 'a t = 'a Schema.Io.t)
    (Body : HttpBody with type +'a io := 'a Schema.Io.t) =
struct
  module Ws = Websocket.Connection.Make (Io)
  module Websocket_transport = Websocket_handler.Make (Schema.Io) (Ws)

  let ( >>= ) = Io.( >>= )

  type response_action =
    [ `Expert of Cohttp.Response.t * (Io.ic -> Io.oc -> unit Io.t)
    | `Response of Cohttp.Response.t * Body.t ]

  type 'conn callback =
    'conn -> Cohttp.Request.t -> Body.t -> response_action Io.t

  let respond_string ~status ~body () =
    Io.return (`Response (Cohttp.Response.make ~status (), Body.of_string body))

  let static_file_response path =
    match Assets.read path with
    | Some body ->
        respond_string ~status:`OK ~body ()
    | None ->
        respond_string ~status:`Not_found ~body:"" ()

  let execute_query ctx schema variables operation_name query =
    match Graphql_parser.parse query with
    | Ok doc ->
        Schema.execute schema ctx ?variables ?operation_name doc
    | Error e ->
        Io.return (Error (`String e))

  let execute_request schema ctx req body =
    Body.to_string body
    >>= fun body_string ->
    match Params.extract req body_string with
    | Error err ->
        respond_string ~status:`Bad_request ~body:err ()
    | Ok (query, variables, operation_name) -> (
        execute_query ctx schema variables operation_name query
        >>= function
        | Ok (`Response data) ->
            let body = Yojson.Basic.to_string data in
            respond_string ~status:`OK ~body ()
        | Ok (`Stream stream) ->
            Schema.Io.Stream.close stream ;
            let body =
              "Subscriptions are only supported via websocket transport"
            in
            respond_string ~status:`Bad_request ~body ()
        | Error err ->
            let body = Yojson.Basic.to_string err in
            respond_string ~status:`OK ~body () )

  let make_callback :
      (Cohttp.Request.t -> 'ctx) -> 'ctx Schema.schema -> 'conn callback =
   fun make_context schema _conn (req : Cohttp.Request.t) body ->
    let req_path = Cohttp.Request.uri req |> Uri.path in
    let path_parts = Astring.String.cuts ~empty:false ~sep:"/" req_path in
    let headers = Cohttp.Request.headers req in
    let accept_html =
      match Cohttp.Header.get headers "accept" with
      | None ->
          false
      | Some s ->
          List.mem "text/html" (String.split_on_char ',' s)
    in
    match (req.meth, path_parts, accept_html) with
    | `GET, [ "graphql" ], true ->
        static_file_response "index_extensions.html"
    | `GET, [ "graphql" ], false ->
        if
          Cohttp.Header.get headers "Connection" = Some "Upgrade"
          && Cohttp.Header.get headers "Upgrade" = Some "websocket"
        then
          let handle_conn =
            Websocket_transport.handle (execute_query (make_context req) schema)
          in
          Io.return (Ws.upgrade_connection req handle_conn)
        else execute_request schema (make_context req) req body
    | `GET, [ "graphql"; path ], _ ->
        static_file_response path
    | `POST, [ "graphql" ], _ ->
        execute_request schema (make_context req) req body
    | _ ->
        respond_string ~status:`Not_found ~body:"" ()
end
