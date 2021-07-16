open Core
open Async
open Signature_lib
open Mina_base

module Client = Graphql_lib.Client.Make (struct
  let preprocess_variables_string = Fn.id

  let headers = String.Map.empty
end)

let run_exn ~f query_obj (uri : Uri.t Cli_lib.Flag.Types.with_name) =
  let log_location_detail =
    if Cli_lib.Flag.Uri.is_localhost uri.value then
      " (in `~/.mina-config/mina.log`)"
    else ""
  in
  match%bind f query_obj uri.value with
  | Ok r ->
      Deferred.return r
  | Error (`Failed_request e) ->
      eprintf
        "Error: Unable to connect to Mina daemon.\n\
         - The daemon might not be running. See logs%s for details.\n\
        \  Run `mina daemon -help` to see how to start daemon.\n\
         - If you just started the daemon, wait a minute for the GraphQL \
         server to start.\n\
         - Alternatively, the daemon may not be running the GraphQL server on \
         %s.\n\
        \  If so, add flag %s with correct port when running this command.\n\
         Error message: %s\n\
         %!"
        log_location_detail (Uri.to_string uri.value) uri.name e ;
      exit 17
  | Error (`Graphql_error e) ->
      eprintf "❌ Error: %s\n" e ;
      exit 17

let query query_obj (uri : Uri.t Cli_lib.Flag.Types.with_name) =
  Client.query query_obj uri.value

let query_exn query_obj port = run_exn ~f:Client.query query_obj port

module Signed_command = struct
  type t =
    { id : string
    ; isDelegation : bool
    ; nonce : int
    ; from : Public_key.Compressed.t
    ; to_ : Public_key.Compressed.t
    ; amount : Currency.Amount.t
    ; fee : Currency.Fee.t
    ; memo : Signed_command_memo.t
    }
  [@@deriving yojson]

  let of_obj x =
    { id = x#id
    ; isDelegation = x#isDelegation
    ; nonce = x#nonce
    ; from = x#from
    ; to_ = x#to_
    ; amount = x#amount
    ; fee = x#fee
    ; memo = x#memo
    }
end
