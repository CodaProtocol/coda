open Core
open Async
open Cohttp_async

type ip_service = {uri: string; body_handler: string -> string}

(* TODO: Make these requests over https: https://github.com/CodaProtocol/coda/issues/4019*)
let services =
  [ {uri= "http://api.ipify.org"; body_handler= Fn.id}
  ; {uri= "http://bot.whatismyipaddress.com"; body_handler= Fn.id}
  ; { uri= "http://ifconfig.co/ip"
    ; body_handler= String.rstrip ~drop:(Char.equal '\n') } ]

let ip_service_result {uri; body_handler} ~logger =
  match%map
    Monitor.try_with ~here:[%here] (fun () ->
        let%bind resp, body = Client.get (Uri.of_string uri) in
        let%map body = Body.to_string body in
        match resp.status with `OK -> Some (body_handler body) | _ -> None )
  with
  | Ok v ->
      v
  | Error e ->
      [%log error] "Failed to query our own IP from $provider: $exn"
        ~metadata:
          [("exn", `String (Exn.to_string e)); ("provider", `String uri)] ;
      None

let find ~logger =
  let handler acc elem =
    match acc with
    | None ->
        ip_service_result elem ~logger
    | Some _ ->
        return acc
  in
  let%map our_ip_maybe = Deferred.List.fold services ~init:None ~f:handler in
  Unix.Inet_addr.of_string
  @@ Option.value_exn
       ~message:
         "Couldn't determine our IP from the internet, use -external-ip flag"
       our_ip_maybe
