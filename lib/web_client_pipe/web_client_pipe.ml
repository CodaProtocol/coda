open Core
open Async

module Config = struct
  type t = {conf_dir: string; log: Logger.t}
end

module type S = sig
  type t

  type data

  val create : unit -> t Deferred.t

  val store : t -> data -> unit Deferred.t
end

module type Put_request_intf = sig
  type t

  val create : unit -> t Deferred.Or_error.t

  val put : t -> string -> unit Deferred.Or_error.t
end

module type Config_intf = sig
  val conf_dir : string

  val log : Logger.t
end

type chain =
  { protocol_state: Lite_base.Protocol_state.t
  ; proof: Lite_base.Proof.t
  ; ledgers: Lite_lib.Sparse_ledger.t list }
[@@deriving bin_io]

module type Chain_intf = sig
  type data

  val create : data -> chain
end

module Make
    (Chain : Chain_intf)
    (Config : Config_intf)
    (Store : Storage.With_checksum_intf with type location := string)
    (Request : Put_request_intf) :
  S with type data = Chain.data =
struct
  open Lite_base

  type data = Chain.data

  type t =
    { location: string
    ; chain_storage: Lite_chain.t Store.Controller.t
    ; reader: chain Linear_pipe.Reader.t
    ; writer: chain Linear_pipe.Writer.t }

  let write_to_storage {location; chain_storage; _}
      request {protocol_state; proof; ledgers; _} =
    let chain_file = location ^/ "chain" in
    (* HACK: we are just passing in the proposer path for this demo *)
    assert (List.length ledgers = 1);
    let chain = {
      Lite_chain.proof
      ; protocol_state      
      ; ledger = (List.hd_exn ledgers)
    } in
    let%bind () = Store.store chain_storage chain_file chain in
    Request.put request location

  let create () =
    let location = Config.conf_dir ^/ "snarkette-data" in
    let%bind () = Unix.mkdir location ~p:() in
    let parent_log = Config.log in
    let chain_storage = Store.Controller.create ~parent_log Lite_chain.bin_t in
    let reader, writer = Linear_pipe.create () in
    let t =
      { location
      ; chain_storage
      ; reader
      ; writer }
    in
    let%map () =
      match%map Request.create () with
      | Ok request ->
          don't_wait_for
            (Linear_pipe.iter reader ~f:(fun chain ->
                 match%map write_to_storage t request chain with
                 | Ok () -> ()
                 | Error e ->
                     Logger.error Config.log
                       !"Writing data IO_error: %s"
                       (Error.to_string_hum e) ))
      | Error e ->
          Logger.error Config.log
            !"Unable to create request: %s"
            (Error.to_string_hum e)
    in
    t

  let store {reader; writer; _} data =
    let lite_chain = Chain.create data in
    Linear_pipe.force_write_maybe_drop_head ~capacity:1 writer reader
      lite_chain ;
    Deferred.unit
end

module S3_put_request = S3_put_request
