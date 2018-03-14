open Async_kernel
open Core_kernel

module type S = sig
  type t

  val connect
    : initial_peers:Peer.t list -> me:Peer.t -> parent_log:Logger.t -> t Deferred.Or_error.t

  val peers : t -> Peer.t list

  val first_peers : t -> Peer.t list Deferred.t

  val changes : t -> Peer.Event.t Linear_pipe.Reader.t

  val stop : t -> unit Deferred.t
end

module Haskell_process = struct
  open Async
  type t = Process.t

  let kill t =
    let%map _ = Process.run_exn ~prog:"kill" ~args:[Pid.to_string (Process.pid t)] () in
    ()

  (* HACK:
    * We "killall kademlia" immediately before starting up the kademlia process
    *
    * The right way to handle this is to have some option for the Process
    * to automatically cleanup after itself. ie: When this OCaml process dies,
    * the process we spawned is also killed.
    *
    * Core's "Process" module doesn't do this for us. We'll need to write
    * something custom. See issue #125
    *)
  let kill_kademlias () =
    let open Deferred.Let_syntax in
    match%map Process.run ~prog:"killall" ~args:["kademlia"] () with
    | Ok _ -> Ok ()
    | Error _ -> Ok ()

  let cli_format (addr : Host_and_port.t) : string =
    Printf.sprintf "(\"%s\", %d)" (Host_and_port.host addr) (Host_and_port.port addr)

  let create ~initial_peers ~me ~log =
    let open Deferred.Or_error.Let_syntax in
    let args = ["test"; cli_format me] @ (List.map initial_peers ~f:cli_format) in
    Logger.debug log "Args: %s\n" (List.sexp_of_t String.sexp_of_t args |> Sexp.to_string_hum);
    let%bind () = kill_kademlias () in
    (* This is where nix dumps the haskell artifact *)
    let kademlia_binary = "app/kademlia-haskell/result/bin/kademlia" in
    let%map p = Process.create ~prog:kademlia_binary ~args () in
    don't_wait_for begin
      Pipe.iter_without_pushback (Reader.pipe (Process.stderr p)) ~f:(fun str ->
        Logger.error log "%s" str
      )
    end;
    p

  let output t ~log =
    Pipe.map (Reader.pipe (Process.stdout t)) ~f:(fun str ->
      List.filter_map (String.split_lines str) ~f:(fun line ->
        let prefix_name_size = 4 in
        let prefix_size = prefix_name_size + 2 in (* a colon and a space *)
        let prefix = String.prefix line prefix_name_size in
        let line_no_prefix = String.slice line prefix_size (String.length line) in
        match prefix with
        | "DBUG" -> Logger.debug log "%s" line_no_prefix; None
        | "EROR" -> Logger.error log "%s" line_no_prefix; None
        | "DATA" -> Logger.info log "%s" line_no_prefix; Some line_no_prefix
        | _ -> Logger.warn log "Unexpected output from Kademlia Haskell: %s" line; None
      )
    )
end

module Make (P : sig
  type t

  val kill : t -> unit Deferred.t
  val create : initial_peers:Peer.t list -> me:Peer.t -> log:Logger.t -> t Deferred.Or_error.t
  val output : t -> log:Logger.t -> string list Pipe.Reader.t
end) : S = struct
  open Async

  type t =
    { p : P.t
    ; peers : string Peer.Table.t
    ; changes_reader : Peer.Event.t Linear_pipe.Reader.t
    ; changes_writer : Peer.Event.t Linear_pipe.Writer.t
    ; first_peers : Peer.t list Deferred.t
    }

  let live t lives =
    List.iter lives ~f:(fun (peer, kkey) ->
      let _ = Peer.Table.add ~key:peer ~data:kkey t.peers in
      ()
    );
    if List.length lives > 0 then
      Linear_pipe.write_or_drop
        ~capacity:500
        t.changes_writer
        t.changes_reader
        (Peer.Event.Connect (List.map lives ~f:fst))
    else ()

  let dead t deads =
    List.iter deads ~f:(fun peer ->
      Peer.Table.remove t.peers peer;
    );
    if List.length deads > 0 then
      Linear_pipe.write_or_drop
        ~capacity:500
        t.changes_writer
        t.changes_reader
        (Peer.Event.Disconnect deads)
    else ()

  let connect ~initial_peers ~me ~parent_log =
    let open Deferred.Or_error.Let_syntax in
    let log = Logger.child parent_log "membership" in
    let%map p = P.create ~initial_peers ~me ~log in
    let peers = Peer.Table.create () in
    let (changes_reader, changes_writer) = Linear_pipe.create () in
    let first_peers_ivar = ref None in
    let first_peers = Deferred.create (fun ivar ->
      first_peers_ivar := Some ivar
    ) in
    let t =
      { p ; peers ; changes_reader ; changes_writer ; first_peers }
    in
    don't_wait_for begin
      Pipe.iter_without_pushback (P.output p ~log) ~f:(fun lines ->
        let (lives, deads) = List.partition_map lines ~f:(fun line ->
          match (String.split ~on:' ' line) with
          | [addr; kademliaKey; "on"] ->
            `Fst (Host_and_port.of_string addr, kademliaKey)
          | [addr; kademliaKey; "off"] ->
            `Snd (Host_and_port.of_string addr)
          | _ ->
            failwith (Printf.sprintf "Unexpected line %s\n" line)
        )
        in
        live t lives;
        let () =
          if List.length lives <> 0 then
            (* Update the peers *)
            Ivar.fill_if_empty
              (Option.value_exn !first_peers_ivar)
              (List.map ~f:fst lives)
          else ()
        in
        dead t deads
      )
    end;
    t

  let peers t = Peer.Table.keys t.peers

  let first_peers t = t.first_peers

  let changes t = t.changes_reader

  let stop t = P.kill t.p
end

let%test_module "Tests" = (module struct
  let fold_membership (module M : S) : init:'b -> f:('b -> 'a -> 'b) -> 'b =
    fun ~init ~f ->
    Async.Thread_safe.block_on_async_exn (fun () ->
      match%bind (
        M.connect ~initial_peers:[] ~me:(Host_and_port.create ~host:"127.0.0.1" ~port:3000) ~parent_log:(Logger.create ())
      ) with
      | Ok t ->
        let acc = ref init in
        don't_wait_for begin
          Linear_pipe.iter (M.changes t) ~f:(fun e -> return (acc := f (!acc) e))
        end;
        let%bind () = Async.after (Time.Span.of_sec 3.) in
        let%map () = M.stop t in
        !acc
      | Error e -> failwith (Printf.sprintf "%s" (Error.to_string_hum e))
    )

  module Scripted_process (Script : sig val s : [`On of int | `Off of int] list end) = struct
    type t = string list

    let kill t = return ()
    let create ~initial_peers ~me ~log =
      let on p = Printf.sprintf "127.0.0.1:%d key on" p in
      let off p = Printf.sprintf "127.0.0.1:%d key off" p in
      let render cmds = List.map cmds ~f:(function
        | `On p -> on p
        | `Off p -> off p
      )
      in
      Deferred.Or_error.return (render Script.s)
    let output t ~log:_log =
      let (r, w) = Pipe.create () in
      List.iter t ~f:(fun line ->
        Pipe.write_without_pushback w [line]
      );
      r
  end

  module Dummy_process = struct
    open Async
    type t = Process.t

    let kill t =
      let%map _ = Process.run_exn ~prog:"kill" ~args:[Pid.to_string (Process.pid t)] () in
      ()

    let create ~initial_peers ~me ~log =
      Process.create
        ~prog:"./dummy.sh"
        ~args:[]
        ()

    let output t ~log:_log = Pipe.map (Reader.pipe (Process.stdout t)) ~f:String.split_lines
  end

  let%test_module "Mock Events" = (module struct
    module Script = struct
      let s =
        [ `On 3000
        ; `Off 3001
        ; `On 3001
        ; `On 3002
        ; `On 3003
        ; `On 3003
        ; `Off 3000
        ; `Off 3001
        ; `On 3000
        ]
    end

    module M = Make(Scripted_process(Script))

    let%test "Membership" =
      let result =
        fold_membership (module M) ~init:Script.s ~f:(fun acc e ->
          match (acc, e) with
          | ((`On p::rest), Peer.Event.Connect [peer]) when ((Host_and_port.port peer) = p) -> rest
          | ((`Off p::rest), Peer.Event.Disconnect [peer]) when (Host_and_port.port peer) = p -> rest
          | _ -> failwith (Printf.sprintf "Unexpected event %s" (Peer.Event.sexp_of_t e |> Sexp.to_string_hum))
        )
      in
      List.length result = 0
    end)

  module M = Make(Dummy_process)
  let%test "Dummy Script" =
    (* Just make sure the dummy is outputting things *)
    fold_membership (module M) ~init:false ~f:(fun b _e -> b || true)

end)

module Haskell = Make(Haskell_process)

let%test_unit "connect" =
  let addr i = Host_and_port.of_string (Printf.sprintf "127.0.0.1:%d" (3005 + i)) in
  let node me peers = Haskell.connect ~initial_peers:peers ~me ~parent_log:(Logger.create ()) in
  let wait_sec s =
    let open Core in
    let open Async in
    after (Time.Span.of_sec s)
  in
  Async.Thread_safe.block_on_async_exn (fun () ->
    let open Deferred.Let_syntax in
    let%bind _n0 = node (addr 0) [addr 1]
         and _n1 = node (addr 1) [addr 0] in
    let n0, n1 = Or_error.ok_exn _n0, Or_error.ok_exn _n1 in
    let%bind n0_peers =
      Deferred.any
        [ Haskell.first_peers n0
        ; Deferred.map (wait_sec 10.) ~f:(fun () -> [])
        ]
    in
    assert (List.length n0_peers <> 0);
    let%bind n1_peers =
      Deferred.any
        [ Haskell.first_peers n1
        ; Deferred.map (wait_sec 1.) ~f:(fun () -> [])
        ]
    in
    assert (List.length n1_peers <> 0);
    assert (
      Host_and_port.((List.hd_exn n0_peers) = addr 1)
      &&
      Host_and_port.((List.hd_exn n1_peers) = addr 0)
    );
    let%map () = Haskell.stop n0
        and () = Haskell.stop n1
    in
    ()
  )

