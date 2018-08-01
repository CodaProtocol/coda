open Core
open Async
open Nanobit_base
open Coda_main
open Spawner

(* let () = Printexc.record_backtrace true *)

module Coda_worker = struct
  type coda = {peers: unit -> Host_and_port.t list}

  type input =
    { host: string
    ; log_dir: string
    ; program_dir: string
    ; my_port: int
    ; gossip_port: int
    ; peers: Host_and_port.t list
    ; should_wait: bool }
  [@@deriving bin_io]

  type t =
    { input: input
    ; reader: Host_and_port.t list Pipe.Reader.t
    ; writer: Host_and_port.t list Pipe.Writer.t }

  type state = Host_and_port.t list [@@deriving bin_io]

  let make {host; log_dir; program_dir; my_port; peers; gossip_port} =
    let log = Logger.create () in
    let%bind location = Unix.getcwd () in
    let%bind () = Sys.chdir program_dir in
    let%bind location = Unix.getcwd () in
    let conf_dir = log_dir in
    let%bind verifier = Verifier.create ~conf_dir in
    let%bind prover = Prover.create ~conf_dir in
    let module Init = struct
      type proof = Proof.Stable.V1.t [@@deriving bin_io, sexp]

      let logger = log

      let conf_dir = conf_dir

      let verifier = verifier

      let prover = prover

      let genesis_proof = Precomputed_values.base_proof

      let fee_public_key = Genesis_ledger.rich_pk
    end in
    let module Main : Main_intf = Coda_without_snark (Init) () in
    let module Run = Run (Main) in
    let net_config =
      { Main.Inputs.Net.Config.conf_dir
      ; parent_log= log
      ; gossip_net_params=
          { Main.Inputs.Net.Gossip_net.Params.timeout= Time.Span.of_sec 1.
          ; target_peer_count= 8
          ; address= Host_and_port.create ~host ~port:gossip_port }
      ; initial_peers= peers
      ; me= Host_and_port.create ~host ~port:my_port
      ; remap_addr_port= Fn.id }
    in
    let%map minibit =
      Main.create
        (Main.Config.make ~log ~net_config
           ~ledger_builder_persistant_location:"ledger_builder"
           ~transaction_pool_disk_location:"transaction_pool"
           ~snark_pool_disk_location:"snark_pool" ())
    in
    let peers () = Main.peers minibit in
    {peers}

  let create input =
    let reader, writer = Pipe.create () in
    return {input; reader; writer}

  let new_states {reader} = reader

  let run {input; writer} =
    let%bind coda = make input in
    Log.Global.info "Created Coda" ;
    let time_to_wait =
      if input.should_wait then (
        Log.Global.info "Waiting to connect Coda" ;
        10. )
      else 0.0
    in
    let%bind () = after (Time.Span.of_sec time_to_wait) in
    Pipe.write writer @@ coda.peers ()
end

module Worker = Parallel_worker.Make (Coda_worker)
module Master = Master.Make (Worker (Int)) (Int)

let run =
  let open Command.Let_syntax in
  (* HACK: to run the dependency, Kademlia *)
  let%map_open program_dir =
    flag "program-directory" ~doc:"base directory of nanobit project "
      (optional file)
  and host = Command_util.host_flag
  and executable_path = Command_util.executable_path_flag in
  fun () ->
    let open Deferred.Let_syntax in
    let open Master in
    let%bind program_dir =
      Option.value_map program_dir ~default:(Unix.getcwd ()) ~f:return
    in
    let setup_peers log_dir peers =
      Out_channel.write_all
        (Filename.concat log_dir "peers")
        ~data:([%sexp_of : Host_and_port.t list] peers |> Sexp.to_string)
    in
    let init_coda t ~config ~my_port ~peers ~gossip_port ~should_wait =
      let {Spawner.Config.host; log_dir; id} = config in
      setup_peers log_dir peers ;
      let%bind () =
        add t
          ( { host
            ; my_port
            ; peers
            ; log_dir
            ; program_dir
            ; gossip_port
            ; should_wait }
          : Coda_worker.input )
          id ~config
      in
      Deferred.unit
    in
    let clean_coda log_dirs =
      Process.run_exn ~prog:"rm" ~args:("-rf" :: log_dirs) ()
    in
    let t = create () in
    let log_dir_1 = "/tmp/current_config_1"
    and log_dir_2 = "/tmp/current_config_2" in
    let%bind () = File_system.create_dir log_dir_1 in
    let%bind () = File_system.create_dir log_dir_2 in
    let process1 = 1 and process2 = 2 in
    let config1 =
      {Spawner.Config.id= process1; host; executable_path; log_dir= log_dir_1}
    and config2 =
      {Spawner.Config.id= process2; host; executable_path; log_dir= log_dir_2}
    in
    let coda_gossip_port = 8000 in
    let coda_my_port = 3000 in
    let%bind () =
      init_coda t ~config:config1 ~my_port:coda_my_port ~peers:[]
        ~gossip_port:coda_gossip_port ~should_wait:false
    in
    let%bind () = Option.value_exn (run t process1) in
    let reader = new_states t in
    let%bind _, _ = Linear_pipe.read_exn reader in
    let expected_peers = [Host_and_port.create host coda_my_port] in
    let%bind () =
      init_coda t ~config:config2 ~my_port:coda_my_port ~peers:expected_peers
        ~gossip_port:(coda_gossip_port + 1) ~should_wait:true
    in
    let%bind () = Option.value_exn (run t process2) in
    let%bind _, coda_2_peers = Linear_pipe.read_exn reader in
    let%map _ = clean_coda [log_dir_1; log_dir_2] in
    assert (expected_peers = coda_2_peers)

let name = "coda-sample-test"

let command =
  Command.async
    ~summary:
      "A test that shows how a coda instance can identify another instance as \
       it's peer"
    run
