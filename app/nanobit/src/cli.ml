open Core
open Async
open Nanobit_base
open Blockchain_snark
open Cli_common

module type Init_intf = sig
  val conf_dir : string
  val prover : Prover.t
  val genesis_proof : Proof.t
end

module Make_inputs0 (Init : Init_intf) = struct
  module Time = Block_time
  module Hash = struct
    type 'a t = Snark_params.Tick.Pedersen.Digest.t
    [@@deriving compare, hash, sexp, bin_io]
    (* TODO *)
    let digest _ = Snark_params.Tick.Pedersen.zero_hash
  end
  module State_hash = State_hash.Stable.V1
  module Ledger_hash = Ledger_hash.Stable.V1
  module Transaction : sig
    type t = Nanobit_base.Transaction.t
    [@@deriving eq, bin_io, compare]

    module With_valid_signature : sig
      type nonrec t = private t
      [@@deriving eq, bin_io, compare]
    end

    val check : t -> With_valid_signature.t option
  end = struct
    type t = Nanobit_base.Transaction.t
    [@@deriving eq, bin_io]
    (* The underlying transaction has an arbitrary compare func, fallback to that *)
    let compare (t : t) (t' : t) = 
      let fee_compare =
        Transaction.Fee.compare t.payload.fee t'.payload.fee
      in
      match fee_compare with
      | 0 -> Transaction.compare t t'
      | _ -> fee_compare

    module With_valid_signature = struct
      type t = Nanobit_base.Transaction.t 
      [@@deriving eq, bin_io]
      let compare = compare
    end

    let check t = Option.some_if (Nanobit_base.Transaction.check_signature t) t
  end

  module Nonce = Nanobit_base.Nonce

  module Difficulty = Difficulty

  module Pow = Snark_params.Tick.Pedersen.Digest

  module Strength = struct
    include Strength

    (* TODO *)
    let increase t ~by = t
  end

  module Ledger = struct
    type t = Nanobit_base.Ledger.t [@@deriving sexp, compare, hash, bin_io]
    type valid_transaction = Transaction.With_valid_signature.t

    let create = Ledger.create
    let merkle_root = Ledger.merkle_root
    let copy = Nanobit_base.Ledger.copy
    let apply_transaction t (valid_transaction : Transaction.With_valid_signature.t) : unit Or_error.t =
      Nanobit_base.Ledger.apply_transaction_unchecked t (valid_transaction :> Transaction.t)
  end

  module Ledger_proof = struct
    type t = Proof.t
    type input =
      { source     : Ledger_hash.t
      ; target     : Ledger_hash.t
      ; proof_type : Transaction_snark.Proof_type.t
      }

    let verify proof { source; target; proof_type } =
      Prover.verify_transaction_snark Init.prover
        (Transaction_snark.create ~source ~target ~proof_type ~proof)
      >>| Or_error.ok_exn
  end

  module Transition = struct
    type t =
      { ledger_hash : Ledger_hash.t
      ; ledger_proof : Ledger_proof.t
      ; timestamp : Time.t
      ; nonce : Nonce.t
      }
    [@@deriving fields]
  end

  module Time_close_validator = struct
    let validate t =
      let now_time = Time.now () in
      Time.(diff now_time t < (Span.of_time_span (Core_kernel.Time.Span.of_sec 900.)))
  end

  module State = struct
    include State
    module Proof = struct
      type input = t

      type t = Proof.Stable.V1.t
      [@@deriving bin_io]

      let verify proof s =
        Prover.verify_blockchain Init.prover
          { Blockchain.state = to_blockchain_state s; proof }
        >>| Or_error.ok_exn
    end
  end

  module Proof_carrying_state = struct
    type t = (State.t, State.Proof.t) Protocols.Minibit_pow.Proof_carrying_data.t
    [@@deriving bin_io]
  end

  module State_with_witness = struct
    type transaction_with_valid_signature = Transaction.With_valid_signature.t
    type transaction = Transaction.t
      [@@deriving bin_io]
    type witness = Transaction.With_valid_signature.t list
      [@@deriving bin_io]
    type state = Proof_carrying_state.t 
      [@@deriving bin_io]
    type t =
      { transactions : witness
      ; state : state
      }
      [@@deriving bin_io]

    module Stripped = struct
      type witness = Transaction.t list
        [@@deriving bin_io]
      type t =
        { transactions : witness
        ; state : Proof_carrying_state.t
        }
      [@@deriving bin_io]
    end

    let strip t = 
      { Stripped.transactions = (t.transactions :> Transaction.t list)
      ; state = t.state
      }

    let forget_witness {state} = state
    (* TODO should we also consume a ledger here so we know the transactions valid? *)
    let add_witness_exn state transactions =
      {state ; transactions}
    (* TODO same *)
    let add_witness state transactions = Or_error.return {state ; transactions}
  end
  module Transition_with_witness = struct
    type witness = Transaction.With_valid_signature.t list
    type t =
      { transactions : witness
      ; transition : Transition.t
      }

    let forget_witness {transition} = transition
    (* TODO should we also consume a ledger here so we know the transactions valid? *)
    let add_witness_exn transition transactions =
      {transition ; transactions}
    (* TODO same *)
    let add_witness transition transactions = Or_error.return {transition ; transactions}
  end

end
module Make_inputs (Init : Init_intf) = struct
  module Inputs0 = Make_inputs0(Init)
  include Inputs0
  module Net = Minibit_networking.Make(struct
    module State_with_witness = State_with_witness
    module Ledger_hash = Ledger_hash
    module Ledger = Ledger
    module State = State
  end)
  module Ledger_fetcher_io = Net.Ledger_fetcher_io
  module State_io = Net.State_io
  module Bundle = struct
    include Bundle

    let create ledger (ts : Transaction.With_valid_signature.t list) =
      create ~conf_dir:Init.conf_dir ledger (ts :> Transaction.t list)

    let result t = Deferred.Option.(snark t >>| Transaction_snark.proof)
  end

  module Transaction_pool = Transaction_pool.Make(Transaction)

  module Genesis = struct
    let state : State.t = State.zero
    let proof = Init.genesis_proof
  end
  module Ledger_fetcher = Ledger_fetcher.Make(struct
    include Inputs0
    module Net = Net
    module Store = Storage.Disk
    module Transaction_pool = Transaction_pool
    module Genesis = Genesis
  end)

  module Miner = Minibit_miner.Make(struct
    include Inputs0
    module Transaction_pool = Transaction_pool
    module Bundle = Bundle
  end)

  module Block_state_transition_proof = struct
    module Witness = struct
      type t =
        { old_state : State.t
        ; old_proof : State.Proof.t
        ; transition : Transition.t
        }
    end

    let prove_zk_state_valid ({ old_state; old_proof; transition } : Witness.t) ~new_state:_ =
      Prover.extend_blockchain Init.prover
        { state = State.to_blockchain_state old_state; proof = old_proof }
        { header = { time = transition.timestamp; nonce = transition.nonce }
        ; body = { target_hash = transition.ledger_hash; proof = transition.ledger_proof }
        }
      >>| Or_error.ok_exn
      >>| Blockchain.proof
  end
end

let daemon =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Current daemon"
    begin
      [%map_open
        let conf_dir =
          flag "config directory"
            ~doc:"Configuration directory"
            (optional file)
        and should_mine =
          flag "mine"
            ~doc:"Run the miner" (required bool)
        and port =
          flag "port"
            ~doc:"Server port for other to connect" (required int16)
        and ip =
          flag "ip"
            ~doc:"External IP address for others to connect" (optional string)
        in
        fun () ->
          let open Deferred.Let_syntax in
          let%bind home = Sys.home_directory () in
          let conf_dir =
            Option.value ~default:(home ^/ ".current-config") conf_dir
          in
          let%bind () = Unix.mkdir ~p:() conf_dir in
          let%bind initial_peers =
            let peers_path = conf_dir ^/ "peers" in
            match%bind Reader.load_sexp peers_path [%of_sexp: Host_and_port.t list] with
            | Ok ls -> return ls
            | Error e -> 
              begin
                let default_initial_peers = [] in
                let%map () = Writer.save_sexp peers_path ([%sexp_of: Host_and_port.t list] default_initial_peers) in
                []
              end
          in
          let log = Logger.create () in
          let%bind ip =
            match ip with
            | None -> Find_ip.find ()
            | Some ip -> return ip
          in
          let remap_addr_port = Fn.id in
          let me = Host_and_port.create ~host:ip ~port in
          let%bind prover = Prover.create ~conf_dir in
          let%bind genesis_proof = Prover.genesis_proof prover >>| Or_error.ok_exn in
          let module Init = struct
            let conf_dir = conf_dir
            let prover = prover
            let genesis_proof = genesis_proof
          end
          in
          let module Inputs = Make_inputs(Init) in
          let module Main = Minibit.Make(Inputs)(Inputs.Block_state_transition_proof) in
          let net_config = 
            { Inputs.Net.Config.parent_log = log
            ; gossip_net_params =
                { timeout = Time.Span.of_sec 1.
                ; target_peer_count = 8
                ; address = remap_addr_port me
                } 
            ; initial_peers
            ; me
            ; remap_addr_port
            }
          in
          let%bind minibit =
            Main.create
              { log
              ; net_config
              ; ledger_disk_location = conf_dir ^/ "ledgers"
              ; pool_disk_location = conf_dir ^/ "transaction_pool"
              }
          in
          printf "Created minibit\n%!";
          Main.run minibit;
          printf "Ran minibit\n%!";
          Async.never ()

          (*let%bind prover =*)
            (*if start_prover*)
            (*then Prover.create ~port:prover_port ~debug:()*)
            (*else Prover.connect { host = "0.0.0.0"; port = prover_port }*)
          (*in*)
          (*let%bind genesis_proof = Prover.genesis_proof prover >>| Or_error.ok_exn in*)
          (*let genesis_blockchain =*)
            (*{ Blockchain.state = Blockchain.State.zero*)
            (*; proof = genesis_proof*)
            (*; most_recent_block = Block.genesis*)
            (*}*)
          (*in*)
          (*let%bind () = Main.assert_chain_verifies prover genesis_blockchain in*)
          (*let%bind ip =*)
            (*match ip with*)
            (*| None -> Find_ip.find ()*)
            (*| Some ip -> return ip*)
          (*in*)
          (*let minibit = Main.create ()*)
          (*let log = Logger.create () in*)
          (*Main.main*)
            (*~log*)
            (*~prover*)
            (*~storage_location:(conf_dir ^/ "storage")*)
            (*~genesis_blockchain*)
            (*~initial_peers *)
            (*~should_mine*)
            (*~me:(Host_and_port.create ~host:ip ~port)*)
            (*()*)
      ]
    end
;;

let () = 
  Command.group ~summary:"Current"
    [ "daemon", daemon
    ; Parallel.worker_command_name, Parallel.worker_command
    ; "rpc", Main_rpc.command
    ; "client", Client.command
    ]
  |> Command.run
;;

let () = never_returns (Scheduler.go ())
;;
