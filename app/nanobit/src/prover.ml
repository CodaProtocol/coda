open Core
open Async
open Nanobit_base
open Util
open Blockchain_snark
open Cli_lib

module type S = sig
  module Consensus_mechanism :
    Consensus.Mechanism.S with type Proof.t = Proof.t

  module Blockchain :
    Blockchain.S with module Consensus_mechanism = Consensus_mechanism

  type t

  val create : conf_dir:string -> t Deferred.t

  val initialized : t -> [`Initialized] Deferred.Or_error.t

  val extend_blockchain :
       t
    -> Blockchain.t
    -> Consensus_mechanism.Snark_transition.value
    -> Blockchain.t Deferred.Or_error.t
end

module Make
    (Consensus_mechanism : Consensus.Mechanism.S
                           with type Proof.t = Nanobit_base.Proof.t)
    (Blockchain : Blockchain.S
                  with module Consensus_mechanism = Consensus_mechanism) =
struct
  module Consensus_mechanism = Consensus_mechanism
  module Blockchain = Blockchain

  module Worker_state = struct
    module type S = sig
      module Transaction_snark : Transaction_snark.S

      val extend_blockchain :
           Blockchain.t
        -> Consensus_mechanism.Snark_transition.value
        -> Blockchain.t Or_error.t

      val verify : Consensus_mechanism.Protocol_state.value -> Proof.t -> bool

      val update :
           Consensus_mechanism.Protocol_state.value
        -> Consensus_mechanism.Snark_transition.value
        -> Consensus_mechanism.Protocol_state.value Or_error.t
    end

    type init_arg = unit [@@deriving bin_io]

    type t = (module S) Deferred.t

    let create () : t Deferred.t =
      Deferred.return
        (let module Keys = Keys_lib.Keys.Make (Consensus_mechanism) in
        let%map (module Keys) = Keys.create () in
        let module Transaction_snark = Transaction_snark.Make (struct
          let keys = Keys.transaction_snark_keys
        end) in
        let module M = struct
          open Snark_params
          open Keys
          module Consensus_mechanism = Keys.Consensus_mechanism
          module Transaction_snark = Transaction_snark
          module Blockchain_state = Blockchain_state.Make (Keys.
                                                           Consensus_mechanism)
          module State = Blockchain_state.Make_update (Transaction_snark)

          let update = State.update

          let wrap hash proof =
            let module Wrap = Keys.Wrap in
            Tock.prove
              (Tock.Keypair.pk Wrap.keys)
              (Wrap.input ()) {Wrap.Prover_state.proof} Wrap.main (embed hash)

          let extend_blockchain (chain: Blockchain.t)
              (block: Keys.Consensus_mechanism.Snark_transition.value) =
            let open Or_error.Let_syntax in
            let%map next_state = update chain.state block in
            let next_state_top_hash = Keys.Step.instance_hash next_state in
            let prover_state =
              { Keys.Step.Prover_state.prev_proof= chain.proof
              ; wrap_vk= Tock.Keypair.vk Keys.Wrap.keys
              ; prev_state= chain.state
              ; update= block }
            in
            let prev_proof =
              Tick.prove
                (Tick.Keypair.pk Keys.Step.keys)
                (Keys.Step.input ()) prover_state Keys.Step.main
                next_state_top_hash
            in
            { Blockchain.state= next_state
            ; proof= wrap next_state_top_hash prev_proof }

          let verify state proof =
            Tock.verify proof
              (Tock.Keypair.vk Wrap.keys)
              (Wrap.input ())
              (embed (Keys.Step.instance_hash state))
        end in
        (module M : S))

    let get = Fn.id
  end

  open Snark_params

  module Functions = struct
    type ('i, 'o) t =
      'i Bin_prot.Type_class.t
      * 'o Bin_prot.Type_class.t
      * (Worker_state.t -> 'i -> 'o Deferred.t)

    let create input output f : ('i, 'o) t = (input, output, f)

    let initialized =
      create bin_unit [%bin_type_class : [`Initialized]] (fun w () ->
          let%map (module W) = Worker_state.get w in
          `Initialized )

    let extend_blockchain =
      create
        [%bin_type_class
          : Blockchain.t * Consensus_mechanism.Snark_transition.value]
        Blockchain.bin_t
        (fun w
        ( ({Blockchain.state= prev_state; proof= prev_proof} as chain)
        , transition )
        ->
          let%map (module W) = Worker_state.get w in
          if Insecure.extend_blockchain then
            let proof = Precomputed_values.base_proof in
            { Blockchain.proof
            ; state=
                Consensus_mechanism.Protocol_state.create_value
                  ~previous_state_hash:
                    (Consensus_mechanism.Protocol_state.hash prev_state)
                  ~blockchain_state:
                    ( transition
                    |> Consensus_mechanism.Snark_transition.protocol_state
                    |> Consensus_mechanism.Protocol_state.blockchain_state )
                  ~consensus_state:
                    (Consensus_mechanism.update_unchecked
                       (Consensus_mechanism.Protocol_state.consensus_state
                          prev_state)
                       transition) }
          else Or_error.ok_exn (W.extend_blockchain chain transition) )

    let verify_blockchain =
      create Blockchain.bin_t bin_bool (fun w {Blockchain.state; proof} ->
          let%map (module W) = Worker_state.get w in
          if Insecure.verify_blockchain then true else W.verify state proof )

    let verify_transaction_snark =
      create Transaction_snark.bin_t bin_bool (fun w proof ->
          let%map (module W) = Worker_state.get w in
          W.Transaction_snark.verify proof )
  end

  module Worker = struct
    module T = struct
      module F = Rpc_parallel.Function

      type 'w functions =
        { initialized: ('w, unit, [`Initialized]) F.t
        ; extend_blockchain:
            ( 'w
            , Blockchain.t * Consensus_mechanism.Snark_transition.value
            , Blockchain.t )
            F.t
        ; verify_blockchain: ('w, Blockchain.t, bool) F.t
        ; verify_transaction_snark: ('w, Transaction_snark.t, bool) F.t }

      module Worker_state = Worker_state

      module Connection_state = struct
        type init_arg = unit [@@deriving bin_io]

        type t = unit
      end

      module Functions
          (C : Rpc_parallel.Creator
               with type worker_state := Worker_state.t
                and type connection_state := Connection_state.t) =
      struct
        let functions =
          let f (i, o, f) =
            C.create_rpc
              ~f:(fun ~worker_state ~conn_state i -> f worker_state i)
              ~bin_input:i ~bin_output:o ()
          in
          let open Functions in
          { initialized= f initialized
          ; extend_blockchain= f extend_blockchain
          ; verify_blockchain= f verify_blockchain
          ; verify_transaction_snark= f verify_transaction_snark }

        let init_worker_state () = Worker_state.create ()

        let init_connection_state ~connection:_ ~worker_state:_ = return
      end
    end

    include Rpc_parallel.Make (T)
  end

  type t = {connection: Worker.Connection.t; process: Process.t}

  let create ~conf_dir =
    let%map connection, process =
      (* HACK: Need to make connection_timeout long since creating a prover can take a long time*)
      Worker.spawn_in_foreground_exn ~connection_timeout:(Time.Span.of_min 1.)
        ~on_failure:Error.raise ~shutdown_on:Disconnect
        ~connection_state_init_arg:() ()
    in
    {connection; process}

  let initialized {connection; _} =
    Worker.Connection.run connection ~f:Worker.functions.initialized ~arg:()

  let extend_blockchain {connection; _} chain block =
    Worker.Connection.run connection ~f:Worker.functions.extend_blockchain
      ~arg:(chain, block)

  let verify_blockchain {connection; _} chain =
    Worker.Connection.run connection ~f:Worker.functions.verify_blockchain
      ~arg:chain

  let verify_transaction_snark {connection; _} snark =
    Worker.Connection.run connection
      ~f:Worker.functions.verify_transaction_snark ~arg:snark
end
