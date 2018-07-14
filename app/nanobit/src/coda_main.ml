open Core
open Async
open Nanobit_base
open Blockchain_snark

module type Init_intf = sig
  type proof [@@deriving bin_io, sexp]

  val logger : Logger.t

  val conf_dir : string

  val prover : Prover.t

  val verifier : Verifier.t

  val genesis_proof : proof

  (* Public key to allocate fees to *)

  val fee_public_key : Public_key.Compressed.t
end

module type State_proof_intf = sig
  type t [@@deriving bin_io, sexp]

  include Protocols.Coda_pow.Proof_intf
          with type input := State.t
           and type t := t
end

module Make_inputs0 (Ledger_proof : sig
  type t [@@deriving sexp, bin_io]

  val verify :
       t
    -> Transaction_snark.Statement.t
    -> message:Currency.Fee.t * Public_key.Compressed.t
    -> bool Deferred.t
end)
(State_proof : State_proof_intf) (Difficulty : module type of Difficulty)
(Init : Init_intf with type proof = State_proof.t) =
struct
  open Protocols.Coda_pow

  module Time : Time_intf with type t = Block_time.t = Block_time

  module Time_close_validator = struct
    let limit = Block_time.Span.of_time_span (Core.Time.Span.of_sec 15.)

    let validate t =
      let now = Block_time.now () in
      (* t should be at most [limit] greater than now *)
      Block_time.Span.( < ) (Block_time.diff t now) limit
  end

  module Public_key = Public_key
  module State_hash = State_hash.Stable.V1
  module Strength = Strength
  module Block_nonce = Block.Nonce
  module Ledger_builder_hash = Ledger_builder_hash.Stable.V1
  module Ledger_hash = Ledger_hash.Stable.V1
  module Pow = Proof_of_work

  module Amount = struct
    module Signed = struct
      include Currency.Amount.Signed

      include (
        Currency.Amount.Signed.Stable.V1 :
          module type of Currency.Amount.Signed.Stable.V1
          with type t := t
           and type ('a, 'b) t_ := ('a, 'b) t_ )
    end
  end

  module Fee = struct
    module Unsigned = struct
      include Currency.Fee

      include (
        Currency.Fee.Stable.V1 :
          module type of Currency.Fee.Stable.V1 with type t := t )
    end

    module Signed = struct
      include Currency.Fee.Signed

      include (
        Currency.Fee.Signed.Stable.V1 :
          module type of Currency.Fee.Signed.Stable.V1
          with type t := t
           and type ('a, 'b) t_ := ('a, 'b) t_ )
    end
  end

  module State = struct
    include State

    module Proof = struct
      include State_proof

      type input = State.t
    end
  end

  module Transaction = struct
    include (
      Transaction :
        module type of Transaction
        with module With_valid_signature := Transaction.With_valid_signature )

    let fee (t: t) = t.payload.Transaction.Payload.fee

    let seed = Secure_random.string ()

    let compare t1 t2 = Transaction.Stable.V1.compare ~seed t1 t2

    module With_valid_signature = struct
      include Transaction.With_valid_signature

      let compare t1 t2 = Transaction.With_valid_signature.compare ~seed t1 t2
    end
  end

  module Fee_transfer = Nanobit_base.Fee_transfer

  module Super_transaction = struct
    module T = struct
      type t = Transaction_snark.Transition.t =
        | Transaction of Transaction.With_valid_signature.t
        | Fee_transfer of Fee_transfer.t
      [@@deriving compare, eq]

      let fee_excess = function
        | Transaction t -> Ok (Transaction.fee (t :> Transaction.t))
        | Fee_transfer t -> Fee_transfer.fee_excess t
    end

    include T

    include (
      Transaction_snark.Transition :
        module type of Transaction_snark.Transition with type t := t )
  end

  module Ledger = struct
    include Ledger

    let apply_super_transaction l = function
      | Super_transaction.Transaction t -> apply_transaction l t
      | Fee_transfer t -> apply_fee_transfer l t

    let undo_super_transaction l = function
      | Super_transaction.Transaction t -> undo_transaction l t
      | Fee_transfer t -> undo_fee_transfer l t
  end

  module Transaction_snark = struct
    module Statement = Transaction_snark.Statement
    include Ledger_proof
  end

  module Ledger_proof = struct
    include Ledger_proof

    type statement = Transaction_snark.Statement.t
  end

  module Completed_work = struct
    let proofs_length = 2

    module Statement = struct
      module T = struct
        type t = Transaction_snark.Statement.t list
        [@@deriving bin_io, sexp, hash, compare]
      end

      include T
      include Hashable.Make_binable (T)

      let gen =
        Quickcheck.Generator.list_with_length proofs_length
          Transaction_snark.Statement.gen
    end

    module Proof = struct
      type t = Transaction_snark.t list [@@deriving bin_io, sexp]
    end

    module T = struct
      type t =
        {fee: Fee.Unsigned.t; proofs: Proof.t; prover: Public_key.Compressed.t}
      [@@deriving sexp, bin_io]
    end

    include T

    module Checked = struct
      include T
    end

    let forget = Fn.id

    let check ({fee; prover; proofs} as t) stmts =
      let message = (fee, prover) in
      match List.zip proofs stmts with
      | None -> return None
      | Some ps ->
          let%map good =
            Deferred.List.for_all ps ~f:(fun (proof, stmt) ->
                Transaction_snark.verify ~message proof stmt )
          in
          Option.some_if good t
  end

  module Difficulty = Difficulty

  module Ledger_builder_diff = struct
    type t =
      { prev_hash: Ledger_builder_hash.t
      ; completed_works: Completed_work.t list
      ; transactions: Transaction.t list
      ; creator: Public_key.Compressed.t }
    [@@deriving sexp, bin_io]

    module With_valid_signatures_and_proofs = struct
      type t =
        { prev_hash: Ledger_builder_hash.t
        ; completed_works: Completed_work.Checked.t list
        ; transactions: Transaction.With_valid_signature.t list
        ; creator: Public_key.Compressed.t }
      [@@deriving sexp, bin_io]
    end

    let forget
        { With_valid_signatures_and_proofs.prev_hash
        ; completed_works
        ; transactions
        ; creator } =
      { prev_hash
      ; completed_works= List.map ~f:Completed_work.forget completed_works
      ; transactions= (transactions :> Transaction.t list)
      ; creator }
  end

  module Ledger_builder = Ledger_builder.Make (struct
    module Amount = Amount
    module Fee = Fee
    module Public_key = Public_key.Compressed
    module Transaction = Transaction
    module Fee_transfer = Fee_transfer
    module Super_transaction = Super_transaction
    module Ledger = Ledger
    module Transaction_snark = Transaction_snark
    module Ledger_hash = Ledger_hash
    module Ledger_builder_hash = Ledger_builder_hash
    module Ledger_builder_diff = Ledger_builder_diff
    module Completed_work = Completed_work
  end)

  module Ledger_builder_transition = struct
    type t = {old: Ledger_builder.t; diff: Ledger_builder_diff.t}
    [@@deriving sexp, bin_io]

    module With_valid_signatures_and_proofs = struct
      type t =
        { old: Ledger_builder.t
        ; diff: Ledger_builder_diff.With_valid_signatures_and_proofs.t }
      [@@deriving sexp, bin_io]
    end

    let forget {With_valid_signatures_and_proofs.old; diff} =
      {old; diff= Ledger_builder_diff.forget diff}
  end

  module Transition = struct
    type t =
      { ledger_hash: Ledger_hash.t
      ; ledger_builder_hash: Ledger_builder_hash.t
      ; ledger_proof: Ledger_proof.t option
      ; ledger_builder_transition: Ledger_builder_diff.t
      ; timestamp: Time.t
      ; nonce: Block_nonce.t }
    [@@deriving fields, sexp]
  end

  module Transition_with_witness = struct
    type t = {previous_ledger_hash: Ledger_hash.t; transition: Transition.t}
    [@@deriving sexp]

    let forget_witness {transition; _} = transition
  end
end

module Make_inputs (Ledger_proof0 : sig
  type t [@@deriving sexp, bin_io]

  val statement : t -> Transaction_snark.Statement.t

  val verify :
       t
    -> Transaction_snark.Statement.t
    -> message:Currency.Fee.t * Public_key.Compressed.t
    -> bool Deferred.t
end)
(State_proof : State_proof_intf) (Difficulty : module type of Difficulty)
(Init : Init_intf with type proof = State_proof.t)
(Store : Storage.With_checksum_intf)
() =
struct
  module Inputs0 =
    Make_inputs0 (Ledger_proof0) (State_proof) (Difficulty) (Init)
  include Inputs0

  module Proof_carrying_state = struct
    type t = (State.t, State.Proof.t) Protocols.Coda_pow.Proof_carrying_data.t
    [@@deriving sexp, bin_io]
  end

  module State_with_witness = struct
    type t =
      { ledger_builder_transition:
          Ledger_builder_transition.With_valid_signatures_and_proofs.t
      ; state: Proof_carrying_state.t }
    [@@deriving sexp]

    module Stripped = struct
      type t =
        { ledger_builder_transition: Ledger_builder_transition.t
        ; state: Proof_carrying_state.t }
      [@@deriving bin_io]
    end

    let strip {ledger_builder_transition; state} =
      { Stripped.ledger_builder_transition=
          Ledger_builder_transition.forget ledger_builder_transition
      ; state }

    (*
    let check
          { Stripped.transactions; ledger_builder_transition; state } =
      let open Option.Let_syntax in
      let%map transactions = Option.all (List.map ~f:Transaction.check transactions) in
      { transactions
      ; ledger_builder_transition
      ; state
      } *)

    let forget_witness {ledger_builder_transition; state} = state

    let add_witness_exn = failwith "TODO?"

    let add_witness = failwith "TODO?"
  end

  module Genesis = struct
    let state = State.zero

    let ledger = Genesis_ledger.ledger

    let proof = Init.genesis_proof
  end

  module Snark_pool = struct
    module Work = Completed_work.Statement
    module Proof = Completed_work.Proof

    module Fee = struct
      module T = struct
        type t = {fee: Fee.Unsigned.t; prover: Public_key.Compressed.t}
        [@@deriving bin_io, sexp]

        (* TODO: Compare in a better way than with public key, like in transaction pool *)
        let compare t1 t2 =
          let r = compare t1.fee t2.fee in
          if Int.( <> ) r 0 then r
          else Public_key.Compressed.compare t1.prover t2.prover
      end

      include T
      include Comparable.Make (T)

      let gen =
        (* This isn't really a valid public key, but good enough for testing *)
        let pk =
          let open Snark_params.Tick in
          let open Quickcheck.Generator.Let_syntax in
          let%map x = Bignum_bigint.(gen_incl zero (Field.size - one))
          and is_odd = Bool.gen in
          let x = Bigint.(to_field (of_bignum_bigint x)) in
          {Public_key.Compressed.x; is_odd}
        in
        Quickcheck.Generator.map2 Fee.Unsigned.gen pk ~f:(fun fee prover ->
            {fee; prover} )
    end

    module Pool = Snark_pool.Make (Proof) (Fee) (Work)
    module Diff = Network_pool.Snark_pool_diff.Make (Proof) (Fee) (Work) (Pool)

    type pool_diff = Diff.t

    include Network_pool.Make (Pool) (Diff)

    let get_completed_work t statement =
      Option.map
        (Pool.request_proof (pool t) statement)
        ~f:(fun {proof; fee= {fee; prover}} ->
          {Completed_work.fee; proofs= proof; prover} )

    let load ~disk_location ~incoming_diffs =
      match%map Reader.load_bin_prot disk_location Pool.bin_reader_t with
      | Ok pool -> of_pool_and_diffs pool ~incoming_diffs
      | Error _e -> create ~incoming_diffs
  end

  module type S_tmp =
    Coda.Network_intf
    with type state_with_witness := State_with_witness.t
     and type ledger_builder := Ledger_builder.t
     and type state := State.t
     and type ledger_builder_hash := Ledger_builder_hash.t

  module Net = (val (failwith "TODO" : (module S_tmp)))

  module Ledger_builder_controller = struct
    module Inputs = struct
      module Store = Store
      module Snark_pool = Snark_pool

      module Net = struct
        type net = Net.t

        include Net.Ledger_builder_io
      end

      module Ledger_hash = Ledger_hash
      module Ledger_builder_hash = Ledger_builder_hash
      module Ledger = Ledger
      module Ledger_builder_diff = Ledger_builder_diff

      module Ledger_builder = struct
        include Ledger_builder

        type proof = Ledger_proof.t

        let create ledger = create ~ledger ~self:Init.fee_public_key

        let apply t diff =
          Deferred.Or_error.map
            (Ledger_builder.apply t diff)
            ~f:
              (Option.map ~f:(fun proof ->
                   ((Ledger_proof0.statement proof).target, proof) ))
      end

      module State = State
      module State_hash = State_hash
      module Valid_transaction = Transaction.With_valid_signature
    end

    include Ledger_builder_controller.Make (Inputs)
  end

  module Transaction_pool = Transaction_pool.Make (Transaction.
                                                   With_valid_signature)
  module Miner = Minibit_miner.Make (Inputs0)
end

module Coda_with_snark
    (Store : Storage.With_checksum_intf)
    (Init : Init_intf with type proof = Proof.t)
    () =
struct
  module Ledger_proof = Ledger_proof.Make_prod (Init)
  module State_proof = State_proof.Make_prod (Init)

  module Inputs =
    Make_inputs (Ledger_proof) (State_proof) (Difficulty) (Init) (Store) ()

  module Block_state_transition_proof = struct
    module Witness = struct
      type t =
        { old_state: State.t
        ; old_proof: Proof.t
        ; transition: Inputs.Transition.t }
    end

    let prove_zk_state_valid {Witness.old_state; old_proof; transition}
        ~new_state:_ =
      Prover.extend_blockchain Init.prover
        {proof= old_proof; state= State.to_blockchain_state old_state}
        { header= {time= transition.timestamp; nonce= transition.nonce}
        ; body=
            { target_hash= transition.ledger_hash
            ; ledger_builder_hash= transition.ledger_builder_hash
            ; proof=
                Option.map ~f:Transaction_snark.proof transition.ledger_proof
            } }
      >>| Or_error.ok_exn
      >>| fun {Blockchain_snark.Blockchain.proof; _} -> proof
  end

  include Coda.Make (Inputs) (Block_state_transition_proof)
end

module Coda_without_snark (Init : Init_intf) () = struct
  module Store = Storage.Memory
  module Ledger_proof = Ledger_proof.Debug

  module State_proof = State_proof.Make_debug (struct
    type t = Init.proof [@@deriving bin_io, sexp]
  end)

  module Inputs =
    Make_inputs (Ledger_proof) (State_proof) (Difficulty) (Init) (Store) ()

  module Block_state_transition_proof = struct
    module Witness = struct
      type t =
        { old_state: State.t
        ; old_proof: State_proof.t
        ; transition: Inputs.Transition.t }
    end

    let prove_zk_state_valid {Witness.old_state; old_proof; transition}
        ~new_state:_ =
      return old_proof
  end

  include Coda.Make (Inputs) (Block_state_transition_proof)
end

module type Main_intf = sig
  module Inputs : Coda.Inputs_intf
end
