open Core
open Import
open Snark_params
open Currency

include Merkle_ledger.Ledger.Make (Public_key.Compressed) (Account)
          (struct
            type t = Merkle_hash.t [@@deriving sexp, hash, compare, bin_io]

            let merge = Merkle_hash.merge

            let empty = Merkle_hash.empty_hash

            let hash_account = Fn.compose Merkle_hash.of_digest Account.digest
          end)
          (struct
            let depth = ledger_depth
          end)

let merkle_root t =
  Ledger_hash.of_hash (merkle_root t :> Tick.Pedersen.Digest.t)

let error s = Or_error.errorf "Ledger.apply_transaction: %s" s

let error_opt e = Option.value_map ~default:(error e) ~f:Or_error.return

let get' ledger tag location =
  error_opt (sprintf "%s account not found" tag) (get ledger location)

let location_of_key' ledger tag key =
  error_opt (sprintf "%s location not found" tag) (location_of_key ledger key)

let add_amount balance amount =
  error_opt "overflow" (Balance.add_amount balance amount)

let sub_amount balance amount =
  error_opt "insufficient funds" (Balance.sub_amount balance amount)

let add_fee amount fee =
  error_opt "add_fee: overflow" (Amount.add_fee amount fee)

let validate_nonces txn_nonce account_nonce =
  if Account.Nonce.equal account_nonce txn_nonce then Or_error.return ()
  else
    Or_error.errorf
      !"Nonce in account %{sexp: Account.Nonce.t} different from nonce in \
        transaction %{sexp: Account.Nonce.t}"
      account_nonce txn_nonce

module Undo = struct
  type transaction =
    { transaction: Transaction.t
    ; previous_receipt_chain_hash: Receipt.Chain_hash.t }
  [@@deriving sexp]

  type t =
    | Transaction of transaction
    | Fee_transfer of Fee_transfer.t
    | Coinbase of Coinbase.t
  [@@deriving sexp]
end

(* someday: It would probably be better if we didn't modify the receipt chain hash
   in the case that the sender is equal to the receiver, but it complicates the SNARK, so
   we don't for now. *)
let apply_transaction_unchecked ledger
    ({payload; sender; signature= _} as transaction: Transaction.t) =
  let sender = Public_key.compress sender in
  let {Transaction.Payload.fee; amount; receiver; nonce} = payload in
  let open Or_error.Let_syntax in
  let%bind sender_location = location_of_key' ledger "" sender in
  let%bind sender_account = get' ledger "sender" sender_location in
  let%bind () = validate_nonces nonce sender_account.nonce in
  let%bind sender_balance' =
    let%bind amount_and_fee = add_fee amount fee in
    sub_amount sender_account.balance amount_and_fee
  in
  let sender_account_without_balance_modified =
    { sender_account with
      nonce= Account.Nonce.succ sender_account.nonce
    ; receipt_chain_hash=
        Receipt.Chain_hash.cons payload sender_account.receipt_chain_hash }
  in
  let undo =
    { Undo.transaction
    ; previous_receipt_chain_hash= sender_account.receipt_chain_hash }
  in
  if Public_key.Compressed.equal sender receiver then (
    ignore
    @@ set ledger sender_location sender_account_without_balance_modified ;
    return undo )
  else
    let%bind receiver_location = location_of_key' ledger "reciever" receiver in
    let%bind receiver_account = get' ledger "receiver" receiver_location in
    let%map receiver_balance' = add_amount receiver_account.balance amount in
    set ledger sender_location
      {sender_account_without_balance_modified with balance= sender_balance'} ;
    set ledger receiver_location
      {receiver_account with balance= receiver_balance'} ;
    undo

let apply_transaction ledger (transaction: Transaction.With_valid_signature.t) =
  apply_transaction_unchecked ledger (transaction :> Transaction.t)

let process_fee_transfer t (transfer: Fee_transfer.t) ~modify_balance =
  let open Or_error.Let_syntax in
  match transfer with
  | One (pk, fee) ->
      let%bind pk_location = location_of_key' t "One-pk" pk in
      let%map a = get' t "One-pk" pk_location in
      set t pk_location {a with balance= modify_balance a.balance fee}
  | Two ((pk1, fee1), (pk2, fee2)) ->
      let%bind l1 = location_of_key' t "Two-pk1" pk1
      and l2 = location_of_key' t "Two-pk2" pk2 in
      let%map a1 = get' t "Two-pk1" l1 and a2 = get' t "Two-pk2" l2 in
      set t l1 {a1 with balance= modify_balance a1.balance fee1} ;
      set t l2 {a2 with balance= modify_balance a2.balance fee2}

let apply_fee_transfer t transfer =
  process_fee_transfer t transfer ~modify_balance:(fun b f ->
      Option.value_exn (Balance.add_amount b (Amount.of_fee f)) )

let undo_fee_transfer t transfer =
  process_fee_transfer t transfer ~modify_balance:(fun b f ->
      Option.value_exn (Balance.sub_amount b (Amount.of_fee f)) )

(* TODO: Better system needed for making atomic changes. Could use a monad. *)
let apply_coinbase t ({proposer; fee_transfer}: Coinbase.t) =
  let get_or_initialize pk =
    match location_of_key t pk with
    | None -> (None, Account.initialize pk)
    | Some l -> (Some l, get t l |> Option.value_exn)
  in
  let update_account location pk account =
    match location with
    | None -> create_account_exn t pk account |> ignore
    | Some l -> set t l account
  in
  let open Or_error.Let_syntax in
  let%bind proposer_reward, receiver_update =
    match fee_transfer with
    | None -> return (Protocols.Coda_praos.coinbase_amount, None)
    | Some (receiver, fee) ->
        let fee = Amount.of_fee fee in
        let%bind proposer_reward =
          error_opt "Coinbase fee transfer too large"
            (Amount.sub Protocols.Coda_praos.coinbase_amount fee)
        in
        let reciever_location, receiver_account = get_or_initialize receiver in
        let%map balance = add_amount receiver_account.balance fee in
        ( proposer_reward
        , Some (receiver, reciever_location, {receiver_account with balance})
        )
  in
  let proposer_location, proposer_account = get_or_initialize proposer in
  let%map balance = add_amount proposer_account.balance proposer_reward in
  update_account proposer_location proposer {proposer_account with balance} ;
  Option.iter receiver_update ~f:(fun (k, l, a) -> update_account l k a)

(* Don't have to be atomic here because these should never fail. In fact, none of
   the undo functions should ever return an error. This should be fixed in the types. *)
let undo_coinbase t ({proposer; fee_transfer}: Coinbase.t) =
  let proposer_reward =
    match fee_transfer with
    | None -> Protocols.Coda_praos.coinbase_amount
    | Some (receiver, fee) ->
        let fee = Amount.of_fee fee in
        let reciever_location =
          Or_error.ok_exn (location_of_key' t "reciever" receiver)
        in
        let receiver_account =
          Or_error.ok_exn (get' t "receiver" reciever_location)
        in
        set t reciever_location
          { receiver_account with
            balance=
              Option.value_exn
                (Balance.sub_amount receiver_account.balance fee) } ;
        Amount.sub Protocols.Coda_praos.coinbase_amount fee |> Option.value_exn
  in
  let proposer_location =
    Or_error.ok_exn (location_of_key' t "reciever" proposer)
  in
  let proposer_account =
    Or_error.ok_exn (get' t "proposer" proposer_location)
  in
  set t proposer_location
    { proposer_account with
      balance=
        Option.value_exn
          (Balance.sub_amount proposer_account.balance proposer_reward) }

let undo_transaction ledger
    { Undo.transaction= {payload; sender; signature= _}
    ; previous_receipt_chain_hash } =
  let sender = Public_key.compress sender in
  let {Transaction.Payload.fee; amount; receiver; nonce} = payload in
  let open Or_error.Let_syntax in
  let%bind sender_location = location_of_key' ledger "sender" sender in
  let%bind sender_account = get' ledger "sender" sender_location in
  let%bind sender_balance' =
    let%bind amount_and_fee = add_fee amount fee in
    add_amount sender_account.balance amount_and_fee
  in
  let%bind () =
    validate_nonces (Account.Nonce.succ nonce) sender_account.nonce
  in
  let sender_account_without_balance_modified =
    {sender_account with nonce; receipt_chain_hash= previous_receipt_chain_hash}
  in
  if Public_key.Compressed.equal sender receiver then (
    set ledger sender_location sender_account_without_balance_modified ;
    return () )
  else
    let%bind reciever_location = location_of_key' ledger "receiver" receiver in
    let%bind receiver_account = get' ledger "receiver" reciever_location in
    let%map receiver_balance' = sub_amount receiver_account.balance amount in
    set ledger sender_location
      {sender_account_without_balance_modified with balance= sender_balance'} ;
    set ledger reciever_location
      {receiver_account with balance= receiver_balance'}

let undo : t -> Undo.t -> unit Or_error.t =
 fun ledger undo ->
  match undo with
  | Fee_transfer t -> undo_fee_transfer ledger t
  | Transaction u -> undo_transaction ledger u
  | Coinbase c -> undo_coinbase ledger c ; Ok ()

let apply_super_transaction ledger (t: Super_transaction.t) =
  match t with
  | Transaction txn ->
      Or_error.map (apply_transaction ledger txn) ~f:(fun u ->
          Undo.Transaction u )
  | Fee_transfer t ->
      Or_error.map (apply_fee_transfer ledger t) ~f:(fun () ->
          Undo.Fee_transfer t )
  | Coinbase t ->
      Or_error.map (apply_coinbase ledger t) ~f:(fun () -> Undo.Coinbase t)

let merkle_root_after_transaction_exn ledger transaction =
  let undo = Or_error.ok_exn (apply_transaction ledger transaction) in
  let root = merkle_root ledger in
  Or_error.ok_exn (undo_transaction ledger undo) ;
  root

let merkle_root_after_transactions t ts =
  let undos_rev =
    List.rev_map ts ~f:(fun txn -> Or_error.ok_exn (apply_transaction t txn))
  in
  let root = merkle_root t in
  List.iter undos_rev ~f:(fun u -> Or_error.ok_exn (undo_transaction t u)) ;
  root
