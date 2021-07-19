# Redesign of the transaction execution model

## Summary

This proposes refactoring the transaction execution model, primarily to make it easy to implement Snapps transactions involving an arbitrary number of parties without having circuits that verify corresponding numbers of proofs. I.e., this model will make it possible to e.g., have a transaction involving 10 snapp accounts while only having a base transaction SNARK circuit that verifies a single proof.

## Introduction

Currently, the transaction execution part of the protocol can be thought of as running a state machine where the state consists of various things, roughly

- current staged ledger
- a signed amount (the fee excess)
- the pending coinbase stack
- the next available token ID
 
We propose extending this state to include an optional field called
`current_transaction_local_state` 
consisting of
- a hash, called `id`
- a signed amount, called `excess`
- an optional token ID, called `current_token_id`
- a non-empty stack of "parties" (described below), called `remaining_parties`

## Transactions
  
Under this approach, a transaction would semantically be a sequence of "parties". A "party" is a sequence of tuples `(authorization, predicate, update)` where

- An authorization is one of
	- a proof
	- a signature
	- nothing
- A predicate is an encoding of a function `Account.t -> bool`
- An update is an encoding of a function `Account.t -> Account.t`

For example, a normal payment transaction from an account at nonce `nonce` for amount `amount` with fee `fee` would be (in pseduocaml) the sequence

```ocaml
[ { authorization= Signature ...
  ; predicate= (fun a -> a.nonce == nonce)
  ; update= (fun a -> {a with balance= a.balance - (amount + fee)})
  }
; { authorization= Nothing
  ; predicate= (fun _ -> true)
  ; update= (fun a -> {a with balance= a.balance + amount})
  }
]
```

A token swap trading `n` of token `A` from `sender_A` for `m` of token `B` from `sender_B`, plus a fee payment of `fee` from `fee_payer` would look like
```ocaml
[ { authorization= Signature ...
  ; predicate= (fun a -> a.nonce == nonce_fee_payer)
  ; update= (fun a -> {a with balance= a.balance - fee})
  }
; { authorization= Signature ...
  ; predicate= (fun a -> a.nonce == nonce_A)
  ; update= (fun a -> {a with balance= a.balance - n})
  }
; { authorization= Nothing
  ; predicate= (fun _ -> a.token_id == A && a.public_key == sender_B)
  ; update= (fun a -> {a with balance= a.balance + n})
  }
; { authorization= Signature ...
  ; predicate= (fun _ -> a.token_id == B)
  ; update= (fun a -> {a with balance= a.balance - m})
  }
; { authorization= Nothing
  ; predicate= (fun _ -> a.token_id == B && a.public_key == sender_A)
  ; update= (fun a -> {a with balance= a.balance + m})
  }
]
```

The authorizations will be verified against the hash of the whole list of "parties".

When actually broadcast, transactions would be in an elaborated form containing witness information needed to actually execute them (for example, the account_id of each party), rather than the mere functions that constrain their execution, but this information is not needed inside the SNARK.

### How transaction execution would work semantically

Currently, the transitions in our state machine are individual transactions. This proposes extending that with the transitions

```
type transition =
  | Transaction of Transaction.t
  | Step_or_start_party_sequence of step_or_start

type step_or_start =
  | Step
  | Start of party list
```

It remains to explain how to execute a "party" as a state transition.
In pseudocaml/snarky, it will work as follows

```ocaml
let apply_step_or_start
  (e : execution_state) (instr : step_or_start)
  : execution_state
  =
  let local_state =
    match e.current_transaction_local_state, instr with
    | None, Step
    | Some _, Start _ -> assert false
    | None, Start ps ->
      {default_local_state with parties=ps; id= hash ps}
    | Some s, Step -> s
  in
  let {authorization; predicate; update}, remaining =
  	Non_empty_list.uncons local_state.remaining_parties
  in
  let a, merkle_path = exists ~request:Party_account in
  assert (implied_root a merkle_path e.ledger_hash = e.ledger_hash) ;
  assert (verify_authorization authorization a s.id) ;
  assert (verify_predicate predicate a) ;
  assert (auth_sufficient_given_permissions a.permissions authorization update) ;
  let fee_excess_change =
    match s.current_token_id with
    | None -> a.token_id
    | Some curr ->
      if curr == a.token_id
      then Currency.Fee.zero
      else (
        (* If we are switching tokens, then we cannot have printed money out of thin air. *)
        assert (s.excess >= 0);
        if curr == Token_id.default
        then s.excess
        else 0 (* burn the excess of this non-default token *)
      )
  in    
  let a' = perform_update update a in
  let excess = current_transaction_local_state.excess + (a.balance - a'.balance) in
  let new_ledger_hash = implied_root a' merkle_path in
  match remaining with
  | [] ->
  	assert (excess >= 0);
  	let fee_excess_change =
  	  fee_excess_change +
  	  if a.token_id == Token_id.default then excess else 0
  	in
  	{ e with current_transaction_local_state= None
  	; ledger_hash= new_ledger_hash
  	; fee_excess= e.fee_excess + fee_excess_change }
  | _::_ ->
  	{ e with current_transaction_local_state=
  	  Some
  	    { local_state with parties= remaining
  	    ; excess= local_state.excess + excess }
  	; ledger_hash= new_ledger_hash 
  	; fee_excess= e.fee_excess + excess_change }
```

### How this would boil down into "base" transaction SNARKs
The idea would be to have 3 new base transaction SNARKs corresponding to the 3 forms of authentication. Each would implement the above `apply_step_or_start` function but with the `verify_authorization` specialized to either signature verification, SNARK verification, or nothing.


Under this model, executing a transaction works as follows. Let `t = [t1; t2; t3]`

### Fees and proofs required

Instead of 2 proofs per transaction as is required now, we will switch to 2 proofs per "party".

Similarly, we should switch to ordering transactions in the transaction pool by `fee / number of parties`.

## Benefits of this approach

The main benefit of this approach is that we can have a small number of base circuits, each of which has at most one verifier inside of it, while still supporting transactions containing arbitrary numbers of parties and proofs. This enables such applications as multi-token swaps and snapp interactions involving arbitrarily many accounts.

Another benefit is the simplified and unified implementation for transaction application logic (both inside and outside the SNARK).

## Eliminating all other user-command types

Ideally, eventually, for simplicity, we will replace the implementation of transaction logic and transaction SNARK for the existing "special case" transactions (of payments and stake delegations) into sequences of "parties" as above. We can still keep the special case variants in the transaction type if desired.

If we do this in the most straightforward way, a payment would go from occupying one leaf in the scan state to either 2 or 3 (if there is a separate fee payer). However, the proofs corresponding to these leaves would be correspondingly simpler. That said, there probably would be some efficiency loss and so if we want to avoid that, we can make circuits that "unroll the loop" and execute several parties per circuit.

Specifically, for any sequence of authorization types, we can make a corresponding circuit to execute a sequence of parties with those authorization types. For example, it might be worth having a special circuit for the authorization type sequence `[ Signature; None ]` for a simple payment transaction that executes one party with a Signature authorization (the sender), and then one with no authorization (the receiver).

## Potential issues

- Backwards compatibility
	+ Before changing the special case transactions into the above, we will have to make sure all signers are updated as the signed payload will change.
- Transaction pool sorting
	+ Currently, transactions in the transaction pool are sorted by sender by nonce. If general sequences of parties are allowed as transactions, this will not work and we will have to figure out another way to order things.
