open Core_kernel
open Currency
open Mina_base
module Impl = Pickles.Impls.Step
include Parties_logic.Local_state.Value

type display =
  (string, string, string, string, bool, string) Parties_logic.Local_state.t
[@@deriving yojson]

let display
    ({ parties
     ; transaction_commitment
     ; token_id
     ; excess
     ; ledger
     ; success
     ; will_succeed
     } :
      t) : display =
  let f x =
    Visualization.display_prefix_of_string
      Zexe_backend.Pasta.(Bigint256.to_hex_string (Fp.to_bigint x))
  in
  { Parties_logic.Local_state.parties = f parties
  ; transaction_commitment = f transaction_commitment
  ; token_id = Token_id.to_string token_id
  ; excess = Amount.to_string excess
  ; ledger =
      Visualization.display_prefix_of_string
      @@ Frozen_ledger_hash.to_string ledger
  ; success
  ; will_succeed
  }

let dummy : t =
  { parties = Parties.With_hashes.empty
  ; transaction_commitment = Parties.Transaction_commitment.empty
  ; token_id = Token_id.default
  ; excess = Amount.zero
  ; ledger = Frozen_ledger_hash.empty_hash
  ; success = true
  ; will_succeed = true
  }

let empty = dummy

let gen : t Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let%map ledger = Frozen_ledger_hash.gen
  and excess = Amount.gen
  and transaction_commitment = Impl.Field.Constant.gen
  and parties = Impl.Field.Constant.gen
  and token_id = Token_id.gen
  and success = Bool.quickcheck_generator
  and will_succeed = Bool.quickcheck_generator in
  { Parties_logic.Local_state.parties
  ; transaction_commitment
  ; token_id
  ; ledger
  ; excess
  ; success
  ; will_succeed
  }

let to_input
    ({ parties
     ; transaction_commitment
     ; token_id
     ; excess
     ; ledger
     ; success
     ; will_succeed
     } :
      t) =
  let open Random_oracle.Input in
  Array.reduce_exn ~f:append
    [| field parties
     ; field transaction_commitment
     ; Token_id.to_input token_id
     ; Amount.to_input excess
     ; Ledger_hash.to_input ledger
     ; bitstring [ success ]
     ; bitstring [ will_succeed ]
    |]

module Checked = struct
  open Impl
  include Parties_logic.Local_state.Checked

  let assert_equal (t1 : t) (t2 : t) =
    let ( ! ) f x y = Impl.run_checked (f x y) in
    let f eq f = Core_kernel.Field.(eq (get f t1) (get f t2)) in
    Parties_logic.Local_state.Fields.iter ~parties:(f Field.Assert.equal)
      ~transaction_commitment:(f Field.Assert.equal)
      ~token_id:(f !Token_id.Checked.Assert.equal)
      ~excess:(f !Currency.Amount.Checked.assert_equal)
      ~ledger:(f !Ledger_hash.assert_equal)
      ~success:(f Impl.Boolean.Assert.( = ))
      ~will_succeed:(f Impl.Boolean.Assert.( = ))

  let equal' (t1 : t) (t2 : t) =
    let ( ! ) f x y = Impl.run_checked (f x y) in
    let f eq acc f = Core_kernel.Field.(eq (get f t1) (get f t2)) :: acc in
    Parties_logic.Local_state.Fields.fold ~init:[] ~parties:(f Field.equal)
      ~transaction_commitment:(f Field.equal)
      ~token_id:(f !Token_id.Checked.equal)
      ~excess:(f !Currency.Amount.Checked.equal)
      ~ledger:(f !Ledger_hash.equal_var) ~success:(f Impl.Boolean.equal)
      ~will_succeed:(f Impl.Boolean.equal)

  let to_input
      ({ parties
       ; transaction_commitment
       ; token_id
       ; excess
       ; ledger
       ; success
       ; will_succeed
       } :
        t) =
    let open Random_oracle.Input in
    Array.reduce_exn ~f:append
      [| field parties
       ; field transaction_commitment
       ; run_checked (Token_id.Checked.to_input token_id)
       ; Amount.var_to_input excess
       ; Ledger_hash.var_to_input ledger
       ; bitstring [ success ]
       ; bitstring [ will_succeed ]
      |]
end

let typ : (Checked.t, t) Impl.Typ.t =
  let open Parties_logic.Local_state in
  let open Impl in
  Typ.of_hlistable
    [ Field.typ
    ; Field.typ
    ; Token_id.typ
    ; Amount.typ
    ; Ledger_hash.typ
    ; Boolean.typ
    ; Boolean.typ
    ]
    ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
    ~value_of_hlist:of_hlist
