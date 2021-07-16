open Core_kernel
open Mina_base
open Snark_params.Tick

module Poly = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type ( 'staged_ledger_hash
           , 'snarked_ledger_hash
           , 'token_id
           , 'local_state
           , 'time )
           t =
        { staged_ledger_hash : 'staged_ledger_hash
        ; genesis_ledger_hash : 'snarked_ledger_hash
        ; registers :
            ( 'snarked_ledger_hash
            , unit
            , 'token_id
            , 'local_state )
            Registers.Stable.V1.t
        ; timestamp : 'time
        }
      [@@deriving sexp, fields, equal, compare, hash, yojson, hlist]
    end

    module V1 = struct
      type ('staged_ledger_hash, 'snarked_ledger_hash, 'token_id, 'time) t =
        { staged_ledger_hash : 'staged_ledger_hash
        ; snarked_ledger_hash : 'snarked_ledger_hash
        ; genesis_ledger_hash : 'snarked_ledger_hash
        ; snarked_next_available_token : 'token_id
        ; timestamp : 'time
        }
      [@@deriving sexp, fields, equal, compare, hash, yojson, hlist]

      let to_latest
          { staged_ledger_hash
          ; snarked_ledger_hash
          ; genesis_ledger_hash
          ; snarked_next_available_token
          ; timestamp
          } =
        { V2.staged_ledger_hash
        ; genesis_ledger_hash
        ; timestamp
        ; registers =
            { Registers.ledger = snarked_ledger_hash
            ; pending_coinbase_stack = ()
            ; next_available_token = snarked_next_available_token
            ; local_state = Local_state.dummy
            }
        }
    end
  end]
end

[%%define_locally
Poly.
  ( staged_ledger_hash
  , genesis_ledger_hash
  , timestamp
  , registers
  , to_hlist
  , of_hlist )]

let snarked_ledger_hash (t : _ Poly.t) = t.registers.ledger

let snarked_next_available_token (t : _ Poly.t) =
  t.registers.next_available_token

module Value = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t =
        ( Staged_ledger_hash.Stable.V1.t
        , Frozen_ledger_hash.Stable.V1.t
        , Token_id.Stable.V1.t
        , Local_state.Stable.V1.t
        , Block_time.Stable.V1.t )
        Poly.Stable.V2.t
      [@@deriving sexp, equal, compare, hash, yojson]

      let to_latest = Fn.id
    end

    module V1 = struct
      type t =
        ( Staged_ledger_hash.Stable.V1.t
        , Frozen_ledger_hash.Stable.V1.t
        , Token_id.Stable.V1.t
        , Block_time.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, equal, compare, hash, yojson]

      let to_latest (t : t) : V2.t = Poly.Stable.V1.to_latest t
    end
  end]
end

type var =
  ( Staged_ledger_hash.var
  , Frozen_ledger_hash.var
  , Token_id.var
  , Local_state.Checked.t
  , Block_time.Unpacked.var )
  Poly.t

let create_value ~staged_ledger_hash ~genesis_ledger_hash ~registers ~timestamp
    =
  { Poly.staged_ledger_hash; timestamp; genesis_ledger_hash; registers }

let typ : (var, Value.t) Typ.t =
  Typ.of_hlistable
    [ Staged_ledger_hash.typ
    ; Frozen_ledger_hash.typ
    ; Registers.typ
        [ Frozen_ledger_hash.typ; Typ.unit; Token_id.typ; Local_state.typ ]
    ; Block_time.Unpacked.typ
    ]
    ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
    ~value_of_hlist:of_hlist

module Impl = Pickles.Impls.Step

let var_to_input (type s)
    ({ staged_ledger_hash; genesis_ledger_hash; registers; timestamp } : var) :
    ((Field.Var.t, Boolean.var) Random_oracle.Input.t, s) Checked.t =
  Impl.make_checked (fun () ->
      let ( ! ) = Impl.run_checked in
      let open Random_oracle.Input in
      let registers =
        (* TODO: If this were the actual Registers itself (without the unit arg)
           then we could more efficiently deal with the transaction SNARK input
           (as we could reuse the hash)
        *)
        let { ledger
            ; pending_coinbase_stack = ()
            ; next_available_token
            ; local_state
            } =
          registers
        in
        Array.reduce_exn ~f:Random_oracle.Input.append
          [| Frozen_ledger_hash.var_to_input ledger
           ; !(Token_id.Checked.to_input next_available_token)
           ; Local_state.Checked.to_input local_state
          |]
      in
      List.reduce_exn ~f:append
        [ Staged_ledger_hash.var_to_input staged_ledger_hash
        ; Frozen_ledger_hash.var_to_input genesis_ledger_hash
        ; registers
        ; bitstring
            (Bitstring_lib.Bitstring.Lsb_first.to_list
               (Block_time.Unpacked.var_to_bits timestamp))
        ])
  |> Impl.Internal_Basic.(with_state (As_prover.return ()))

let to_input
    ({ staged_ledger_hash; genesis_ledger_hash; registers; timestamp } :
      Value.t) =
  let open Random_oracle.Input in
  let registers =
    (* TODO: If this were the actual Registers itself (without the unit arg)
       then we could more efficiently deal with the transaction SNARK input
       (as we could reuse the hash)
    *)
    let { ledger
        ; pending_coinbase_stack = ()
        ; next_available_token
        ; local_state
        } =
      registers
    in
    Array.reduce_exn ~f:append
      [| Frozen_ledger_hash.to_input ledger
       ; Token_id.to_input next_available_token
       ; Local_state.to_input local_state
      |]
  in
  List.reduce_exn ~f:append
    [ Staged_ledger_hash.to_input staged_ledger_hash
    ; Frozen_ledger_hash.to_input genesis_ledger_hash
    ; registers
    ; bitstring (Block_time.Bits.to_bits timestamp)
    ]

let set_timestamp t timestamp = { t with Poly.timestamp }

let negative_one
    ~(constraint_constants : Genesis_constants.Constraint_constants.t)
    ~(consensus_constants : Consensus.Constants.t) ~genesis_ledger_hash
    ~snarked_next_available_token : Value.t =
  { staged_ledger_hash =
      Staged_ledger_hash.genesis ~constraint_constants ~genesis_ledger_hash
  ; genesis_ledger_hash
  ; registers =
      { ledger = genesis_ledger_hash
      ; pending_coinbase_stack = ()
      ; next_available_token = snarked_next_available_token
      ; local_state = Local_state.dummy
      }
  ; timestamp = consensus_constants.genesis_state_timestamp
  }

(* negative_one and genesis blockchain states are equivalent *)
let genesis = negative_one

type display = (string, string, string, Local_state.display, string) Poly.t
[@@deriving yojson]

let display
    ({ staged_ledger_hash
     ; genesis_ledger_hash
     ; registers =
         { ledger
         ; pending_coinbase_stack = ()
         ; next_available_token
         ; local_state
         }
     ; timestamp
     } :
      Value.t) : display =
  { Poly.staged_ledger_hash =
      Visualization.display_prefix_of_string @@ Ledger_hash.to_string
      @@ Staged_ledger_hash.ledger_hash staged_ledger_hash
  ; genesis_ledger_hash =
      Visualization.display_prefix_of_string @@ Frozen_ledger_hash.to_string
      @@ genesis_ledger_hash
  ; registers =
      { ledger =
          Visualization.display_prefix_of_string
          @@ Frozen_ledger_hash.to_string ledger
      ; pending_coinbase_stack = ()
      ; next_available_token = Token_id.to_string next_available_token
      ; local_state = Local_state.display local_state
      }
  ; timestamp =
      Time.to_string_trimmed ~zone:Time.Zone.utc (Block_time.to_time timestamp)
  }
