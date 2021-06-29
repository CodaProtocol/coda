(* processor.ml -- database processing for archive node *)

module Archive_rpc = Rpc
open Async
open Core
open Caqti_async
open Mina_base
open Mina_state
open Mina_transition
open Pipe_lib
open Signature_lib
open Pickles_types

module Caqti_type_spec = struct
  type (_, _) t =
    | [] : (unit, unit) t
    | ( :: ) : 'c Caqti_type.t * ('a, 'b) t -> ('c -> 'a, 'c * 'b) t

  let rec to_rep : 'hlist 'tuple. ('hlist, 'tuple) t -> 'tuple Caqti_type.t =
    fun (type hlist tuple) (spec : (hlist, tuple) t) ->
     match spec with
     | [] ->
         (Caqti_type.unit : tuple Caqti_type.t)
     | rep :: spec ->
         Caqti_type.tup2 rep (to_rep spec)

  let rec hlist_to_tuple :
            'hlist 'tuple.    ('hlist, 'tuple) t -> (unit, 'hlist) H_list.t
            -> 'tuple =
    fun (type hlist tuple) (spec : (hlist, tuple) t)
        (l : (unit, hlist) H_list.t) ->
     match (spec, l) with
     | [], [] ->
         (() : tuple)
     | _ :: spec, x :: l ->
         ((x, hlist_to_tuple spec l) : tuple)

  let rec tuple_to_hlist :
            'hlist 'tuple.    ('hlist, 'tuple) t -> 'tuple
            -> (unit, 'hlist) H_list.t =
    fun (type hlist tuple) (spec : (hlist, tuple) t) (t : tuple) ->
     match (spec, t) with
     | [], () ->
         ([] : (unit, hlist) H_list.t)
     | _ :: spec, (x, t) ->
         x :: tuple_to_hlist spec t
end

let rec vector : type t n.
    n Nat.t -> t Caqti_type.t -> (t, n) Vector.t Caqti_type.t =
 fun n t ->
  match n with
  | Z ->
      Caqti_type.(custom unit)
        ~encode:(fun Vector.[] -> Ok ())
        ~decode:(fun () -> Ok Vector.[])
  | S n ->
      let r = vector n t in
      Caqti_type.(custom (tup2 t r))
        ~encode:(fun Vector.(x :: xs) -> Ok (x, xs))
        ~decode:(fun (x, xs) -> Ok (x :: xs))

(* process a Caqti query on list of items
   if we were instead to simply map the query over the list,
    we'd get "in use" assertion failures for the connection
   the bind makes sure the connection is available for
    each query
*)
let rec deferred_result_list_fold ls ~init ~f =
  let open Deferred.Result.Let_syntax in
  match ls with
  | [] ->
      return init
  | h :: t ->
      let%bind init = f init h in
      deferred_result_list_fold t ~init ~f

let query ~f pool =
  match%bind Caqti_async.Pool.use f pool with
  | Ok v ->
      return v
  | Error msg ->
      failwithf "Error querying db, error: %s" (Caqti_error.show msg) ()

module Public_key = struct
  let find (module Conn : CONNECTION) (t : Public_key.Compressed.t) =
    let public_key = Public_key.Compressed.to_base58_check t in
    Conn.find
      (Caqti_request.find Caqti_type.string Caqti_type.int
         "SELECT id FROM public_keys WHERE value = ?")
      public_key

  let find_opt (module Conn : CONNECTION) (t : Public_key.Compressed.t) =
    let public_key = Public_key.Compressed.to_base58_check t in
    Conn.find_opt
      (Caqti_request.find_opt Caqti_type.string Caqti_type.int
         "SELECT id FROM public_keys WHERE value = ?")
      public_key

  let add_if_doesn't_exist (module Conn : CONNECTION)
      (t : Public_key.Compressed.t) =
    let open Deferred.Result.Let_syntax in
    match%bind find_opt (module Conn) t with
    | Some id ->
        return id
    | None ->
        let public_key = Public_key.Compressed.to_base58_check t in
        Conn.find
          (Caqti_request.find Caqti_type.string Caqti_type.int
             "INSERT INTO public_keys (value) VALUES (?) RETURNING id")
          public_key
end

module Timing_info = struct
  type t =
    { public_key_id: int
    ; token: int64
    ; initial_balance: int64
    ; initial_minimum_balance: int64
    ; cliff_time: int64
    ; cliff_amount: int64
    ; vesting_period: int64
    ; vesting_increment: int64 }
  [@@deriving hlist]

  let typ =
    let open Caqti_type_spec in
    let spec =
      Caqti_type.[int; int64; int64; int64; int64; int64; int64; int64]
    in
    let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
    let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
    Caqti_type.custom ~encode ~decode (to_rep spec)

  let find (module Conn : CONNECTION) (acc : Account.t) =
    let open Deferred.Result.Let_syntax in
    let%bind pk_id = Public_key.find (module Conn) acc.public_key in
    Conn.find
      (Caqti_request.find Caqti_type.int typ
         {sql| SELECT public_key_id, token, initial_balance,
                      initial_minimum_balance, cliff_time, cliff_amount,
                      vesting_period, vesting_increment
               FROM timing_info
               WHERE public_key_id = ?
         |sql})
      pk_id

  let find_by_pk_opt (module Conn : CONNECTION) public_key =
    let open Deferred.Result.Let_syntax in
    let%bind pk_id = Public_key.find (module Conn) public_key in
    Conn.find_opt
      (Caqti_request.find_opt Caqti_type.int typ
         {sql| SELECT public_key_id, token, initial_balance,
                     initial_minimum_balance, cliff_time, cliff_amount,
                     vesting_period, vesting_increment
               FROM timing_info
               WHERE public_key_id = ?
         |sql})
      pk_id

  let add_if_doesn't_exist (module Conn : CONNECTION) (acc : Account.t) =
    let open Deferred.Result.Let_syntax in
    let amount_to_int64 x =
      Unsigned.UInt64.to_int64 (Currency.Amount.to_uint64 x)
    in
    let balance_to_int64 x = amount_to_int64 (Currency.Balance.to_amount x) in
    let slot_to_int64 x =
      Mina_numbers.Global_slot.to_uint32 x |> Unsigned.UInt32.to_int64
    in
    let%bind public_key_id =
      Public_key.add_if_doesn't_exist (module Conn) acc.public_key
    in
    match%bind
      Conn.find_opt
        (Caqti_request.find_opt Caqti_type.int Caqti_type.int
           "SELECT id FROM timing_info WHERE public_key_id = ?")
        public_key_id
    with
    | Some id ->
        return id
    | None ->
        let values =
          let token =
            Token_id.to_uint64 (Account.token acc) |> Unsigned.UInt64.to_int64
          in
          match acc.timing with
          | Timed timing ->
              { public_key_id
              ; token
              ; initial_balance= balance_to_int64 acc.balance
              ; initial_minimum_balance=
                  balance_to_int64 timing.initial_minimum_balance
              ; cliff_time= slot_to_int64 timing.cliff_time
              ; cliff_amount= amount_to_int64 timing.cliff_amount
              ; vesting_period= slot_to_int64 timing.vesting_period
              ; vesting_increment= amount_to_int64 timing.vesting_increment }
          | Untimed ->
              let zero = Int64.zero in
              { public_key_id
              ; token
              ; initial_balance= balance_to_int64 acc.balance
              ; initial_minimum_balance= zero
              ; cliff_time= zero
              ; cliff_amount= zero
              ; vesting_period= zero
              ; vesting_increment= zero }
        in
        Conn.find
          (Caqti_request.find typ Caqti_type.int
             {sql| INSERT INTO timing_info
                    (public_key_id,token,initial_balance,initial_minimum_balance,
                     cliff_time, cliff_amount, vesting_period, vesting_increment)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                   RETURNING id
             |sql})
          values
end

module Snarked_ledger_hash = struct
  let find (module Conn : CONNECTION) (t : Frozen_ledger_hash.t) =
    let hash = Frozen_ledger_hash.to_string t in
    Conn.find
      (Caqti_request.find Caqti_type.string Caqti_type.int
         "SELECT id FROM snarked_ledger_hashes WHERE value = ?")
      hash

  let add_if_doesn't_exist (module Conn : CONNECTION)
      (t : Frozen_ledger_hash.t) =
    let open Deferred.Result.Let_syntax in
    let hash = Frozen_ledger_hash.to_string t in
    match%bind
      Conn.find_opt
        (Caqti_request.find_opt Caqti_type.string Caqti_type.int
           "SELECT id FROM snarked_ledger_hashes WHERE value = ?")
        hash
    with
    | Some id ->
        return id
    | None ->
        Conn.find
          (Caqti_request.find Caqti_type.string Caqti_type.int
             "INSERT INTO snarked_ledger_hashes (value) VALUES (?) RETURNING id")
          hash
end

module Epoch_data = struct
  type t = {seed: string; ledger_hash_id: int}

  let typ =
    let encode t = Ok (t.seed, t.ledger_hash_id) in
    let decode (seed, ledger_hash_id) = Ok {seed; ledger_hash_id} in
    let rep = Caqti_type.(tup2 string int) in
    Caqti_type.custom ~encode ~decode rep

  (* for extensional blocks, we have just the seed and ledger hash *)
  let add_from_seed_and_ledger_hash_id (module Conn : CONNECTION) ~seed
      ~ledger_hash_id =
    let open Deferred.Result.Let_syntax in
    let seed = Epoch_seed.to_string seed in
    match%bind
      Conn.find_opt
        (Caqti_request.find_opt typ Caqti_type.int
           "SELECT id FROM epoch_data WHERE seed = ? AND ledger_hash_id = ?")
        {seed; ledger_hash_id}
    with
    | Some id ->
        return id
    | None ->
        Conn.find
          (Caqti_request.find typ Caqti_type.int
             {sql| INSERT INTO epoch_data (seed, ledger_hash_id) VALUES (?, ?)
                   RETURNING id
             |sql})
          {seed; ledger_hash_id}

  let add_if_doesn't_exist (module Conn : CONNECTION)
      (t : Mina_base.Epoch_data.Value.t) =
    let open Deferred.Result.Let_syntax in
    let Mina_base.Epoch_ledger.Poly.{hash; _} =
      Mina_base.Epoch_data.Poly.ledger t
    in
    let%bind ledger_hash_id =
      Snarked_ledger_hash.add_if_doesn't_exist (module Conn) hash
    in
    add_from_seed_and_ledger_hash_id
      (module Conn)
      ~seed:(Mina_base.Epoch_data.Poly.seed t)
      ~ledger_hash_id
end

module User_command = struct
  module Signed_command = struct
    type t =
      { typ: string
      ; fee_payer_id: int
      ; source_id: int
      ; receiver_id: int
      ; fee_token: int64
      ; token: int64
      ; nonce: int
      ; amount: int64 option
      ; fee: int64
      ; valid_until: int64 option
      ; memo: string
      ; hash: string }
    [@@deriving hlist]

    let typ =
      let open Caqti_type_spec in
      let spec =
        Caqti_type.
          [ string
          ; int
          ; int
          ; int
          ; int64
          ; int64
          ; int
          ; option int64
          ; int64
          ; option int64
          ; string
          ; string ]
      in
      let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
      let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
      Caqti_type.custom ~encode ~decode (to_rep spec)

    let find (module Conn : CONNECTION)
        ~(transaction_hash : Transaction_hash.t) =
      Conn.find_opt
        (Caqti_request.find_opt Caqti_type.string Caqti_type.int
           "SELECT id FROM user_commands WHERE hash = ?")
        (Transaction_hash.to_base58_check transaction_hash)

    let load (module Conn : CONNECTION) ~(id : int) =
      Conn.find
        (Caqti_request.find Caqti_type.int typ
           {sql| SELECT type,fee_payer_id,source_id,receiver_id,
                 fee_token,token,
                 nonce,amount,fee,valid_until,memo,hash
                 FROM user_commands
                 WHERE id = ?
           |sql})
        id

    type balance_public_key_ids =
      {fee_payer_id: int; source_id: int; receiver_id: int}

    let add_balance_public_keys_if_don't_exist (module Conn : CONNECTION)
        (t : Signed_command.t) =
      let open Deferred.Result.Let_syntax in
      let%bind fee_payer_id =
        Public_key.add_if_doesn't_exist
          (module Conn)
          (Signed_command.fee_payer_pk t)
      in
      let%bind source_id =
        Public_key.add_if_doesn't_exist
          (module Conn)
          (Signed_command.source_pk t)
      in
      let%map receiver_id =
        Public_key.add_if_doesn't_exist
          (module Conn)
          (Signed_command.receiver_pk t)
      in
      {fee_payer_id; source_id; receiver_id}

    let add_if_doesn't_exist ?(via = `Ident) (module Conn : CONNECTION)
        (t : Signed_command.t) =
      let open Deferred.Result.Let_syntax in
      let transaction_hash =
        Transaction_hash.hash_command (Signed_command t)
      in
      match%bind find (module Conn) ~transaction_hash with
      | Some user_command_id ->
          return user_command_id
      | None ->
          let%bind {fee_payer_id; source_id; receiver_id} =
            add_balance_public_keys_if_don't_exist (module Conn) t
          in
          let valid_until =
            let open Mina_numbers in
            let slot = Signed_command.valid_until t in
            if Global_slot.equal slot Global_slot.max_value then None
            else
              Some
                ( slot |> Mina_numbers.Global_slot.to_uint32
                |> Unsigned.UInt32.to_int64 )
          in
          (* TODO: Converting these uint64s to int64 can overflow; see #5419 *)
          Conn.find
            (Caqti_request.find typ Caqti_type.int
               {sql| INSERT INTO user_commands (type, fee_payer_id, source_id,
                      receiver_id, fee_token, token, nonce, amount, fee,
                      valid_until, memo, hash)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    RETURNING id |sql})
            { typ=
                ( match via with
                | `Ident ->
                    Signed_command.tag_string t
                | `Snapp_command ->
                    "snapp" )
            ; fee_payer_id
            ; source_id
            ; receiver_id
            ; fee_token=
                Signed_command.fee_token t |> Token_id.to_uint64
                |> Unsigned.UInt64.to_int64
            ; token=
                Signed_command.token t |> Token_id.to_uint64
                |> Unsigned.UInt64.to_int64
            ; nonce= Signed_command.nonce t |> Unsigned.UInt32.to_int
            ; amount=
                Signed_command.amount t
                |> Core.Option.map ~f:(fun amt ->
                       Currency.Amount.to_uint64 amt
                       |> Unsigned.UInt64.to_int64 )
            ; fee=
                ( Signed_command.fee t
                |> fun amt ->
                Currency.Fee.to_uint64 amt |> Unsigned.UInt64.to_int64 )
            ; valid_until
            ; memo= Signed_command.memo t |> Signed_command_memo.to_string
            ; hash= transaction_hash |> Transaction_hash.to_base58_check }
  end

  let as_signed_command (t : User_command.t) : Mina_base.Signed_command.t =
    match t with
    | Signed_command c ->
        c
    | Snapp_command c ->
        let module S = Mina_base.Snapp_command in
        let ({source; receiver; amount} : S.transfer) = S.as_transfer c in
        let fee_payer = S.fee_payer c in
        { signature= Signature.dummy
        ; signer= Snark_params.Tick.Field.(zero, zero)
        ; payload=
            { common=
                { fee= S.fee_exn c
                ; fee_token= Account_id.token_id fee_payer
                ; fee_payer_pk= Account_id.public_key fee_payer
                ; nonce=
                    Option.value (S.nonce c)
                      ~default:Mina_numbers.Account_nonce.zero
                ; valid_until= Mina_numbers.Global_slot.max_value
                ; memo= Signed_command_memo.create_from_string_exn "snapp" }
            ; body=
                Payment
                  { source_pk= source
                  ; receiver_pk= receiver
                  ; token_id= S.token_id c
                  ; amount } } }

  let via (t : User_command.t) : [`Snapp_command | `Ident] =
    match t with
    | Signed_command _ ->
        `Ident
    | Snapp_command _ ->
        `Snapp_command

  let add_if_doesn't_exist conn (t : User_command.t) =
    Signed_command.add_if_doesn't_exist conn ~via:(via t) (as_signed_command t)

  let find conn ~(transaction_hash : Transaction_hash.t) =
    Signed_command.find conn ~transaction_hash

  (* meant to work with either a signed command, or a snapp *)
  let add_extensional (module Conn : CONNECTION)
      (user_cmd : Extensional.User_command.t) =
    let amount_opt_to_int64_opt amt_opt =
      Option.map amt_opt
        ~f:(Fn.compose Unsigned.UInt64.to_int64 Currency.Amount.to_uint64)
    in
    let open Deferred.Result.Let_syntax in
    let%bind fee_payer_id =
      Public_key.add_if_doesn't_exist (module Conn) user_cmd.fee_payer
    in
    let%bind source_id =
      Public_key.add_if_doesn't_exist (module Conn) user_cmd.source
    in
    let%bind receiver_id =
      Public_key.add_if_doesn't_exist (module Conn) user_cmd.receiver
    in
    Conn.find
      (Caqti_request.find Signed_command.typ Caqti_type.int
         {sql| INSERT INTO user_commands (type, fee_payer_id, source_id,
                      receiver_id, fee_token, token, nonce, amount, fee,
                      valid_until, memo, hash)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    RETURNING id
         |sql})
      { typ= user_cmd.typ
      ; fee_payer_id
      ; source_id
      ; receiver_id
      ; fee_token=
          user_cmd.fee_token |> Token_id.to_uint64 |> Unsigned.UInt64.to_int64
      ; token= user_cmd.token |> Token_id.to_uint64 |> Unsigned.UInt64.to_int64
      ; nonce= user_cmd.nonce |> Unsigned.UInt32.to_int
      ; amount= user_cmd.amount |> amount_opt_to_int64_opt
      ; fee=
          user_cmd.fee
          |> Fn.compose Unsigned.UInt64.to_int64 Currency.Fee.to_uint64
      ; valid_until=
          Option.map user_cmd.valid_until
            ~f:
              (Fn.compose Unsigned.UInt32.to_int64
                 Mina_numbers.Global_slot.to_uint32)
      ; memo= user_cmd.memo |> Signed_command_memo.to_string
      ; hash= user_cmd.hash |> Transaction_hash.to_base58_check }

  let add_extensional_if_doesn't_exist (module Conn : CONNECTION)
      (user_cmd : Extensional.User_command.t) =
    let open Deferred.Result.Let_syntax in
    match%bind find (module Conn) ~transaction_hash:user_cmd.hash with
    | None ->
        add_extensional (module Conn) user_cmd
    | Some user_cmd_id ->
        return user_cmd_id
end

module Internal_command = struct
  type t =
    {typ: string; receiver_id: int; fee: int64; token: int64; hash: string}

  let typ =
    let encode t = Ok ((t.typ, t.receiver_id, t.fee, t.token), t.hash) in
    let decode ((typ, receiver_id, fee, token), hash) =
      Ok {typ; receiver_id; fee; token; hash}
    in
    let rep = Caqti_type.(tup2 (tup4 string int int64 int64) string) in
    Caqti_type.custom ~encode ~decode rep

  let find (module Conn : CONNECTION) ~(transaction_hash : Transaction_hash.t)
      ~(typ : string) =
    Conn.find_opt
      (Caqti_request.find_opt
         Caqti_type.(tup2 string string)
         Caqti_type.int
         "SELECT id FROM internal_commands WHERE hash = $1 AND type = $2")
      (Transaction_hash.to_base58_check transaction_hash, typ)

  let load (module Conn : CONNECTION) ~(id : int) =
    Conn.find
      (Caqti_request.find Caqti_type.int typ
         {sql| SELECT type,receiver_id,fee,token,hash
               FROM internal_commands
               WHERE id = ?
         |sql})
      id

  let add_extensional_if_doesn't_exist (module Conn : CONNECTION)
      (internal_cmd : Extensional.Internal_command.t) =
    let open Deferred.Result.Let_syntax in
    match%bind
      find
        (module Conn)
        ~transaction_hash:internal_cmd.hash ~typ:internal_cmd.typ
    with
    | Some internal_command_id ->
        return internal_command_id
    | None ->
        let%bind receiver_id =
          Public_key.add_if_doesn't_exist (module Conn) internal_cmd.receiver
        in
        Conn.find
          (Caqti_request.find typ Caqti_type.int
             {sql| INSERT INTO internal_commands
                    (type, receiver_id, fee, token,hash)
                   VALUES (?, ?, ?, ?, ?)
                   RETURNING id
             |sql})
          { typ= internal_cmd.typ
          ; receiver_id
          ; fee=
              internal_cmd.fee |> Currency.Fee.to_uint64
              |> Unsigned.UInt64.to_int64
          ; token=
              internal_cmd.token |> Token_id.to_uint64
              |> Unsigned.UInt64.to_int64
          ; hash= internal_cmd.hash |> Transaction_hash.to_base58_check }
end

module Fee_transfer = struct
  module Kind = struct
    type t = [`Normal | `Via_coinbase]

    let to_string : t -> string = function
      | `Normal ->
          "fee_transfer"
      | `Via_coinbase ->
          "fee_transfer_via_coinbase"
  end

  type t =
    {kind: Kind.t; receiver_id: int; fee: int; token: int64; hash: string}

  let typ =
    let encode t =
      let kind = Kind.to_string t.kind in
      Ok ((kind, t.receiver_id, t.fee, t.token), t.hash)
    in
    let decode ((kind, receiver_id, fee, token), hash) =
      let open Result.Let_syntax in
      let%bind kind =
        match kind with
        | "fee_transfer" ->
            return `Normal
        | "fee_transfer_via_coinbase" ->
            return `Via_coinbase
        | s ->
            Result.fail (sprintf "Bad kind %s in decode attempt" s)
      in
      Ok {kind; receiver_id; fee; token; hash}
    in
    let rep = Caqti_type.(tup2 (tup4 string int int int64) string) in
    Caqti_type.custom ~encode ~decode rep

  let add_if_doesn't_exist (module Conn : CONNECTION)
      (t : Fee_transfer.Single.t) (kind : [`Normal | `Via_coinbase]) =
    let open Deferred.Result.Let_syntax in
    let transaction_hash = Transaction_hash.hash_fee_transfer t in
    match%bind
      Internal_command.find
        (module Conn)
        ~transaction_hash ~typ:(Kind.to_string kind)
    with
    | Some internal_command_id ->
        return internal_command_id
    | None ->
        let%bind receiver_id =
          Public_key.add_if_doesn't_exist
            (module Conn)
            (Fee_transfer.Single.receiver_pk t)
        in
        Conn.find
          (Caqti_request.find typ Caqti_type.int
             {sql| INSERT INTO internal_commands
                    (type, receiver_id, fee, token, hash)
                   VALUES (?, ?, ?, ?, ?)
                   RETURNING id
             |sql})
          { kind
          ; receiver_id
          ; fee= Fee_transfer.Single.fee t |> Currency.Fee.to_int
          ; token= Token_id.to_string t.fee_token |> Int64.of_string
          ; hash= transaction_hash |> Transaction_hash.to_base58_check }
end

module Coinbase = struct
  type t = {receiver_id: int; amount: int; hash: string}

  let coinbase_typ = "coinbase"

  let typ =
    let encode t =
      Ok
        ( ( coinbase_typ
          , t.receiver_id
          , t.amount
          , Token_id.(to_string default) |> Int64.of_string )
        , t.hash )
    in
    let decode ((_, receiver_id, amount, _), hash) =
      Ok {receiver_id; amount; hash}
    in
    let rep = Caqti_type.(tup2 (tup4 string int int int64) string) in
    Caqti_type.custom ~encode ~decode rep

  let add_if_doesn't_exist (module Conn : CONNECTION) (t : Coinbase.t) =
    let open Deferred.Result.Let_syntax in
    let transaction_hash = Transaction_hash.hash_coinbase t in
    match%bind
      Internal_command.find (module Conn) ~transaction_hash ~typ:coinbase_typ
    with
    | Some internal_command_id ->
        return internal_command_id
    | None ->
        let%bind receiver_id =
          Public_key.add_if_doesn't_exist
            (module Conn)
            (Coinbase.receiver_pk t)
        in
        Conn.find
          (Caqti_request.find typ Caqti_type.int
             {sql| INSERT INTO internal_commands
                    (type, receiver_id, fee, token, hash)
                   VALUES (?, ?, ?, ?, ?)
                   RETURNING id
             |sql})
          { receiver_id
          ; amount= Coinbase.amount t |> Currency.Amount.to_int
          ; hash= transaction_hash |> Transaction_hash.to_base58_check }
end

module Balance = struct
  type t = {id: int; public_key_id: int; balance: int64} [@@deriving hlist]

  let typ =
    let open Caqti_type_spec in
    let spec = Caqti_type.[int; int; int64] in
    let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
    let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
    Caqti_type.custom ~encode ~decode (to_rep spec)

  let balance_to_int64 (balance : Currency.Balance.t) : int64 =
    balance |> Currency.Balance.to_amount |> Currency.Amount.to_uint64
    |> Unsigned.UInt64.to_int64

  let find (module Conn : CONNECTION) ~(public_key_id : int)
      ~(balance : Currency.Balance.t) =
    Conn.find_opt
      (Caqti_request.find_opt
         Caqti_type.(tup2 int int64)
         Caqti_type.int
         {sql| SELECT id FROM balances
               WHERE public_key_id = $1
               AND balance = $2
         |sql})
      (public_key_id, balance_to_int64 balance)

  let load (module Conn : CONNECTION) ~(id : int) =
    Conn.find
      (Caqti_request.find Caqti_type.int
         Caqti_type.(tup2 int int64)
         {sql| SELECT public_key_id, balance FROM balances
               WHERE id = $1
         |sql})
      id

  let add (module Conn : CONNECTION) ~(public_key_id : int)
      ~(balance : Currency.Balance.t) =
    Conn.find
      (Caqti_request.find
         Caqti_type.(tup2 int int64)
         Caqti_type.int
         {sql| INSERT INTO balances (public_key_id, balance) VALUES (?, ?) RETURNING id |sql})
      (public_key_id, balance_to_int64 balance)

  let add_if_doesn't_exist (module Conn : CONNECTION) ~(public_key_id : int)
      ~(balance : Currency.Balance.t) =
    let open Deferred.Result.Let_syntax in
    match%bind find (module Conn) ~public_key_id ~balance with
    | Some balance_id ->
        return balance_id
    | None ->
        add (module Conn) ~public_key_id ~balance
end

module Block_and_internal_command = struct
  type t =
    { block_id: int
    ; internal_command_id: int
    ; sequence_no: int
    ; secondary_sequence_no: int
    ; receiver_balance_id: int }
  [@@deriving hlist]

  let typ =
    let open Caqti_type_spec in
    let spec = Caqti_type.[int; int; int; int; int] in
    let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
    let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
    Caqti_type.custom ~encode ~decode (to_rep spec)

  let add (module Conn : CONNECTION) ~block_id ~internal_command_id
      ~sequence_no ~secondary_sequence_no ~receiver_balance_id =
    Conn.exec
      (Caqti_request.exec typ
         {sql| INSERT INTO blocks_internal_commands
                (block_id, internal_command_id, sequence_no, secondary_sequence_no, receiver_balance)
                VALUES (?, ?, ?, ?, ?)
         |sql})
      { block_id
      ; internal_command_id
      ; sequence_no
      ; secondary_sequence_no
      ; receiver_balance_id }

  let find (module Conn : CONNECTION) ~block_id ~internal_command_id
      ~sequence_no ~secondary_sequence_no =
    Conn.find_opt
      (Caqti_request.find_opt
         Caqti_type.(tup4 int int int int)
         Caqti_type.string
         {sql| SELECT 'exists' FROM blocks_internal_commands
               WHERE block_id = $1
               AND internal_command_id = $2
               AND sequence_no = $3
               AND secondary_sequence_no = $4
         |sql})
      (block_id, internal_command_id, sequence_no, secondary_sequence_no)

  let add_if_doesn't_exist (module Conn : CONNECTION) ~block_id
      ~internal_command_id ~sequence_no ~secondary_sequence_no
      ~receiver_balance_id =
    let open Deferred.Result.Let_syntax in
    match%bind
      find
        (module Conn)
        ~block_id ~internal_command_id ~sequence_no ~secondary_sequence_no
    with
    | Some _ ->
        return ()
    | None ->
        add
          (module Conn)
          ~block_id ~internal_command_id ~sequence_no ~secondary_sequence_no
          ~receiver_balance_id
end

module Block_and_signed_command = struct
  type t =
    { block_id: int
    ; user_command_id: int
    ; sequence_no: int
    ; status: string
    ; failure_reason: string option
    ; fee_payer_account_creation_fee_paid: int64 option
    ; receiver_account_creation_fee_paid: int64 option
    ; created_token: int64 option
    ; fee_payer_balance_id: int
    ; source_balance_id: int option
    ; receiver_balance_id: int option }
  [@@deriving hlist]

  let typ =
    let open Caqti_type_spec in
    let spec =
      Caqti_type.
        [ int
        ; int
        ; int
        ; string
        ; option string
        ; option int64
        ; option int64
        ; option int64
        ; int
        ; option int
        ; option int ]
    in
    let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
    let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
    Caqti_type.custom ~encode ~decode (to_rep spec)

  let add (module Conn : CONNECTION) ~block_id ~user_command_id ~sequence_no
      ~status ~failure_reason ~fee_payer_account_creation_fee_paid
      ~receiver_account_creation_fee_paid ~created_token ~fee_payer_balance_id
      ~source_balance_id ~receiver_balance_id =
    let failure_reason =
      Option.map ~f:Transaction_status.Failure.to_string failure_reason
    in
    let amount_to_int64 x =
      Unsigned.UInt64.to_int64 (Currency.Amount.to_uint64 x)
    in
    let fee_payer_account_creation_fee_paid =
      Option.map ~f:amount_to_int64 fee_payer_account_creation_fee_paid
    in
    let receiver_account_creation_fee_paid =
      Option.map ~f:amount_to_int64 receiver_account_creation_fee_paid
    in
    let created_token =
      Option.map created_token ~f:(fun tid ->
          Unsigned.UInt64.to_int64 (Token_id.to_uint64 tid) )
    in
    Conn.exec
      (Caqti_request.exec typ
         {sql| INSERT INTO blocks_user_commands
                 (block_id,
                 user_command_id,
                 sequence_no,
                 status,
                 failure_reason,
                 fee_payer_account_creation_fee_paid,
                 receiver_account_creation_fee_paid,
                 created_token,
                 fee_payer_balance,
                 source_balance,
                 receiver_balance)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
         |sql})
      { block_id
      ; user_command_id
      ; sequence_no
      ; status
      ; failure_reason
      ; fee_payer_account_creation_fee_paid
      ; receiver_account_creation_fee_paid
      ; created_token
      ; fee_payer_balance_id
      ; source_balance_id
      ; receiver_balance_id }

  let add_with_status (module Conn : CONNECTION) ~block_id ~user_command_id
      ~sequence_no ~(status : Transaction_status.t) ~fee_payer_id ~source_id
      ~receiver_id =
    let open Deferred.Result.Let_syntax in
    let ( status_str
        , failure_reason
        , fee_payer_account_creation_fee_paid
        , receiver_account_creation_fee_paid
        , created_token
        , { Transaction_status.Balance_data.fee_payer_balance
          ; source_balance
          ; receiver_balance } ) =
      match status with
      | Applied
          ( { fee_payer_account_creation_fee_paid
            ; receiver_account_creation_fee_paid
            ; created_token }
          , balances ) ->
          ( "applied"
          , None
          , fee_payer_account_creation_fee_paid
          , receiver_account_creation_fee_paid
          , created_token
          , balances )
      | Failed (failure, balances) ->
          ("failed", Some failure, None, None, None, balances)
    in
    let add_optional_balance id balance =
      match balance with
      | None ->
          Deferred.Result.return None
      | Some balance ->
          let%map balance_id =
            Balance.add_if_doesn't_exist
              (module Conn)
              ~public_key_id:id ~balance
          in
          Some balance_id
    in
    (* Any transaction included in a block will have had its fee paid, so we can
     * assume the fee payer balance will be Some here *)
    let fee_payer_balance = Option.value_exn fee_payer_balance in
    let%bind fee_payer_balance_id =
      Balance.add_if_doesn't_exist
        (module Conn)
        ~public_key_id:fee_payer_id ~balance:fee_payer_balance
    in
    let%bind source_balance_id =
      add_optional_balance source_id source_balance
    in
    let%bind receiver_balance_id =
      add_optional_balance receiver_id receiver_balance
    in
    add
      (module Conn)
      ~block_id ~user_command_id ~sequence_no ~status:status_str
      ~failure_reason ~fee_payer_account_creation_fee_paid
      ~receiver_account_creation_fee_paid ~created_token ~fee_payer_balance_id
      ~source_balance_id ~receiver_balance_id

  let add_if_doesn't_exist (module Conn : CONNECTION) ~block_id
      ~user_command_id ~sequence_no ~(status : string) ~failure_reason
      ~fee_payer_account_creation_fee_paid ~receiver_account_creation_fee_paid
      ~created_token ~fee_payer_balance_id ~source_balance_id
      ~receiver_balance_id =
    let open Deferred.Result.Let_syntax in
    match%bind
      Conn.find_opt
        (Caqti_request.find_opt
           Caqti_type.(tup3 int int int)
           Caqti_type.string
           {sql| SELECT 'exists' FROM blocks_user_commands
                 WHERE block_id = $1
                 AND user_command_id = $2
                 AND sequence_no = $3
           |sql})
        (block_id, user_command_id, sequence_no)
    with
    | Some _ ->
        return ()
    | None ->
        add
          (module Conn)
          ~block_id ~user_command_id ~sequence_no ~status ~failure_reason
          ~fee_payer_account_creation_fee_paid
          ~receiver_account_creation_fee_paid ~created_token
          ~fee_payer_balance_id ~source_balance_id ~receiver_balance_id

  let load (module Conn : CONNECTION) ~block_id ~user_command_id =
    Conn.find
      (Caqti_request.find
         Caqti_type.(tup2 int int)
         typ
         {sql| SELECT block_id, user_command_id,
               sequence_no,
               status,failure_reason,
               fee_payer_account_creation_fee_paid,
               receiver_account_creation_fee_paid,
               created_token,
               fee_payer_balance,
               source_balance,
               receiver_balance
               FROM blocks_user_commands
               WHERE block_id = $1
               AND user_command_id = $2
           |sql})
      (block_id, user_command_id)
end

module Block = struct
  type t =
    { state_hash: string
    ; parent_id: int option
    ; parent_hash: string
    ; creator_id: int
    ; block_winner_id: int
    ; snarked_ledger_hash_id: int
    ; staking_epoch_data_id: int
    ; next_epoch_data_id: int
    ; ledger_hash: string
    ; height: int64
    ; global_slot: int64
    ; global_slot_since_genesis: int64
    ; timestamp: int64 }
  [@@deriving hlist]

  let typ =
    let open Caqti_type_spec in
    let spec =
      Caqti_type.
        [ string
        ; option int
        ; string
        ; int
        ; int
        ; int
        ; int
        ; int
        ; string
        ; int64
        ; int64
        ; int64
        ; int64 ]
    in
    let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
    let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
    Caqti_type.custom ~encode ~decode (to_rep spec)

  let find (module Conn : CONNECTION) ~(state_hash : State_hash.t) =
    Conn.find
      (Caqti_request.find Caqti_type.string Caqti_type.int
         "SELECT id FROM blocks WHERE state_hash = ?")
      (State_hash.to_string state_hash)

  let find_opt (module Conn : CONNECTION) ~(state_hash : State_hash.t) =
    Conn.find_opt
      (Caqti_request.find_opt Caqti_type.string Caqti_type.int
         "SELECT id FROM blocks WHERE state_hash = ?")
      (State_hash.to_string state_hash)

  let load (module Conn : CONNECTION) ~(id : int) =
    Conn.find
      (Caqti_request.find Caqti_type.int typ
         {sql| SELECT state_hash, parent_id, parent_hash, creator_id,
                      block_winner_id, snarked_ledger_hash_id, staking_epoch_data_id,
                      next_epoch_data_id, ledger_hash, height, global_slot,
                      global_slot_since_genesis, timestamp FROM blocks
               WHERE id = ?
         |sql})
      id

  let add_parts_if_doesn't_exist (module Conn : CONNECTION)
      ~constraint_constants ~protocol_state ~staged_ledger_diff ~hash =
    let open Deferred.Result.Let_syntax in
    match%bind find_opt (module Conn) ~state_hash:hash with
    | Some block_id ->
        return block_id
    | None ->
        let consensus_state = Protocol_state.consensus_state protocol_state in
        let%bind parent_id =
          find_opt
            (module Conn)
            ~state_hash:(Protocol_state.previous_state_hash protocol_state)
        in
        let%bind creator_id =
          Public_key.add_if_doesn't_exist
            (module Conn)
            (Consensus.Data.Consensus_state.block_creator consensus_state)
        in
        let%bind block_winner_id =
          Public_key.add_if_doesn't_exist
            (module Conn)
            (Consensus.Data.Consensus_state.block_stake_winner consensus_state)
        in
        let%bind snarked_ledger_hash_id =
          Snarked_ledger_hash.add_if_doesn't_exist
            (module Conn)
            ( Protocol_state.blockchain_state protocol_state
            |> Blockchain_state.snarked_ledger_hash )
        in
        let%bind staking_epoch_data_id =
          Epoch_data.add_if_doesn't_exist
            (module Conn)
            (Consensus.Data.Consensus_state.staking_epoch_data consensus_state)
        in
        let%bind next_epoch_data_id =
          Epoch_data.add_if_doesn't_exist
            (module Conn)
            (Consensus.Data.Consensus_state.next_epoch_data consensus_state)
        in
        let%bind block_id =
          Conn.find
            (Caqti_request.find typ Caqti_type.int
               {sql| INSERT INTO blocks (state_hash, parent_id, parent_hash,
                      creator_id, block_winner_id,
                      snarked_ledger_hash_id, staking_epoch_data_id,
                      next_epoch_data_id, ledger_hash, height, global_slot,
                      global_slot_since_genesis, timestamp)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id
               |sql})
            { state_hash= hash |> State_hash.to_string
            ; parent_id
            ; parent_hash=
                Protocol_state.previous_state_hash protocol_state
                |> State_hash.to_string
            ; creator_id
            ; block_winner_id
            ; snarked_ledger_hash_id
            ; staking_epoch_data_id
            ; next_epoch_data_id
            ; ledger_hash=
                Protocol_state.blockchain_state protocol_state
                |> Blockchain_state.staged_ledger_hash
                |> Staged_ledger_hash.ledger_hash |> Ledger_hash.to_string
            ; height=
                consensus_state
                |> Consensus.Data.Consensus_state.blockchain_length
                |> Unsigned.UInt32.to_int64
            ; global_slot=
                Consensus.Data.Consensus_state.curr_global_slot consensus_state
                |> Unsigned.UInt32.to_int64
            ; global_slot_since_genesis=
                consensus_state
                |> Consensus.Data.Consensus_state.global_slot_since_genesis
                |> Unsigned.UInt32.to_int64
            ; timestamp=
                Protocol_state.blockchain_state protocol_state
                |> Blockchain_state.timestamp |> Block_time.to_int64 }
        in
        let transactions =
          let coinbase_receiver =
            Consensus.Data.Consensus_state.coinbase_receiver consensus_state
          in
          let supercharge_coinbase =
            Consensus.Data.Consensus_state.supercharge_coinbase consensus_state
          in
          match
            Staged_ledger.Pre_diff_info.get_transactions ~constraint_constants
              ~coinbase_receiver ~supercharge_coinbase staged_ledger_diff
          with
          | Ok transactions ->
              transactions
          | Error e ->
              Error.raise (Staged_ledger.Pre_diff_info.Error.to_error e)
        in
        let%bind (_ : int) =
          deferred_result_list_fold transactions ~init:0 ~f:(fun sequence_no ->
            function
            | { Mina_base.With_status.status
              ; data= Mina_base.Transaction.Command command } ->
                let user_command =
                  {Mina_base.With_status.status; data= command}
                in
                let%bind id =
                  User_command.add_if_doesn't_exist
                    (module Conn)
                    user_command.data
                in
                let%bind {fee_payer_id; source_id; receiver_id} =
                  User_command.Signed_command
                  .add_balance_public_keys_if_don't_exist
                    (module Conn)
                    (User_command.as_signed_command user_command.data)
                in
                let%map () =
                  Block_and_signed_command.add_with_status
                    (module Conn)
                    ~block_id ~user_command_id:id ~sequence_no
                    ~status:user_command.status ~fee_payer_id ~source_id
                    ~receiver_id
                  >>| ignore
                in
                sequence_no + 1
            | {data= Fee_transfer fee_transfer_bundled; status} ->
                let balances =
                  Transaction_status.Fee_transfer_balance_data
                  .of_balance_data_exn
                    (Transaction_status.balance_data status)
                in
                let fee_transfers =
                  Mina_base.Fee_transfer.to_numbered_list fee_transfer_bundled
                in
                (* balances.receiver1_balance is for receiver of head of fee_transfers
                   balances.receiver2_balance, if it exists, is for receiver of
                     next element of fee_transfers
                *)
                let%bind fee_transfer_infos =
                  deferred_result_list_fold fee_transfers ~init:[]
                    ~f:(fun acc (secondary_sequence_no, fee_transfer) ->
                      let%map id =
                        Fee_transfer.add_if_doesn't_exist
                          (module Conn)
                          fee_transfer `Normal
                      in
                      (id, secondary_sequence_no, fee_transfer.receiver_pk)
                      :: acc )
                in
                let fee_transfer_infos_with_balances =
                  match fee_transfer_infos with
                  | [id] ->
                      [(id, balances.receiver1_balance)]
                  | [id2; id1] ->
                      (* the fold reverses the order of the infos from the fee transfers *)
                      [ (id1, balances.receiver1_balance)
                      ; (id2, Option.value_exn balances.receiver2_balance) ]
                  | _ ->
                      failwith
                        "Unexpected number of single fee transfers in a fee \
                         transfer transaction"
                in
                let%map () =
                  deferred_result_list_fold fee_transfer_infos_with_balances
                    ~init:()
                    ~f:(fun ()
                       ( (fee_transfer_id, secondary_sequence_no, receiver_pk)
                       , balance )
                       ->
                      let%bind receiver_id =
                        Public_key.add_if_doesn't_exist
                          (module Conn)
                          receiver_pk
                      in
                      let%bind receiver_balance_id =
                        Balance.add_if_doesn't_exist
                          (module Conn)
                          ~public_key_id:receiver_id ~balance
                      in
                      Block_and_internal_command.add
                        (module Conn)
                        ~block_id ~internal_command_id:fee_transfer_id
                        ~sequence_no ~secondary_sequence_no
                        ~receiver_balance_id
                      >>| ignore )
                in
                sequence_no + 1
            | {data= Coinbase coinbase; status} ->
                let balances =
                  Transaction_status.Coinbase_balance_data.of_balance_data_exn
                    (Transaction_status.balance_data status)
                in
                let%bind () =
                  match Mina_base.Coinbase.fee_transfer coinbase with
                  | None ->
                      return ()
                  | Some {receiver_pk; fee} ->
                      let fee_transfer =
                        Mina_base.Fee_transfer.Single.create ~receiver_pk ~fee
                          ~fee_token:Token_id.default
                      in
                      let%bind id =
                        Fee_transfer.add_if_doesn't_exist
                          (module Conn)
                          fee_transfer `Via_coinbase
                      in
                      let%bind fee_transfer_receiver_id =
                        Public_key.add_if_doesn't_exist
                          (module Conn)
                          receiver_pk
                      in
                      let%bind receiver_balance_id =
                        Balance.add_if_doesn't_exist
                          (module Conn)
                          ~public_key_id:fee_transfer_receiver_id
                          ~balance:
                            (Option.value_exn
                               balances.fee_transfer_receiver_balance)
                      in
                      Block_and_internal_command.add
                        (module Conn)
                        ~block_id ~internal_command_id:id ~sequence_no
                        ~secondary_sequence_no:0 ~receiver_balance_id
                      >>| ignore
                in
                let%bind id =
                  Coinbase.add_if_doesn't_exist (module Conn) coinbase
                in
                let%bind coinbase_receiver_id =
                  Public_key.add_if_doesn't_exist
                    (module Conn)
                    coinbase.receiver
                in
                let%bind receiver_balance_id =
                  Balance.add_if_doesn't_exist
                    (module Conn)
                    ~public_key_id:coinbase_receiver_id
                    ~balance:balances.coinbase_receiver_balance
                in
                let%map () =
                  Block_and_internal_command.add
                    (module Conn)
                    ~block_id ~internal_command_id:id ~sequence_no
                    ~secondary_sequence_no:0 ~receiver_balance_id
                  >>| ignore
                in
                sequence_no + 1 )
        in
        return block_id

  let add_if_doesn't_exist conn ~constraint_constants
      ({data= t; hash} : (External_transition.t, State_hash.t) With_hash.t) =
    add_parts_if_doesn't_exist conn ~constraint_constants
      ~protocol_state:(External_transition.protocol_state t)
      ~staged_ledger_diff:(External_transition.staged_ledger_diff t)
      ~hash

  let add_from_precomputed conn ~constraint_constants
      (t : External_transition.Precomputed_block.t) =
    add_parts_if_doesn't_exist conn ~constraint_constants
      ~protocol_state:t.protocol_state ~staged_ledger_diff:t.staged_ledger_diff
      ~hash:(Protocol_state.hash t.protocol_state)

  let add_from_extensional (module Conn : CONNECTION)
      (block : Extensional.Block.t) =
    let open Deferred.Result.Let_syntax in
    let%bind block_id =
      match%bind find_opt (module Conn) ~state_hash:block.state_hash with
      | Some block_id ->
          return block_id
      | None ->
          let%bind parent_id =
            find_opt (module Conn) ~state_hash:block.parent_hash
          in
          let%bind creator_id =
            Public_key.add_if_doesn't_exist (module Conn) block.creator
          in
          let%bind block_winner_id =
            Public_key.add_if_doesn't_exist (module Conn) block.block_winner
          in
          let%bind snarked_ledger_hash_id =
            Snarked_ledger_hash.add_if_doesn't_exist
              (module Conn)
              block.snarked_ledger_hash
          in
          let%bind staking_ledger_hash_id =
            Snarked_ledger_hash.add_if_doesn't_exist
              (module Conn)
              block.staking_epoch_ledger_hash
          in
          let%bind staking_epoch_data_id =
            Epoch_data.add_from_seed_and_ledger_hash_id
              (module Conn)
              ~seed:block.staking_epoch_seed
              ~ledger_hash_id:staking_ledger_hash_id
          in
          let%bind next_ledger_hash_id =
            Snarked_ledger_hash.add_if_doesn't_exist
              (module Conn)
              block.next_epoch_ledger_hash
          in
          let%bind next_epoch_data_id =
            Epoch_data.add_from_seed_and_ledger_hash_id
              (module Conn)
              ~seed:block.next_epoch_seed ~ledger_hash_id:next_ledger_hash_id
          in
          Conn.find
            (Caqti_request.find typ Caqti_type.int
               {sql| INSERT INTO blocks
                     (state_hash, parent_id, parent_hash,
                      creator_id, block_winner_id,
                      snarked_ledger_hash_id, staking_epoch_data_id,
                      next_epoch_data_id, ledger_hash, height, global_slot,
                      global_slot_since_genesis, timestamp)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id
               |sql})
            { state_hash= block.state_hash |> State_hash.to_string
            ; parent_id
            ; parent_hash= block.parent_hash |> State_hash.to_string
            ; creator_id
            ; block_winner_id
            ; snarked_ledger_hash_id
            ; staking_epoch_data_id
            ; next_epoch_data_id
            ; ledger_hash= block.ledger_hash |> Ledger_hash.to_string
            ; height= block.height |> Unsigned.UInt32.to_int64
            ; global_slot= block.global_slot |> Unsigned.UInt32.to_int64
            ; global_slot_since_genesis=
                block.global_slot_since_genesis |> Unsigned.UInt32.to_int64
            ; timestamp= block.timestamp |> Block_time.to_int64 }
    in
    (* add user commands *)
    let%bind user_cmds_with_ids =
      let%map user_cmd_ids_rev =
        deferred_result_list_fold block.user_cmds ~init:[]
          ~f:(fun acc user_cmd ->
            let%map cmd_id =
              User_command.add_extensional_if_doesn't_exist
                (module Conn)
                user_cmd
            in
            cmd_id :: acc )
      in
      List.zip_exn block.user_cmds (List.rev user_cmd_ids_rev)
    in
    let balance_id_of_pk_and_balance pk balance =
      let%bind public_key_id =
        Public_key.add_if_doesn't_exist (module Conn) pk
      in
      Balance.add_if_doesn't_exist (module Conn) ~public_key_id ~balance
    in
    let balance_id_of_pk_and_balance_opt pk balance_opt =
      Option.value_map balance_opt ~default:(Deferred.Result.return None)
        ~f:(fun balance ->
          let%map id = balance_id_of_pk_and_balance pk balance in
          Some id )
    in
    (* add user commands to join table *)
    let%bind () =
      deferred_result_list_fold user_cmds_with_ids ~init:()
        ~f:(fun () (user_command, user_command_id) ->
          let%bind source_balance_id =
            balance_id_of_pk_and_balance_opt user_command.source
              user_command.source_balance
          in
          let%bind fee_payer_balance_id =
            balance_id_of_pk_and_balance user_command.fee_payer
              user_command.fee_payer_balance
          in
          let%bind receiver_balance_id =
            balance_id_of_pk_and_balance_opt user_command.receiver
              user_command.receiver_balance
          in
          Block_and_signed_command.add_if_doesn't_exist
            (module Conn)
            ~block_id ~user_command_id ~sequence_no:user_command.sequence_no
            ~status:user_command.status
            ~failure_reason:user_command.failure_reason
            ~fee_payer_account_creation_fee_paid:
              user_command.fee_payer_account_creation_fee_paid
            ~receiver_account_creation_fee_paid:
              user_command.receiver_account_creation_fee_paid
            ~created_token:user_command.created_token ~fee_payer_balance_id
            ~source_balance_id ~receiver_balance_id )
    in
    (* add internal commands *)
    let%bind internal_cmds_ids_and_seq_nos =
      let%map internal_cmds_and_ids_rev =
        deferred_result_list_fold block.internal_cmds ~init:[]
          ~f:(fun acc internal_cmd ->
            let%map cmd_id =
              Internal_command.add_extensional_if_doesn't_exist
                (module Conn)
                internal_cmd
            in
            (internal_cmd, cmd_id) :: acc )
      in
      let sequence_nos =
        List.map block.internal_cmds ~f:(fun internal_cmd ->
            (internal_cmd.sequence_no, internal_cmd.secondary_sequence_no) )
      in
      List.zip_exn (List.rev internal_cmds_and_ids_rev) sequence_nos
    in
    (* add internal commands to join table *)
    let%bind () =
      deferred_result_list_fold internal_cmds_ids_and_seq_nos ~init:()
        ~f:(fun ()
           ( (internal_command, internal_command_id)
           , (sequence_no, secondary_sequence_no) )
           ->
          let%bind receiver_balance_id =
            balance_id_of_pk_and_balance internal_command.receiver
              internal_command.receiver_balance
          in
          Block_and_internal_command.add_if_doesn't_exist
            (module Conn)
            ~block_id ~internal_command_id ~sequence_no ~secondary_sequence_no
            ~receiver_balance_id )
    in
    return block_id

  let set_parent_id_if_null (module Conn : CONNECTION) ~parent_hash
      ~(parent_id : int) =
    Conn.exec
      (Caqti_request.exec
         Caqti_type.(tup2 int string)
         {sql| UPDATE blocks SET parent_id = ?
               WHERE parent_hash = ?
               AND parent_id IS NULL
         |sql})
      (parent_id, State_hash.to_base58_check parent_hash)

  let delete_if_older_than ?height ?num_blocks ?timestamp
      (module Conn : CONNECTION) =
    let open Deferred.Result.Let_syntax in
    let%bind height =
      match (height, num_blocks) with
      | Some height, _ ->
          return height
      | None, Some num_blocks -> (
          match%map
            Conn.find_opt
              (Caqti_request.find_opt Caqti_type.unit Caqti_type.int
                 "SELECT MAX(height) FROM blocks")
              ()
          with
          | Some max_block_height ->
              max_block_height - num_blocks
          | _ ->
              0 )
      | None, None ->
          return 0
    in
    let timestamp = Option.value ~default:Int64.zero timestamp in
    if height > 0 || Int64.(timestamp > 0L) then
      let%bind () =
        (* Delete user commands from old blocks. *)
        Conn.exec
          (Caqti_request.exec
             Caqti_type.(tup2 int int64)
             "DELETE FROM user_commands\n\
              WHERE id IN\n\
              (SELECT user_command_id FROM blocks_user_commands\n\
              INNER JOIN blocks ON blocks.id = block_id\n\
              WHERE (blocks.height < ? OR blocks.timestamp < ?))")
          (height, timestamp)
      in
      let%bind () =
        (* Delete old blocks. *)
        Conn.exec
          (Caqti_request.exec
             Caqti_type.(tup2 int int64)
             "DELETE FROM blocks WHERE blocks.height < ? OR blocks.timestamp \
              < ?")
          (height, timestamp)
      in
      let%bind () =
        (* Delete orphaned internal commands. *)
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             "DELETE FROM internal_commands\n\
              WHERE id NOT IN\n\
              (SELECT internal_commands.id FROM internal_commands\n\
              INNER JOIN blocks_internal_commands ON\n\
              internal_command_id = internal_commands.id)")
          ()
      in
      let%bind () =
        (* Delete orphaned snarked ledger hashes. *)
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             "DELETE FROM snarked_ledger_hashes\n\
              WHERE id NOT IN\n\
              (SELECT snarked_ledger_hash_id FROM blocks)")
          ()
      in
      let%bind () =
        (* Delete orphaned public keys. *)
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             "DELETE FROM public_keys\n\
              WHERE id NOT IN (SELECT fee_payer_id FROM user_commands)\n\
              AND id NOT IN (SELECT source_id FROM user_commands)\n\
              AND id NOT IN (SELECT receiver_id FROM user_commands)\n\
              AND id NOT IN (SELECT receiver_id FROM internal_commands)\n\
              AND id NOT IN (SELECT creator_id FROM blocks)")
          ()
      in
      return ()
    else return ()
end

let retry ~f ~logger ~error_str retries =
  let rec go retry_count =
    match%bind f () with
    | Error e ->
        if retry_count <= 0 then return (Error e)
        else (
          [%log warn] "Error in %s : $error. Retrying..." error_str
            ~metadata:[("error", `String (Caqti_error.show e))] ;
          let wait_for = Random.float_range 20. 2000. in
          let%bind () = after (Time.Span.of_ms wait_for) in
          go (retry_count - 1) )
    | Ok res ->
        return (Ok res)
  in
  go retries

let add_block_aux ?(retries = 3) ~logger ~add_block ~hash ~delete_older_than
    pool block =
  let add () =
    Caqti_async.Pool.use
      (fun (module Conn : CONNECTION) ->
        let%bind res =
          let open Deferred.Result.Let_syntax in
          let%bind () = Conn.start () in
          let%bind block_id = add_block (module Conn : CONNECTION) block in
          (* if an existing block has a parent hash that's for the block just added,
       set its parent id
      *)
          let%bind () =
            Block.set_parent_id_if_null
              (module Conn)
              ~parent_hash:(hash block) ~parent_id:block_id
          in
          match delete_older_than with
          | Some num_blocks ->
              Block.delete_if_older_than ~num_blocks (module Conn)
          | None ->
              return ()
        in
        match res with
        | Error e as err ->
            (*Error in the current transaction*)
            [%log warn]
              "Error when adding block data to the database, rolling it back: \
               $error"
              ~metadata:[("error", `String (Caqti_error.show e))] ;
            let%map _ = Conn.rollback () in
            err
        | Ok _ ->
            [%log info] "Committing block data for $state_hash"
              ~metadata:
                [("state_hash", Mina_base.State_hash.to_yojson (hash block))] ;
            Conn.commit () )
      pool
  in
  retry ~f:add ~logger ~error_str:"add_block_aux" retries

let add_block_aux_precomputed ~constraint_constants =
  add_block_aux ~add_block:(Block.add_from_precomputed ~constraint_constants)
    ~hash:(fun block ->
      block.External_transition.Precomputed_block.protocol_state
      |> Protocol_state.hash )

let add_block_aux_extensional =
  add_block_aux ~add_block:Block.add_from_extensional
    ~hash:(fun (block : Extensional.Block.t) -> block.state_hash)

let run pool reader ~constraint_constants ~logger ~delete_older_than =
  Strict_pipe.Reader.iter reader ~f:(function
    | Diff.Transition_frontier (Breadcrumb_added {block; _}) -> (
        let add_block = Block.add_if_doesn't_exist ~constraint_constants in
        let hash block = With_hash.hash block in
        match%map
          add_block_aux ~logger ~delete_older_than ~hash ~add_block pool block
        with
        | Error e ->
            [%log warn]
              ~metadata:
                [ ("block", With_hash.hash block |> State_hash.to_yojson)
                ; ("error", `String (Caqti_error.show e)) ]
              "Failed to archive block: $block, see $error"
        | Ok () ->
            () )
    | Transition_frontier _ ->
        Deferred.return ()
    | Transaction_pool {added; removed= _} ->
        let%map _ =
          Caqti_async.Pool.use
            (fun (module Conn : CONNECTION) ->
              let%map () =
                Deferred.List.iter added ~f:(fun command ->
                    match%map
                      User_command.add_if_doesn't_exist (module Conn) command
                    with
                    | Ok _ ->
                        ()
                    | Error e ->
                        [%log warn]
                          ~metadata:
                            [ ("error", `String (Caqti_error.show e))
                            ; ( "command"
                              , Mina_base.User_command.to_yojson command ) ]
                          "Failed to archive user command $command from \
                           transaction pool: $block, see $error" )
              in
              Ok () )
            pool
        in
        () )

let add_genesis_accounts ~logger
    ~(runtime_config_opt : Runtime_config.t option) pool =
  match runtime_config_opt with
  | None ->
      Deferred.unit
  | Some runtime_config -> (
      let accounts =
        match Option.map runtime_config.ledger ~f:(fun l -> l.base) with
        | Some (Accounts accounts) ->
            Genesis_ledger_helper.Accounts.to_full accounts
        | Some (Named name) -> (
          match Genesis_ledger.fetch_ledger name with
          | Some (module M) ->
              [%log info] "Found ledger with name $ledger_name"
                ~metadata:[("ledger_name", `String name)] ;
              Lazy.force M.accounts
          | None ->
              [%log error]
                "Could not find a built-in ledger named $ledger_name"
                ~metadata:[("ledger_name", `String name)] ;
              failwith "Could not add genesis accounts: Named ledger not found"
          )
        | _ ->
            failwith "No accounts found in runtime config file"
      in
      let add_accounts () =
        Caqti_async.Pool.use
          (fun (module Conn : CONNECTION) ->
            let open Deferred.Result.Let_syntax in
            let%bind () = Conn.start () in
            let rec go accounts =
              let open Deferred.Let_syntax in
              match accounts with
              | [] ->
                  Deferred.Result.return ()
              | (_, account) :: accounts' -> (
                  match%bind
                    Timing_info.add_if_doesn't_exist (module Conn) account
                  with
                  | Error e as err ->
                      [%log error]
                        ~metadata:
                          [ ("account", Account.to_yojson account)
                          ; ("error", `String (Caqti_error.show e)) ]
                        "Failed to add genesis account: $account, see $error" ;
                      let%map _ = Conn.rollback () in
                      err
                  | Ok _ ->
                      go accounts' )
            in
            let%bind () = go accounts in
            Conn.commit () )
          pool
      in
      match%map
        retry ~f:add_accounts ~logger ~error_str:"add_genesis_accounts" 3
      with
      | Error e ->
          [%log warn] "genesis accounts could not be added"
            ~metadata:[("error", `String (Caqti_error.show e))] ;
          failwith "Failed to add genesis accounts"
      | Ok () ->
          () )

let create_metrics_server ~logger ~metrics_server_port ~missing_blocks_width
    pool =
  match metrics_server_port with
  | None ->
      return ()
  | Some port ->
      let missing_blocks_width =
        Option.value ~default:Metrics.default_missing_blocks_width
          missing_blocks_width
      in
      let%bind metric_server =
        Mina_metrics.Archive.create_archive_server ~port ~logger ()
      in
      let interval =
        Float.of_int (Mina_compile_config.block_window_duration_ms * 2)
      in
      let rec go () =
        let%bind () =
          Metrics.update pool metric_server ~logger ~missing_blocks_width
        in
        let%bind () = after (Time.Span.of_ms interval) in
        go ()
      in
      go ()

let setup_server ~metrics_server_port ~constraint_constants ~logger
    ~postgres_address ~server_port ~delete_older_than ~runtime_config_opt
    ~missing_blocks_width =
  let where_to_listen =
    Async.Tcp.Where_to_listen.bind_to All_addresses (On_port server_port)
  in
  let reader, writer = Strict_pipe.create ~name:"archive" Synchronous in
  let precomputed_block_reader, precomputed_block_writer =
    Strict_pipe.create ~name:"precomputed_archive_block" Synchronous
  in
  let extensional_block_reader, extensional_block_writer =
    Strict_pipe.create ~name:"extensional_archive_block" Synchronous
  in
  let implementations =
    [ Async.Rpc.Rpc.implement Archive_rpc.t (fun () archive_diff ->
          Strict_pipe.Writer.write writer archive_diff )
    ; Async.Rpc.Rpc.implement Archive_rpc.precomputed_block
        (fun () precomputed_block ->
          Strict_pipe.Writer.write precomputed_block_writer precomputed_block
      )
    ; Async.Rpc.Rpc.implement Archive_rpc.extensional_block
        (fun () extensional_block ->
          Strict_pipe.Writer.write extensional_block_writer extensional_block
      ) ]
  in
  match Caqti_async.connect_pool ~max_size:30 postgres_address with
  | Error e ->
      [%log error]
        "Failed to create a Caqti pool for Postgresql, see error: $error"
        ~metadata:[("error", `String (Caqti_error.show e))] ;
      Deferred.unit
  | Ok pool ->
      let%bind () = add_genesis_accounts pool ~logger ~runtime_config_opt in
      run ~constraint_constants pool reader ~logger ~delete_older_than
      |> don't_wait_for ;
      Strict_pipe.Reader.iter precomputed_block_reader
        ~f:(fun precomputed_block ->
          match%map
            add_block_aux_precomputed ~logger ~constraint_constants
              ~delete_older_than pool precomputed_block
          with
          | Error e ->
              [%log warn]
                "Precomputed block $block could not be archived: $error"
                ~metadata:
                  [ ( "block"
                    , Protocol_state.hash precomputed_block.protocol_state
                      |> State_hash.to_yojson )
                  ; ("error", `String (Caqti_error.show e)) ]
          | Ok () ->
              () )
      |> don't_wait_for ;
      Strict_pipe.Reader.iter extensional_block_reader
        ~f:(fun extensional_block ->
          match%map
            add_block_aux_extensional ~logger ~delete_older_than pool
              extensional_block
          with
          | Error e ->
              [%log warn]
                "Extensional block $block could not be archived: $error"
                ~metadata:
                  [ ( "block"
                    , extensional_block.state_hash |> State_hash.to_yojson )
                  ; ("error", `String (Caqti_error.show e)) ]
          | Ok () ->
              () )
      |> don't_wait_for ;
      Deferred.ignore
      @@ Tcp.Server.create
           ~on_handler_error:
             (`Call
               (fun _net exn ->
                 [%log error]
                   "Exception while handling TCP server request: $error"
                   ~metadata:
                     [ ("error", `String (Core.Exn.to_string_mach exn))
                     ; ("context", `String "rpc_tcp_server") ] ))
           where_to_listen
           (fun address reader writer ->
             let address = Socket.Address.Inet.addr address in
             Async.Rpc.Connection.server_with_close reader writer
               ~implementations:
                 (Async.Rpc.Implementations.create_exn ~implementations
                    ~on_unknown_rpc:`Raise)
               ~connection_state:(fun _ -> ())
               ~on_handshake_error:
                 (`Call
                   (fun exn ->
                     [%log error]
                       "Exception while handling RPC server request from \
                        $address: $error"
                       ~metadata:
                         [ ("error", `String (Core.Exn.to_string_mach exn))
                         ; ("context", `String "rpc_server")
                         ; ( "address"
                           , `String (Unix.Inet_addr.to_string address) ) ] ;
                     Deferred.unit )) )
      |> don't_wait_for ;
      (*Update archive metrics*)
      create_metrics_server ~logger ~metrics_server_port ~missing_blocks_width
        pool
      |> don't_wait_for ;
      [%log info] "Archive process ready. Clients can now connect" ;
      Async.never ()

module For_test = struct
  let assert_parent_exist ~parent_id ~parent_hash conn =
    let open Deferred.Result.Let_syntax in
    match parent_id with
    | Some id ->
        let%map Block.{state_hash= actual; _} = Block.load conn ~id in
        [%test_result: string]
          ~expect:(parent_hash |> State_hash.to_base58_check)
          actual
    | None ->
        failwith "Failed to find parent block in database"
end
