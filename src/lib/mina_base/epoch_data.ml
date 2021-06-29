open Core_kernel
open Mina_numbers

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ( 'epoch_ledger
           , 'epoch_seed
           , 'start_checkpoint
           , 'lock_checkpoint
           , 'length )
           t =
        { ledger: 'epoch_ledger
        ; seed: 'epoch_seed
        ; start_checkpoint: 'start_checkpoint
              (* The lock checkpoint is the hash of the latest state in the seed update range, not including
                 the current state. *)
        ; lock_checkpoint: 'lock_checkpoint
        ; epoch_length: 'length }
      [@@deriving hlist, sexp, equal, compare, hash, yojson, fields]
    end
  end]
end

type var =
  ( Epoch_ledger.var
  , Epoch_seed.var
  , State_hash.var
  , State_hash.var
  , Length.Checked.t )
  Poly.t

let if_ cond ~(then_ : var) ~(else_ : var) =
  let open Snark_params.Tick.Checked.Let_syntax in
  let%map ledger =
    Epoch_ledger.if_ cond ~then_:then_.ledger ~else_:else_.ledger
  and seed = Epoch_seed.if_ cond ~then_:then_.seed ~else_:else_.seed
  and start_checkpoint =
    State_hash.if_ cond ~then_:then_.start_checkpoint
      ~else_:else_.start_checkpoint
  and lock_checkpoint =
    State_hash.if_ cond ~then_:then_.lock_checkpoint
      ~else_:else_.lock_checkpoint
  and epoch_length =
    Length.Checked.if_ cond ~then_:then_.epoch_length ~else_:else_.epoch_length
  in
  {Poly.ledger; seed; start_checkpoint; lock_checkpoint; epoch_length}

module Value = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Epoch_ledger.Value.Stable.V1.t
        , Epoch_seed.Stable.V1.t
        , State_hash.Stable.V1.t
        , State_hash.Stable.V1.t
        , Length.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, compare, equal, hash, yojson]

      let to_latest = Fn.id
    end
  end]
end
