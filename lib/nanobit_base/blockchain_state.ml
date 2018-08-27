open Core_kernel
open Coda_numbers
open Util
open Snark_params
open Tick
open Let_syntax
open Fold_lib

module Digest = Pedersen.Digest

let all_but_last_exn xs = fst (split_last_exn xs)

module Hash = State_hash

module Stable = struct
  module V1 = struct
    type ('ledger_builder_hash, 'ledger_hash, 'time, 'fee) t_ =
      { ledger_builder_hash: 'ledger_builder_hash
      ; ledger_hash: 'ledger_hash
      ; timestamp: 'time 
      ; fee_excess: 'fee}
    [@@deriving bin_io, sexp, fields, eq, compare, hash]

    type t = (Ledger_builder_hash.Stable.V1.t, Ledger_hash.Stable.V1.t, Block_time.Stable.V1.t, Currency.Fee.Signed.t) t_
    [@@deriving bin_io, sexp, eq, compare, hash]
  end
end

include Stable.V1

type var =
  ( Ledger_builder_hash.var
  , Ledger_hash.var
  , Block_time.Unpacked.var
  , Currency.Fee.Signed.var
  ) t_

type value = t [@@deriving bin_io, sexp, eq, compare, hash]

let create_value ~ledger_builder_hash ~ledger_hash ~timestamp ~fee_excess =
  { ledger_builder_hash; ledger_hash; timestamp; fee_excess }

let to_hlist { ledger_builder_hash; ledger_hash; timestamp; fee_excess } =
  H_list.([ ledger_builder_hash; ledger_hash; timestamp; fee_excess ])
let of_hlist : (unit, 'lbh -> 'lh -> 'ti -> 'fe -> unit) H_list.t -> ('lbh, 'lh, 'ti, 'fe) t_ =
  H_list.(fun [ ledger_builder_hash; ledger_hash; timestamp; fee_excess ] -> { ledger_builder_hash; ledger_hash; timestamp; fee_excess })

let data_spec =
  let open Data_spec in
  [ Ledger_builder_hash.typ
  ; Ledger_hash.typ
  ; Block_time.Unpacked.typ
  ; Currency.Fee.Signed.typ
  ]

let typ : (var, value) Typ.t =
  Typ.of_hlistable data_spec
    ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
    ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

let var_to_triples ({ ledger_builder_hash; ledger_hash; timestamp; fee_excess } : var) =
  let%map ledger_hash_triples = Ledger_hash.var_to_triples ledger_hash
  and ledger_builder_hash_triples = Ledger_builder_hash.var_to_triples ledger_builder_hash
  in
  ledger_builder_hash_triples
  @ ledger_hash_triples
  @ Block_time.Unpacked.var_to_triples timestamp
  @ Currency.Fee.Signed.to_triples fee_excess

let fold ({ ledger_builder_hash; ledger_hash; timestamp; fee_excess } : value) =
  Fold.(Ledger_builder_hash.fold ledger_builder_hash
  +> Ledger_hash.fold ledger_hash
  +> Block_time.fold timestamp
  +> Currency.Fee.Signed.fold fee_excess)

let length_in_triples =
  Ledger_builder_hash.length_in_triples
  + Ledger_hash.length_in_triples
  + Block_time.length_in_triples

let set_timestamp t timestamp = { t with timestamp }

let genesis_time =
  Time.of_date_ofday ~zone:Time.Zone.utc
    (Date.create_exn ~y:2018 ~m:Month.Feb ~d:2)
    Time.Ofday.start_of_day
  |> Block_time.of_time

let genesis =
  { ledger_builder_hash= Ledger_builder_hash.dummy
  ; ledger_hash= Ledger.merkle_root Genesis_ledger.ledger
  ; timestamp= genesis_time 
  ; fee_excess = Currency.Fee.Signed.zero }

module Message = struct
  open Util
  open Tick

  type nonrec t = t

  type nonrec var = var

  let hash t ~nonce =
    let d =
      Pedersen.digest_fold Hash_prefix.signature
        Fold.(fold t +> Fold.(group3 ~default:false (of_list nonce)))
    in
    List.take (Field.unpack d) Inner_curve.Scalar.length_in_bits
    |> Inner_curve.Scalar.of_bits

  let () = assert Insecure.signature_hash_function

  let hash_checked t ~nonce =
    let open Let_syntax in
    with_label __LOC__
      (let%bind trips = var_to_triples t in
       let%bind hash =
         Pedersen.Checked.digest_triples ~init:Hash_prefix.signature
           (trips @ Fold.(to_list (group3 ~default:Boolean.false_ (of_list nonce))))
       in
       let%map bs =
         Pedersen.Checked.Digest.choose_preimage hash
       in
       List.take (bs :> Boolean.var list) Inner_curve.Scalar.length_in_bits)
end

module Signature = Snarky.Signature.Schnorr (Tick) (Snark_params.Tick.Inner_curve) (Message)
