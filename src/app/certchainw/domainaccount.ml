(* account.ml *)

open Coda_base
open Core


open Core_kernel
open Snarky


open Snark_params
open Tick



open Currency
open Coda_numbers
open Fold_lib
open Import

module Index = struct

  type t = int [@@deriving to_yojson, sexp]

  let to_latest = Fn.id




  let to_int = Int.to_int

  let gen ~ledger_depth = Int.gen_incl 0 ((1 lsl ledger_depth) - 1)

  module Table = Int.Table

  module Vector = struct
    include Int

    let empty = zero

    let get t i = (t lsr i) land 1 = 1

    let set v i b = if b then v lor (one lsl i) else v land lnot (one lsl i)
  end

  let to_bits ~ledger_depth t = List.init ledger_depth ~f:(Vector.get t)

  let of_bits =
    List.foldi ~init:Vector.empty ~f:(fun i t b -> Vector.set t i b)

  let fold_bits ~ledger_depth t =
    { Fold.fold=
        (fun ~init ~f ->
          let rec go acc i =
            if i = ledger_depth then acc
            else go (f acc (Vector.get t i)) (i + 1)
          in
          go init 0 ) }

  let fold ~ledger_depth t =
    Fold.group3 ~default:false (fold_bits ~ledger_depth t)
end

(*module Nonce = Account_nonce*)

module Poly = struct
  module Stable = struct
    module V1 = struct
      type ( 'domain
           , 'pkd
           , 'cert
           , 'receipt_chain_hash
           , 'state_hash)
           t =
        { domain: 'domain
        ; public_key: 'pkd
        ; signature: 'cert
        ; receipt_chain_hash: 'receipt_chain_hash
        ; voting_for: 'state_hash }
      [@@deriving sexp, eq, compare, hash, yojson]
    end
  end

  type ( 'domain
        , 'pkd
        , 'cert
        , 'receipt_chain_hash
        , 'state_hash )
       t =
    { domain: 'domain
    ; public_key: 'pkd
    ; signature: 'cert
    ; receipt_chain_hash: 'receipt_chain_hash
    ; voting_for: 'state_hash }
  [@@deriving sexp, eq, compare, hash, yojson, fields]



  let of_hlist
      ([ domain
       ; public_key
       ; signature
       ; receipt_chain_hash
       ; voting_for ] :
        (unit, _) H_list.t) =
    { domain
    ; public_key
    ; signature
    ; receipt_chain_hash
    ; voting_for }

  let to_hlist
      { domain
      ; public_key
      ; signature
      ; receipt_chain_hash
      ; voting_for } =
    H_list.
      [ domain
      ; public_key
      ; signature
      ; receipt_chain_hash
      ; voting_for ]

end

module Key = struct

      type t = string (*Certchainw.Domain.t*)
      [@@deriving sexp, eq, hash, compare, yojson]

      let to_latest = Fn.id

end

module Identifier = Domainaccount_id

type key = Key.t [@@deriving sexp, eq, hash, compare, yojson]




    type t =
      ( Domain.t
      , Public_key.Compressed.Stable.V1.t
      , Signature_lib.Schnorr.Signature.t
      , Receipt.Chain_hash.Stable.V1.t
      , State_hash.Stable.V1.t )
      Poly.Stable.V1.t
    [@@deriving sexp, eq, hash, compare, yojson]

    let to_latest = Fn.id

    let domain (t : t) : key = t.domain



 




let identifier ({domain;  _} : t) =
  Domainaccount_id.create domain

type value =
  ( Domain.t
  , Public_key.Compressed.t
  , Signature.t
  , Receipt.Chain_hash.t
  , State_hash.t )
  Poly.t
[@@deriving sexp]

(*let key_gen = Public_key.Compressed.gen*)

let initialize account_id : t =
  let domain =  account_id in
  { domain
  ; public_key = Public_key.Compressed.empty
  ; signature = Signature_lib.Schnorr.Signature.t (* ATTN: change it to dummy signature *))
  ; receipt_chain_hash= Receipt.Chain_hash.empty
  ; voting_for= State_hash.dummy}
  

let to_input (t : t) =
  let open Random_oracle.Input in
  let f mk acc field = mk (Core_kernel.Field.get field t) :: acc in
  let bits conv = f (Fn.compose bitstring conv) in
  Poly.Fields.fold ~init:[]
    ~domain:(bits Certchainw.Domain.to_bits)
    ~public_key:(f Public_key.Compressed.to_input)
    ~signature:(bits Signature_lib.Schnorr.Signature.to_bits) (*ATTN: implement to_bits for Signatue*)
    ~receipt_chain_hash:(f Receipt.Chain_hash.to_input)
    ~voting_for:(f State_hash.to_input) 
  |> List.reduce_exn ~f:append

let crypto_hash_prefix = Random_oracle.salt (domainaccount :> string)

let crypto_hash t =
  Random_oracle.hash ~init:crypto_hash_prefix
    (Random_oracle.pack_input (to_input t))

[%%ifdef
consensus_mechanism]

type var =
  ( Certchainw.Domain.var (** ATTN todo: implement var in domain *))
  , Public_key.Compressed.var
  , Signature_lib.Schnorr.Signature.var
  , Receipt.Chain_hash.var
  , State_hash.var)
  Poly.t

let identifier_of_var ({domain;  _} : var) =
  Domainaccount_id.Checked.create domain

let typ : (var, value) Typ.t =
  let spec =
    Data_spec.
      [ Certchainw.Domain.typ (* ATTN: todo implement typ for domain *))
      ; Public_key.Compressed.typ
      ; Signature_lib.Schnorr.Signature.typ
      ; Receipt.Chain_hash.typ
      ; Public_key.Compressed.typ
      ; State_hash.typ]
  in
  Typ.of_hlistable spec ~var_to_hlist:Poly.to_hlist ~var_of_hlist:Poly.of_hlist
    ~value_to_hlist:Poly.to_hlist ~value_of_hlist:Poly.of_hlist

let var_of_t
    ({ domain
     ; public_key
     ; signature
     ; receipt_chain_hash
     ; voting_for} :
      value) =
  { Poly.domain = Certchainw.Domain.var_of_t domain
  ; public_key= Public_key.Compressed.var_of_t public_key
  ; signature = Signature_lib.Schnorr.Signature.var_of_t signature (* Attn: todo implemnt var_of_t *)
  ; receipt_chain_hash= Receipt.Chain_hash.var_of_t receipt_chain_hash
  ; voting_for= State_hash.var_of_t voting_for  }

module Checked = struct
  let to_input (t : var) =
    let ( ! ) f x = Run.run_checked (f x) in
    let f mk acc field = mk (Core_kernel.Field.get field t) :: acc in
    let open Random_oracle.Input in
    let bits conv =
      f (fun x ->
          bitstring (Bitstring_lib.Bitstring.Lsb_first.to_list (conv x)) )
    in
    make_checked (fun () ->
        List.reduce_exn ~f:append
          (Poly.Fields.fold ~init:[]
             ~domain: (bits Certchainw.Domain.var_to_bits) (** ATTN todo: implement *))
             ~public_key:(f Public_key.Compressed.Checked.to_input)
             ~signature:(f Signature_lib.Schnorr.Signature.var_to_input) (** ATTN todo: implement *))
             ~receipt_chain_hash:(f Receipt.Chain_hash.var_to_input)
             ~voting_for:(f State_hash.var_to_input) )

  let digest t =
    make_checked (fun () ->
        Random_oracle.Checked.(
          hash ~init:crypto_hash_prefix
            (pack_input (Run.run_checked (to_input t)))) )
end

[%%endif]

let digest = crypto_hash

let empty =
  { Poly.domain = Certchainw.Domain.empty (*ATTN: todo implement * *))
  ; public_key= Public_key.Compressed.empty
  ; signature = Signature_lib.Schnorr.Signature.dummy (*ATTN: todo implement * *))
  ; receipt_chain_hash= Receipt.Chain_hash.empty
  ; voting_for= State_hash.dummy }

let empty_digest = digest empty

let create domainaccount_id (pkd, signature) =
  let domain = domainaccount_id in
  let public_key = pkd in
  { Poly.domain
  ; public_key
  ; signature
  ; receipt_chain_hash= Receipt.Chain_hash.empty
  ; voting_for= State_hash.dummy  }



let gen =
  let open Quickcheck.Let_syntax in
  let%bind domain = Certchainw.Domain.gen in (** ATTN: should this be a random string? todo *)
  let%bind public_key = Public_key.Compressed.gen in
  let%bind signature = Signature_lib.Schnorr.Signature.dummy in
  create (Domainaccount_id.create domain) (public_key,signature)

