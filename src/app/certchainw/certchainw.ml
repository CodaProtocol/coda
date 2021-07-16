open Core_kernel
open Coda_base
open Core

open Async

open Import
open Snark_params
open Snarky
open Tick
open Signature_lib
open Let_syntax

open Currency
open Coda_numbers
open Sparse_ledger_lib

module Domain = struct
  type t = string [@@deriving sexp, eq, to_yojson]

  let nybble_bits = function
| 0x0 ->
        [false; false; false; false]
| 0x1 ->
        [false; false; false; true]
| 0x2 ->
        [false; false; true; false]
| 0x3 ->
        [false; false; true; true]
| 0x4 ->
        [false; true; false; false]
| 0x5 ->
        [false; true; false; true]
| 0x6 ->
        [false; true; true; false]
| 0x7 ->
        [false; true; true; true]
| 0x8 ->
        [true; false; false; false]
| 0x9 ->
        [true; false; false; true]
| 0xA ->
        [true; false; true; false]
| 0xB ->
        [true; false; true; true]
| 0xC ->
        [true; true; false; false]
| 0xD ->
        [true; true; false; true]
| 0xE ->
        [true; true; true; false]
| 0xF ->
        [true; true; true; true]
| _ ->
        failwith "nybble_bits: expected value from 0 to 0xF"

  let char_bits c =
    let open Core_kernel in
    let n = Char.to_int c in
    let hi = Int.(shift_right (bit_and n 0xF0) 4) in
    let lo = Int.bit_and n 0x0F in
    List.concat_map [hi; lo] ~f:nybble_bits

  let string_to_input s =
    Random_oracle.Input.
            { field_elements= [||]
            ; bitstrings=
    Stdlib.(Array.of_seq (Seq.map char_bits (String.to_seq s))) }


  let to_bits : t -> bool list =
    fun d ->
      let list_of_chars = List.init (String.length d) (String.get d) in
      let list_of_bits = List.concat_map list_of_chars char_bits in
      list_of_bits

  let gen () = string_of_int (1+ (Random.int 999)) ;;
end

module Certificate_authority : sig
  type t

  (*val register
    : t
    -> domain: Domain.t
    -> public_key:Signature_lib.Schnorr.Public_key.t
    -> self_signature:Signature.t
    -> Signature.t Or_error.t *)
end = struct
  type t = Private_key.t

  



  let register (skca : t) (domain : Domain.t) (pkd : Public_key.t) (signature_self : Signature_lib.Schnorr.Signature.t)  =
    let (b : bool) =
      Schnorr.verify signature_self 
        (Inner_curve.of_affine pkd)
        (Domain.string_to_input domain)
    in
    let msg =
      let (x, y) = pkd in
      { Random_oracle.Input.
        field_elements= [| x; y |]
      ; bitstrings=
          [| Domain.to_bits domain
          |]
      }
    in
    if b then Ok (Signature_lib.Schnorr.sign skca msg)
    else Or_error.error_string "invalid query"
end

module DomainAccount = struct
  module T = struct
    type t = {domain: Domain.t; pkd: Public_key.t; cert: Signature_lib.Schnorr.Signature.t}
    [@@deriving bin_io, eq, sexp, to_yojson, fields]
  end

  include T

  let key {domain; _} = domain



  let to_input (t : t) =
    let open Random_oracle.Input in
    let f mk acc field = mk (Core_kernel.Field.get field t) :: acc in
    let bits conv = f (Fn.compose bitstring conv) in
    let (x, y) = t.pkd in
     T.Fields.fold ~init:[]
      ~domain:(bits Domain.to_bits)
      ~public_key:(f [x; y]])
      ~cert:(f Signature_lib.Schnorr.Signature.to_input) 
    |> List.reduce_exn ~f:append



  let data_hash t  = 
    Random_oracle.hash ~init:[] (Random_oracle.pack_input (to_input t))}
    
 

  (* TODO: Should use poseidon (i.e., Random_oracle) *)
  (*let data_hash t = Md5.digest_string (Binable.to_string (module T) t) *)

  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map domain = String.quickcheck_generator
    and let kp = Signature_lib.Keypair.create () in
    {domain; kp.public_key; kp.private_key}
end



  (* TODO: Should use poseidon (i.e., Random_oracle) *)
module Hash = struct
  (* TODO: Use Random_oracle.Digest *)
    type t = Random_oracle.Digest.t [@@deriving sexp, compare, yojson]

    let equal h1 h2 = Int.equal (compare h1 h2) 0

    (*let to_yojson md5 = `String (Random_oracle.Digest. hash)*)
    let merge ~height x y = Random_oracle.hash  [|x;y|]


    (*let gen =
      Quickcheck.Generator.map String.quickcheck_generator
        ~f:Md5.digest_string*)
  end


module Merkle_tree_maintainer : sig
  type t 

  val get_path: Domain.t -> [`Left of Hash.t | `Right of Hash.t] list
end = struct
  include Sparse_ledger_lib.Sparse_ledger.Make (Hash) (Domain) (DomainAccount)
end

module Proof = struct
  type t = Tick.Proof.t
end

module Statement = struct
  type t =
    { merkle_root_old: Hash.t
    ; merkle_root_new: Hash.t
    }
end

module Witness = struct
  type t =
    { path        : [`Left of Hash.t | `Right of Hash.t] list
    ; account_old : DomainAccount.t
    ; account_new : DomainAccount.t
    }
end

module Prover : sig
  type t

  val prove : Witness.t -> Proof.t * Statement.t
end = struct

  (* *)
  let prove (w : Witness.t) =
    (* Use the function

       Tick.prove
    *)

end





module Merkle_tree =
  Snarky.Merkle_tree.Checked
    (Tick)
    (struct
      type value = Field.t

      type var = Field.Var.t

      let typ = Field.typ

      let merge ~height h1 h2 =
        Tick.make_checked (fun () ->
            Random_oracle.Checked.hash
              ~init:(Hash_prefix.merkle_tree height)
              [|h1; h2|] )

      let assert_equal h1 h2 = Field.Checked.Assert.equal h1 h2

      let if_ = Field.Checked.if_
    end)
    (struct
      include Domainaccount (** VERIFY *)

      let hash = Checked.digest
    end)

include Data_hash.Make_full_size (struct
  let description = "Ledger hash"

  let version_byte = Base58_check.Version_bytes.ledger_hash
end)

(* Data hash versioned boilerplate below *)

module Stable = struct
  module V1 = struct
    module T = struct
      type t = Field.t [@@deriving sexp, compare, hash, version {asserted}]
    end

    include T

    let to_latest = Core.Fn.id

    [%%define_from_scope
    to_yojson, of_yojson]

    include Comparable.Make (T)
    include Hashable.Make_binable (T)
  end
end

type _unused = unit constraint t = Stable.Latest.t

(* End boilerplate *)
let merge ~height (h1 : t) (h2 : t) =
  Random_oracle.hash
    ~init:(Hash_prefix.merkle_tree height)
    [|(h1 :> field); (h2 :> field)|]
  |> of_hash

let empty_hash = of_hash Outside_hash_image.t

let%bench "Ledger_hash.merge ~height:1 empty_hash empty_hash" =
  merge ~height:1 empty_hash empty_hash

let of_digest = Fn.compose Fn.id of_hash

type path = Random_oracle.Digest.t list

type _ Request.t +=
  | Get_path : Account.Index.t -> path Request.t
  | Get_element : Account.Index.t -> (Account.t * path) Request.t
  | Set : Account.Index.t * Account.t -> unit Request.t
  | Find_index : Account_id.t -> Account.Index.t Request.t

let reraise_merkle_requests (With {request; respond}) =
  match request with
  | Merkle_tree.Get_path addr ->
      respond (Delegate (Get_path addr))
  | Merkle_tree.Set (addr, account) ->
      respond (Delegate (Set (addr, account)))
  | Merkle_tree.Get_element addr ->
      respond (Delegate (Get_element addr))
  | _ ->
      unhandled

let get ~depth t addr =
  handle
    (Merkle_tree.get_req ~depth (var_to_hash_packed t) addr)
    reraise_merkle_requests

(*
   [modify_account t aid ~filter ~f] implements the following spec:
   - finds an account [account] in [t] for [aid] at path [addr] where [filter
     account] holds.
     note that the account is not guaranteed to have identifier [aid]; it might
     be a new account created to satisfy this request.
   - returns a root [t'] of a tree of depth [depth] which is [t] but with the
     account [f account] at path [addr].
*)
let%snarkydef modify_account ~depth t aid
    ~(filter : Account.var -> ('a, _) Checked.t) ~f =
  let%bind addr =
    request_witness
      (Account.Index.Unpacked.typ ~ledger_depth:depth)
      As_prover.(map (read Account_id.typ aid) ~f:(fun s -> Find_index s))
  in
  handle
    (Merkle_tree.modify_req ~depth (var_to_hash_packed t) addr
       ~f:(fun account ->
         let%bind x = filter account in
         f x account ))
    reraise_merkle_requests
  >>| var_of_hash_packed

(*
   [modify_account_send t aid ~f] implements the following spec:
   - finds an account [account] in [t] at path [addr] whose account id is [aid]
     OR it is a fee transfer and is an empty account
   - returns a root [t'] of a tree of depth [depth] which is [t] but with the
     account [f account] at path [addr].
*)
let%snarkydef modify_account_send ~depth t aid ~is_writeable ~f =
  modify_account ~depth t aid
    ~filter:(fun account ->
      let%bind account_already_there =
        Account_id.Checked.equal (Account.identifier_of_var account) aid
      in
      let%bind account_not_there =
        Public_key.Compressed.Checked.equal account.public_key
          Public_key.Compressed.(var_of_t empty)
      in
      let%bind not_there_but_writeable =
        Boolean.(account_not_there && is_writeable)
      in
      let%bind () =
        Boolean.Assert.any [account_already_there; not_there_but_writeable]
      in
      return not_there_but_writeable )
    ~f:(fun is_empty_and_writeable x -> f ~is_empty_and_writeable x)

(*
   [modify_account_recv t aid ~f] implements the following spec:
   - finds an account [account] in [t] at path [addr] whose account id is [aid]
     OR which is an empty account
   - returns a root [t'] of a tree of depth [depth] which is [t] but with the
     account [f account] at path [addr].
*)
let%snarkydef modify_account_recv ~depth t aid ~f =
  modify_account ~depth t aid
    ~filter:(fun account ->
      let%bind account_already_there =
        Account_id.Checked.equal (Account.identifier_of_var account) aid
      in
      let%bind account_not_there =
        Public_key.Compressed.Checked.equal account.public_key
          Public_key.Compressed.(var_of_t empty)
      in
      let%bind () =
        Boolean.Assert.any [account_already_there; account_not_there]
      in
      return account_not_there )
    ~f:(fun is_empty_and_writeable x -> f ~is_empty_and_writeable x)