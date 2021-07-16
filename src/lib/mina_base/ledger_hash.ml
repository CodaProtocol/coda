open Core
open Import
open Snark_params
open Snarky_backendless
open Tick
open Let_syntax

let merge_var ~height h1 h2 =
  Random_oracle.Checked.hash ~init:(Hash_prefix.merkle_tree height) [| h1; h2 |]

module Merkle_tree =
  Snarky_backendless.Merkle_tree.Checked
    (Tick)
    (struct
      type value = Field.t

      type var = Field.Var.t

      let typ = Field.typ

      let merge ~height h1 h2 =
        Tick.make_checked (fun () -> merge_var ~height h1 h2)

      let assert_equal h1 h2 = Field.Checked.Assert.equal h1 h2

      let if_ = Field.Checked.if_
    end)
    (struct
      include Account

      let hash = Checked.digest
    end)

include Ledger_hash0

(* End boilerplate *)
let merge ~height (h1 : t) (h2 : t) =
  Random_oracle.hash
    ~init:(Hash_prefix.merkle_tree height)
    [| (h1 :> field); (h2 :> field) |]
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

let reraise_merkle_requests (With { request; respond }) =
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
         f x account))
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
      [%with_label "modify_account_send filter"]
        (let%bind account_already_there =
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
           [%with_label "account is either present or empty and writeable"]
             (Boolean.Assert.any
                [ account_already_there; not_there_but_writeable ])
         in
         return not_there_but_writeable))
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
      [%with_label "modify_account_recv filter"]
        (let%bind account_already_there =
           Account_id.Checked.equal (Account.identifier_of_var account) aid
         in
         let%bind account_not_there =
           Public_key.Compressed.Checked.equal account.public_key
             Public_key.Compressed.(var_of_t empty)
         in
         let%bind () =
           [%with_label "account is either present or empty"]
             (Boolean.Assert.any [ account_already_there; account_not_there ])
         in
         return account_not_there))
    ~f:(fun is_empty_and_writeable x -> f ~is_empty_and_writeable x)
