(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 * Schema Block.t : Blocks contain an array of Transactions that occurred at a particular BlockIdentifier.
 *)

type t =
  { block_identifier: Block_identifier.t
  ; parent_block_identifier: Block_identifier.t
  ; timestamp: Timestamp.t
  ; transactions: Transaction.t list
  ; metadata: Yojson.Safe.t option [@default None] }
[@@deriving yojson {strict= false}, show]

(** Blocks contain an array of Transactions that occurred at a particular BlockIdentifier. *)
let create (block_identifier : Block_identifier.t)
    (parent_block_identifier : Block_identifier.t) (timestamp : Timestamp.t)
    (transactions : Transaction.t list) : t =
  { block_identifier
  ; parent_block_identifier
  ; timestamp
  ; transactions
  ; metadata= None }
