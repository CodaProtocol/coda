(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 * Schema Operation_status.t : OperationStatus is utilized to indicate which Operation status are considered successful.
 *)

type t =
  { (* The status is the network-specific status of the operation. *)
    status: string
  ; (* An Operation is considered successful if the Operation.Amount should affect the Operation.Account. Some blockchains (like Bitcoin) only include successful operations in blocks but other blockchains (like Ethereum) include unsuccessful operations that incur a fee.  To reconcile the computed balance from the stream of Operations, it is critical to understand which Operation.Status indicate an Operation is successful and should affect an Account. *)
    successful: bool }
[@@deriving yojson {strict= false}, show]

(** OperationStatus is utilized to indicate which Operation status are considered successful. *)
let create (status : string) (successful : bool) : t = {status; successful}
