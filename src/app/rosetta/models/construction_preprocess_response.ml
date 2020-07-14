(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 * Schema Construction_preprocess_response.t : ConstructionPreprocessResponse contains the request that will be sent directly to `/construction/metadata`. If it is not necessary to make a request to `/construction/metadata`, options should be null.
 *)

type t =
  { (* The options that will be sent directly to `/construction/metadata` by the caller. *)
    options: Yojson.Safe.t option [@default None] }
[@@deriving yojson {strict= false}, show]

(** ConstructionPreprocessResponse contains the request that will be sent directly to `/construction/metadata`. If it is not necessary to make a request to `/construction/metadata`, options should be null. *)
let create () : t = {options= None}
