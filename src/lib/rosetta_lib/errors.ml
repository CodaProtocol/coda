open Core_kernel
open Async_kernel

module Partial_reason = struct
  type t =
    | Length_mismatch
    | Fee_payer_and_source_mismatch
    | Amount_not_some
    | Account_not_some
    | Invalid_metadata
    | Incorrect_token_id
    | Amount_inc_dec_mismatch
    | Status_not_pending
    | Can't_find_kind of string
  [@@deriving yojson, sexp, show, equal]
end

module Variant = struct
  (* DO NOT change the order of this variant, the generated error code relies
   * on it and we want that to remain stable *)
  type t =
    [ `Sql of string
    | `Json_parse of string option
    | `Graphql_coda_query of string
    | `Network_doesn't_exist of string * string
    | `Chain_info_missing
    | `Account_not_found of string
    | `Invariant_violation
    | `Transaction_not_found of string
    | `Block_missing
    | `Malformed_public_key
    | `Operations_not_valid of Partial_reason.t list
    | `Unsupported_operation_for_construction
    | `Signature_missing
    | `Public_key_format_not_valid
    | `No_options_provided
    | `Exception of string
    | `Signature_invalid
    | `Memo_invalid
    | `Graphql_uri_not_set ]
  [@@deriving yojson, show, equal, to_enum, to_representatives]
end

module T : sig
  type t [@@deriving yojson, show, equal]

  val create : ?context:string -> Variant.t -> t

  val erase : t -> Rosetta_models.Error.t

  val all_errors : Rosetta_models.Error.t list lazy_t

  module Lift : sig
    val parse :
      ?context:string -> ('a, string) Result.t -> ('a, t) Deferred.Result.t

    val sql :
         ?context:string
      -> ('a, [< Caqti_error.t]) Deferred.Result.t
      -> ('a, t) Deferred.Result.t

    val wrap :
      ('a, t) Deferred.Result.t -> ('a, [> `App of t]) Deferred.Result.t
  end
end = struct
  type t = {extra_context: string option; kind: Variant.t}
  [@@deriving yojson, show, equal]

  let code = Fn.compose (fun x -> x + 1) Variant.to_enum

  let message = function
    | `Sql _ ->
        "SQL failure"
    | `Json_parse _ ->
        "JSON parse error"
    | `Graphql_coda_query _ ->
        "GraphQL query failed"
    | `Network_doesn't_exist _ ->
        "Network doesn't exist"
    | `Chain_info_missing ->
        "Chain info missing"
    | `Account_not_found _ ->
        "Account not found"
    | `Invariant_violation ->
        "Internal invariant violation (you found a bug)"
    | `Transaction_not_found _ ->
        "Transaction not found"
    | `Block_missing ->
        "Block not found"
    | `Malformed_public_key ->
        "Malformed public key"
    | `Operations_not_valid _ ->
        "Cannot convert operations to valid transaction"
    | `Public_key_format_not_valid ->
        "Invalid public key format"
    | `Unsupported_operation_for_construction ->
        "Unsupported operation for construction"
    | `Signature_missing ->
        "Signature missing"
    | `No_options_provided ->
        "No options provided"
    | `Exception _ ->
        "Exception"
    | `Signature_invalid ->
        "Invalid signature"
    | `Memo_invalid ->
        "Invalid memo"
    | `Graphql_uri_not_set ->
        "No GraphQL URI set"

  let context = function
    | `Sql msg ->
        Some msg
    | `Json_parse optional_msg ->
        optional_msg
    | `Graphql_coda_query msg ->
        Some msg
    | `Network_doesn't_exist (req, conn) ->
        Some
          (sprintf
             !"You are requesting the status for the network %s, but you are \
               connected to the network %s\n"
             req conn)
    | `Chain_info_missing ->
        Some
          "Could not get chain information. This probably means you are \
           bootstrapping -- bootstrapping is the process of synchronizing \
           with peers that are way ahead of you on the chain. Try again in a \
           few seconds."
    | `Account_not_found addr ->
        Some
          (sprintf
             !"You attempted to lookup %s, but we couldn't find it in the \
               ledger."
             addr)
    | `Invariant_violation ->
        None
    | `Transaction_not_found hash ->
        Some
          (sprintf
             "You attempted to lookup %s, but it is missing from the mempool. \
              This may be due to its inclusion in a block -- try looking for \
              this transaction in a recent block. It also could be due to the \
              transaction being evicted from the mempool."
             hash)
    | `Block_missing ->
        Some
          "We couldn't find the block you specified in the archive node. Ask \
           a friend for the missing data."
    | `Malformed_public_key ->
        None
    | `Operations_not_valid reasons ->
        Some
          (sprintf
             !"Cannot recover transaction for the following reasons: %{sexp: \
               Partial_reason.t list}"
             reasons)
    | `Public_key_format_not_valid ->
        None
    | `Unsupported_operation_for_construction ->
        None
    | `Signature_missing ->
        None
    | `No_options_provided ->
        None
    | `Exception s ->
        Some (sprintf "Exception when processing request: %s" s)
    | `Signature_invalid ->
        None
    | `Memo_invalid ->
        None
    | `Graphql_uri_not_set ->
        None

  let retriable = function
    | `Sql _ ->
        false
    | `Json_parse _ ->
        false
    | `Graphql_coda_query _ ->
        false
    | `Network_doesn't_exist _ ->
        false
    | `Chain_info_missing ->
        true
    | `Account_not_found _ ->
        true
    | `Invariant_violation ->
        false
    | `Transaction_not_found _ ->
        true
    | `Block_missing ->
        true
    | `Malformed_public_key ->
        false
    | `Operations_not_valid _ ->
        false
    | `Public_key_format_not_valid ->
        false
    | `Unsupported_operation_for_construction ->
        false
    | `Signature_missing ->
        false
    | `No_options_provided ->
        false
    | `Exception _ ->
        false
    | `Signature_invalid ->
        false
    | `Memo_invalid ->
        false
    | `Graphql_uri_not_set ->
        false

  (* Unlike message above, description can be updated whenever we see fit *)
  let description = function
    | `Sql _ ->
        "We encountered a SQL failure."
    | `Json_parse _ ->
        "We encountered an error while parsing JSON."
    | `Graphql_coda_query _ ->
        "The GraphQL query failed."
    | `Network_doesn't_exist _ ->
        "The network doesn't exist."
    | `Chain_info_missing ->
        "Some chain info is missing."
    | `Account_not_found _ ->
        "That account could not be found."
    | `Invariant_violation ->
        "One of our internal invariants was violated. (That means you found a \
         bug!)"
    | `Transaction_not_found _ ->
        "That transaction could not be found."
    | `Block_missing ->
        "We couldn't find the block you specified in the archive node. Ask a \
         friend for the missing data."
    | `Malformed_public_key ->
        "The public key you provided was malformed."
    | `Operations_not_valid _ ->
        "We could not convert those operations to a valid transaction."
    | `Public_key_format_not_valid ->
        "The public key you provided had an invalid format."
    | `Unsupported_operation_for_construction ->
        "An operation you provided isn't supported for construction."
    | `Signature_missing ->
        "Your request is missing a signature."
    | `Signature_invalid ->
        "Your request has an invalid signature."
    | `Memo_invalid ->
        "Your request has an invalid memo."
    | `No_options_provided ->
        "Your request is missing options."
    | `Graphql_uri_not_set ->
        "This Rosetta instance is running without a GraphQL URI set but this \
         request requires one."
    | `Exception _ ->
        "We encountered an internal exception while processing your request. \
         (That means you found a bug!)"

  let create ?context kind = {extra_context= context; kind}

  let erase (t : t) =
    { Rosetta_models.Error.code= Int32.of_int_exn (code t.kind)
    ; message= message t.kind
    ; retriable= retriable t.kind
    ; details=
        ( match (context t.kind, t.extra_context) with
        | None, None ->
            Some (`Assoc [("body", Variant.to_yojson t.kind)])
        | None, Some context | Some context, None ->
            Some
              (`Assoc
                [("body", Variant.to_yojson t.kind); ("error", `String context)])
        | Some context1, Some context2 ->
            Some
              (`Assoc
                [ ("body", Variant.to_yojson t.kind)
                ; ("error", `String context1)
                ; ("extra", `String context2) ]) )
    ; description= Some (description t.kind) }

  (* The most recent rosetta-cli denies errors that have details in them. When
   * future versions of the spec allow for more detailed descriptions we can
   * remove this filtering. *)
  let all_errors =
    (* This is n^2, but |input| is small enough that the performance doesn't
     * matter here. Plus this is likely cheaper than sorting first due to the
     * small size *)
    let rec uniq ~eq = function
      | [] ->
          []
      | x :: xs ->
          x :: (xs |> List.filter ~f:(fun x' -> not (eq x x')) |> uniq ~eq)
    in
    Variant.to_representatives
    |> Lazy.map ~f:(fun vs -> List.map vs ~f:(Fn.compose erase create))
    |> Lazy.map ~f:(fun es ->
           List.map es ~f:(fun e -> {e with Rosetta_models.Error.details= None})
           |> uniq ~eq:(fun {Rosetta_models.Error.code; _} {code= code2; _} ->
                  Int32.equal code code2 ) )

  module Lift = struct
    let parse ?context res =
      Deferred.return
        (Result.map_error
           ~f:(fun s -> create ?context (`Json_parse (Some s)))
           res)

    let sql ?context res =
      Deferred.Result.map_error
        ~f:(fun e -> create ?context (`Sql (Caqti_error.show e)))
        res

    let wrap t = Deferred.Result.map_error ~f:(fun e -> `App e) t
  end
end

include T
