open Core_kernel
open Signature_lib
open Graphql_async
open Schema

(* HACK: To prevent the compiler from identifying the `ctx value in a graphql
   typ to be a weak variable, the type needs to be created as unit -> ('ctx,
   'output) value *)
let unsigned_scalar_scalar ~to_string typ_name =
  scalar typ_name
    ~doc:
      (Core.sprintf
         !"String representing a %s number in base 10"
         (String.lowercase typ_name))
    ~coerce:(fun num -> `String (to_string num))

let public_key () =
  scalar "PublicKey" ~doc:"Base58Check-encoded public key string"
    ~coerce:(fun key -> `String (Public_key.Compressed.to_base58_check key))

let uint32 () =
  unsigned_scalar_scalar ~to_string:Unsigned.UInt32.to_string "UInt32"

let uint64 () =
  unsigned_scalar_scalar ~to_string:Unsigned.UInt64.to_string "UInt64"

let token_id () =
  scalar "TokenId" ~doc:"String representation of a token's UInt64 identifier"
    ~coerce:(fun tid -> `String (Mina_base.Token_id.to_string tid))

let epoch_seed () =
  scalar "EpochSeed" ~doc:"Base58Check-encoded epoch seed" ~coerce:(fun seed ->
      `String (Mina_base.Epoch_seed.to_base58_check seed) )
