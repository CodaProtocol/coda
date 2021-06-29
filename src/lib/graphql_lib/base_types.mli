open Graphql_async
open Schema
open Signature_lib
open Unsigned

val public_key : unit -> ('a, Public_key.Compressed.t option) typ

val epoch_seed : unit -> ('a, Mina_base.Epoch_seed.t option) typ

val uint32 : unit -> ('a, UInt32.t option) typ

val uint64 : unit -> ('a, UInt64.t option) typ

val token_id : unit -> ('a, Mina_base.Token_id.t option) typ
