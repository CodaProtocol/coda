open Ppxlib
open Asttypes
open Parsetree
open Longident
open Core
open Async
open Pickles_types

let proof_string prev_width =
  let dummy = Pickles.Proof.dummy Nat.N2.n Nat.N2.n prev_width in
  Binable.to_string (module Pickles.Proof.Branching_2.Stable.Latest) dummy

let blockchain_proof_string = proof_string Nat.N2.n

let transaction_proof_string = proof_string Nat.N0.n

let str ~loc =
  let module E = Ppxlib.Ast_builder.Make (struct
    let loc = loc
  end) in
  let open E in
  [%str
    let blockchain_proof, transaction_proof =
      ( Core_kernel.Binable.of_string
          (module Pickles.Proof.Branching_2.Stable.Latest)
          [%e estring blockchain_proof_string]
      , Core_kernel.Binable.of_string
          (module Pickles.Proof.Branching_2.Stable.Latest)
          [%e estring transaction_proof_string] )]

let main () =
  let fmt =
    Format.formatter_of_out_channel (Out_channel.create "dummy_values.ml")
  in
  let loc = Ppxlib.Location.none in
  Pprintast.top_phrase fmt (Ptop_def (str ~loc)) ;
  ignore (exit 0 : 'a Deferred.t)

let () =
  main () ;
  never_returns (Scheduler.go ())
