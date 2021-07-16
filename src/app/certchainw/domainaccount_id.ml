

open Core_kernel



open Signature_lib





type t = Certchainw.Domain.t
[@@deriving sexp, equal, compare, hash, yojson]

let to_latest = Fn.id




let create domain = domain

let empty = Certchainw.Domain.empty

(*let public_key (key, _tid) = key*)

(*let token_id (_key, tid) = tid*)

let to_input domain = Certchainw.Domain.string_to_input domain


let gen =
  let open Quickcheck.Let_syntax in
  let%map domain = Certchainw.Domain.gen in
  domain

include Comparable.Make_binable (Stable.Latest)
include Hashable.Make_binable (Stable.Latest)



type var = Certchainw.Domain.var (*ATTN : todo implement var*)

let typ = Snarky.Typ.(Certchainw.Domain.typ) (*ATTN : todo implement typ*)

let var_of_t domain =
  Certchainw.Domain.var_of_t domain (*ATTN : todo implement var_of_t*)

module Checked = struct
  open Snark_params
  open Tick

  let create key = key


  let to_input domain =
    let%map domainc = Certchainw.Domain.Checked.to_input domain in
    domain

  let equal domain1 domain2 =
    let%bind dm_equal = Certchainw.Domain.Checked.equal domain1 domain2 in
    dm_equal

  let if_ b ~then_:domain_then ~else_:domain_else =
    let%bind domain =
      Certchainw.Domain.Checked.if_ b ~then_:domain_then ~else_:domain_else
    in
    domain
end

