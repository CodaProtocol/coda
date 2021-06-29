open Core
open Import

module Single = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { receiver_pk: Public_key.Compressed.Stable.V1.t
        ; fee: Currency.Fee.Stable.V1.t
        ; fee_token: Token_id.Stable.V1.t }
      [@@deriving sexp, compare, equal, yojson, hash]

      let to_latest = Fn.id

      let description = "Fee transfer Single"

      let version_byte = Base58_check.Version_bytes.fee_transfer_single
    end
  end]

  include Comparable.Make (Stable.Latest)
  module Base58_check = Codable.Make_base58_check (Stable.Latest)

  [%%define_locally
  Base58_check.(to_base58_check, of_base58_check, of_base58_check_exn)]

  [%%define_locally
  Base58_check.String_ops.(to_string, of_string)]

  let create ~receiver_pk ~fee ~fee_token = {receiver_pk; fee; fee_token}

  let receiver_pk {receiver_pk; _} = receiver_pk

  let receiver {receiver_pk; fee_token; _} =
    Account_id.create receiver_pk fee_token

  let fee {fee; _} = fee

  let fee_token {fee_token; _} = fee_token

  module Gen = struct
    let with_random_receivers ?(min_fee = 0) ~keys ~max_fee ~token :
        t Quickcheck.Generator.t =
      let open Quickcheck.Generator.Let_syntax in
      let%map receiver_pk =
        let open Signature_lib in
        Quickcheck_lib.of_array keys
        >>| fun keypair -> Public_key.compress keypair.Keypair.public_key
      and fee = Int.gen_incl min_fee max_fee >>| Currency.Fee.of_int
      and fee_token = token in
      {receiver_pk; fee; fee_token}
  end
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t = Single.Stable.V1.t One_or_two.Stable.V1.t
    [@@deriving sexp, compare, equal, yojson, hash]

    let to_latest = Fn.id
  end
end]

type single = Single.t =
  { receiver_pk: Public_key.Compressed.t
  ; fee: Currency.Fee.t
  ; fee_token: Token_id.t }
[@@deriving sexp, compare, yojson, hash]

let to_singles = Fn.id

let of_singles = function
  | `One _ as t ->
      Or_error.return t
  | `Two (one, two) as t ->
      if Token_id.equal one.fee_token two.fee_token then Or_error.return t
      else
        (* Necessary invariant for the transaction snark: we should never have
           fee excesses in multiple tokens simultaneously.
        *)
        Or_error.errorf
          !"Cannot combine single fee transfers with incompatible tokens: \
            %{sexp: Token_id.t} <> %{sexp: Token_id.t}"
          one.fee_token two.fee_token

let create one two =
  let singles =
    match two with None -> `One one | Some two -> `Two (one, two)
  in
  of_singles singles

let create_single ~receiver_pk ~fee ~fee_token =
  `One (Single.create ~receiver_pk ~fee ~fee_token)

include Comparable.Make (Stable.Latest)

let fee_excess ft =
  ft
  |> One_or_two.map ~f:(fun {fee_token; fee; _} ->
         (fee_token, Currency.Fee.Signed.(negate (of_unsigned fee))) )
  |> Fee_excess.of_one_or_two

let receiver_pks t =
  One_or_two.to_list (One_or_two.map ~f:Single.receiver_pk t)

let receivers t = One_or_two.to_list (One_or_two.map ~f:Single.receiver t)

(* This must match [Transaction_union].
   TODO: enforce this.
*)
let fee_payer_pk ft =
  match ft with
  | `One ft ->
      Single.receiver_pk ft
  | `Two (_, ft) ->
      Single.receiver_pk ft

let fee_token = Single.fee_token

let fee_tokens = One_or_two.map ~f:Single.fee_token

let map = One_or_two.map

let fold = One_or_two.fold

let to_list = One_or_two.to_list

let to_numbered_list = One_or_two.to_numbered_list
