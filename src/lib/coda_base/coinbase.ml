open Core
open Import
module Fee_transfer = Coinbase_fee_transfer

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { receiver: Public_key.Compressed.Stable.V1.t
      ; amount: Currency.Amount.Stable.V1.t
      ; fee_transfer: Fee_transfer.Stable.V1.t option }
    [@@deriving sexp, compare, eq, hash, yojson]

    let to_latest = Fn.id

    let description = "Coinbase"

    let version_byte = Base58_check.Version_bytes.coinbase
  end
end]

type t = Stable.Latest.t =
  { receiver: Public_key.Compressed.t
  ; amount: Currency.Amount.t
  ; fee_transfer: Fee_transfer.t option }
[@@deriving sexp, compare, eq, hash, yojson]

module Base58_check = Codable.Make_base58_check (Stable.Latest)

[%%define_locally
Base58_check.(to_base58_check, of_base58_check, of_base58_check_exn)]

[%%define_locally
Base58_check.String_ops.(to_string, of_string)]

let receiver_pk t = t.receiver

let receiver t = Account_id.create t.receiver Token_id.default

let amount t = t.amount

let fee_transfer t = t.fee_transfer

let accounts_accessed t =
  receiver t
  :: List.map ~f:Fee_transfer.receiver (Option.to_list t.fee_transfer)

let is_valid {amount; fee_transfer; _} =
  match fee_transfer with
  | None ->
      true
  | Some {fee; _} ->
      Currency.Amount.(of_fee fee <= amount)

let create ~amount ~receiver ~fee_transfer =
  let t = {receiver; amount; fee_transfer} in
  if is_valid t then
    let adjusted_fee_transfer =
      Option.bind fee_transfer ~f:(fun fee_transfer ->
          Option.some_if
            (Public_key.Compressed.equal receiver
               (Fee_transfer.receiver_pk fee_transfer))
            fee_transfer )
    in
    Ok {t with fee_transfer= adjusted_fee_transfer}
  else Or_error.error_string "Coinbase.create: invalid coinbase"

let supply_increase {receiver= _; amount; fee_transfer} =
  match fee_transfer with
  | None ->
      Ok amount
  | Some {fee; _} ->
      Currency.Amount.sub amount (Currency.Amount.of_fee fee)
      |> Option.value_map
           ~f:(fun _ -> Ok amount)
           ~default:(Or_error.error_string "Coinbase underflow")

let fee_excess t =
  Or_error.map (supply_increase t) ~f:(fun _increase ->
      Currency.Fee.Signed.zero )

module Gen = struct
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind receiver = Public_key.Compressed.gen in
    let%bind amount =
      Currency.Amount.(gen_incl zero Coda_compile_config.coinbase)
    in
    let max_fee = Currency.Amount.to_fee amount in
    let%map fee_transfer =
      Option.quickcheck_generator (Fee_transfer.Gen.gen ~max_fee)
    in
    let fee_transfer =
      match fee_transfer with
      | Some {Fee_transfer.receiver_pk; _}
        when Public_key.Compressed.equal receiver receiver_pk ->
          (* Erase fee transfer, to mirror [create]. *)
          None
      | _ ->
          fee_transfer
    in
    {receiver; amount; fee_transfer}

  let with_random_receivers ~keys ~min_amount ~max_amount ~fee_transfer =
    let open Quickcheck.Let_syntax in
    let%map receiver =
      let open Signature_lib in
      Quickcheck_lib.of_array keys
      >>| fun keypair -> Public_key.compress keypair.Keypair.public_key
    and amount = Int.gen_incl min_amount max_amount >>| Currency.Amount.of_int
    and fee_transfer = Option.quickcheck_generator fee_transfer in
    let fee_transfer =
      match fee_transfer with
      | Some {Fee_transfer.receiver_pk; _}
        when Public_key.Compressed.equal receiver receiver_pk ->
          (* Erase fee transfer, to mirror [create]. *)
          None
      | _ ->
          fee_transfer
    in
    {receiver; amount; fee_transfer}
end
