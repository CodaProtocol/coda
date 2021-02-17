open Snark_params
open Core
open Signature_lib
open Mina_base
open Async

let create_hd_account_summary =
  "Create an account with hardware wallet - this will let the hardware wallet \
   generate a keypair corresponds to the HD-index you give and store this \
   HD-index and the generated public key in the daemon. Calling this command \
   with the same HD-index and the same hardware wallet will always generate \
   the same keypair."

let hardware_wallet_script = "codaledgercli"

module type Tick_intf = sig
  type field

  module Bigint : sig
    type t

    val of_bignum_bigint : Bigint.t -> t

    val to_field : t -> field
  end
end

let decode_field (type field) (module Tick : Tick_intf with type field = field)
    : string -> field =
 fun field ->
  Bytes.of_string field
  |> B58.decode Base58_check.coda_alphabet
  |> Bytes.to_list |> List.rev |> Bytes.of_char_list |> Bytes.to_string
  |> String.foldi ~init:Bigint.zero ~f:(fun i acc byte ->
         Bigint.(acc lor (of_int (Char.to_int byte) lsl Int.( * ) 8 i)) )
  |> Tick.Bigint.of_bignum_bigint |> Tick.Bigint.to_field

type public_key = {status: string; x: string; y: string} [@@deriving yojson]

let decode_status_code ~f = function
  | "Ok" ->
      Ok (f ())
  | "Hardware_wallet_not_found" ->
      Error
        "Hardware wallet not found. Is the device plugged in and activated?"
  | "Computation_aborted" ->
      Error "An internal error happens in hardware wallet."
  | s ->
      Error (sprintf !"Unknown status code returned by hardware wallet: %s" s)

let report_json_error s =
  sprintf !"Failed to parse the json returned by hardware wallet: %s" s

let report_process_error e =
  sprintf
    !"Failed to communicate with hardware wallet program %s: %s.\n\
      Do you have the auxiliary dependencies required for using the hardware \
      wallet?"
    hardware_wallet_script (Error.to_string_hum e)

let decode_public_key : string -> (Public_key.t, string) Result.t =
 fun public_key ->
  Yojson.Safe.from_string public_key
  |> public_key_of_yojson
  |> Result.map_error ~f:report_json_error
  |> Result.bind ~f:(fun {status; x; y} ->
         decode_status_code status ~f:(fun () ->
             ( decode_field (module Snark_params.Tick) x
             , decode_field (module Snark_params.Tick) y ) ) )

type signature = {status: string; field: string; scalar: string}
[@@deriving yojson]

let decode_signature : string -> (Signature.t, string) Result.t =
 fun signature ->
  Yojson.Safe.from_string signature
  |> signature_of_yojson
  |> Result.map_error ~f:report_json_error
  |> Result.bind ~f:(fun {status; field; scalar} ->
         decode_status_code status ~f:(fun () ->
             ( decode_field (module Snark_params.Tick) field
             , decode_field (module Snark_params.Tock) scalar ) ) )

let compute_public_key ~hd_index =
  let prog, args =
    ( "python3"
    , [ "-m" ^ hardware_wallet_script
      ; "--request=publickey"
      ; "--nonce=" ^ Mina_numbers.Hd_index.to_string hd_index ] )
  in
  Process.run ~prog ~args ()
  |> Deferred.Result.map_error ~f:report_process_error
  |> Deferred.map ~f:(Result.bind ~f:decode_public_key)

let sign ~hd_index ~public_key ~user_command_payload :
    (Signed_command.With_valid_signature.t, string) Deferred.Result.t =
  let open Deferred.Result.Let_syntax in
  let input =
    Transaction_union_payload.to_input
    @@ Transaction_union_payload.of_user_command_payload user_command_payload
  in
  let fields = Random_oracle.pack_input input in
  let messages =
    Array.map fields ~f:(fun field -> Tick.Field.to_string field)
  in
  if Array.length messages <> 2 then
    Deferred.Result.fail "Malformed user command"
  else
    let prog, args =
      ( "python3"
      , [ "-m" ^ hardware_wallet_script
        ; "--request=sign"
        ; "--msgx=" ^ messages.(0)
        ; "--msgm=" ^ messages.(1)
        ; "--nonce=" ^ Mina_numbers.Hd_index.to_string hd_index ] )
    in
    let%bind signature_str =
      Process.run ~prog ~args ()
      |> Deferred.Result.map_error ~f:report_process_error
    in
    let%bind signature = decode_signature signature_str |> Deferred.return in
    match
      Signed_command.create_with_signature_checked signature
        (Public_key.compress public_key)
        user_command_payload
    with
    | Some signed_command ->
        return signed_command
    | None ->
        let%bind computed_public_key = compute_public_key ~hd_index in
        if Public_key.equal computed_public_key public_key then
          Deferred.Result.fail
            "Failed to verify signature returned by hardware wallet."
        else
          Deferred.Result.fail
            "The cached public doesn't match the one that is computed by the \
             hardware wallet. Possible reason could be you are using a \
             different hardware wallet or you reinitialized your hardware \
             wallet using a different seed. If you want to use your new \
             ledger, please first create an account by 'coda account \
             create-hd' command"

let sign_snapp ~hd_index:(_ : Unsigned.UInt32.t) ~public_key:(_ : Public_key.t)
    ~snapp_payload_digest:(_ : Tick.Field.t) :
    (Signature.t, string) Deferred.Result.t =
  Deferred.Result.fail
    "The hardware wallet script currently does not support signing snapp \
     commands"

let write_exn ~hd_index ~index_path : unit Deferred.t =
  let%bind index_file = Writer.open_file index_path in
  Writer.write_line index_file (Mina_numbers.Hd_index.to_string hd_index) ;
  Writer.close index_file
