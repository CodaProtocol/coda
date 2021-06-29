let length_in_bytes = 20

module T : sig
  type t = private string

  val create : string -> t
end = struct
  type t = string

  let padding_char = '*'

  let create s : t =
    let string_length = String.length s in
    assert (string_length <= length_in_bytes) ;
    let diff = length_in_bytes - string_length in
    let r = s ^ String.init diff (fun _ -> padding_char) in
    assert (String.length r = length_in_bytes) ;
    r
end

include T

let protocol_state = create "CodaProtoState"

let protocol_state_body = create "CodaProtoStateBody"

let account = create "CodaAccount"

let side_loaded_vk = create "CodaSideLoadedVk"

let snapp_account = create "CodaSnappAccount"

let snapp_payload = create "CodaSnappPayload"

let snapp_body = create "CodaSnappBody"

let merkle_tree i = create (Printf.sprintf "CodaMklTree%03d" i)

let coinbase_merkle_tree i = create (Printf.sprintf "CodaCbMklTree%03d" i)

let merge_snark = create "CodaMergeSnark"

let base_snark = create "CodaBaseSnark"

let transition_system_snark = create "CodaTransitionSnark"

let signature_testnet = create "CodaSignature"

let signature_mainnet = create "MinaSignatureMainnet"

let receipt_chain_user_command = create "CodaReceiptUC"

let receipt_chain_snapp = create "CodaReceiptSnapp"

let epoch_seed = create "CodaEpochSeed"

let vrf_message = create "CodaVrfMessage"

let vrf_output = create "CodaVrfOutput"

let vrf_evaluation = create "MinaVrfEvaluation"

let pending_coinbases = create "PendingCoinbases"

let coinbase_stack_data = create "CoinbaseStackData"

(* length is limited, so we drop some characters here *)
let coinbase_stack_state_hash = create "CoinbaseStackStaHash"

let coinbase_stack = create "CoinbaseStack"

let coinbase = create "Coinbase"

let checkpoint_list = create "CodaCheckpoints"

let bowe_gabizon_hash = create "CodaTockBGHash"

let snapp_predicate = create "CodaSnappPred"

let snapp_predicate_account = create "CodaSnappPredAcct"

let snapp_predicate_protocol_state = create "CodaSnappPredPS"
