open Signature_lib
open Coda_base
open Core

let deserialize_optional_block_time = Option.map ~f:Types.Bitstring.of_yojson

module User_commands = struct
  let decode_optional_block_time = Option.map ~f:Types.Block_time.deserialize

  module Query_first_seen =
  [%graphql
  {|
    query query_first_seen ($hashes: [String!]!) {
        user_commands(where: {hash: {_in: $hashes}} ) {
            hash @bsDecoder(fn: "Transaction_hash.of_base58_check_exn")
            first_seen @bsDecoder(fn: "decode_optional_block_time")
        }
    }
|}]

  (* TODO: replace this with pagination *)
  module Query =
  [%graphql
  {|
    query query_user_commands ($hash: String!) {
        user_commands(where: {hash: {_eq: $hash}} ) {
            fee @bsDecoder (fn: "Types.Fee.deserialize")
            hash @bsDecoder(fn: "Transaction_hash.of_base58_check_exn")
            memo @bsDecoder(fn: "User_command_memo.of_string")
            nonce @bsDecoder (fn: "Types.Nonce.deserialize")
            public_key {
                value @bsDecoder (fn: "Public_key.Compressed.of_base58_check_exn")
            }
            publicKeyByReceiver {
              value @bsDecoder (fn: "Public_key.Compressed.of_base58_check_exn")
            } 
            typ @bsDecoder (fn: "Types.User_command_type.decode")
            amount @bsDecoder (fn: "Types.Amount.deserialize")
            first_seen @bsDecoder(fn: "decode_optional_block_time")
        }
    }
|}]

  module Insert =
  [%graphql
  {|
    mutation transaction_insert($user_commands: [user_commands_insert_input!]!) {
    insert_user_commands(objects: $user_commands,
    on_conflict: {constraint: user_commands_hash_key, update_columns: first_seen}
    ) {
      returning {
        id
        hash @bsDecoder(fn: "Transaction_hash.of_base58_check_exn")
        first_seen @bsDecoder(fn: "deserialize_optional_block_time")
      }
    }
  }
|}]
end

module Public_key = struct
  module Query =
  [%graphql
  {|
    query query_public_keys {
        public_keys {
            value @bsDecoder (fn: "Public_key.Compressed.of_base58_check_exn")
        }
    }
|}]
end

module Clear_data =
[%graphql
{|
  mutation clear  {
    delete_user_commands(where: {}) {
      affected_rows
    }
      
    delete_public_keys(where: {}) {
      affected_rows
    }

  }
|}]
