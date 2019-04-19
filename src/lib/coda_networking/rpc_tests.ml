(* rpc_tests.ml -- deserialization tests for RPC types *)

(* TODO : uncomment unit tests below when Coda_networking, Rpcs are defunctored so we can
   run unit tests here
*)

let%test_module "RPC deserialization tests" =
  ( module struct
    let serialization_in_buffer serialization =
      let len = String.length serialization in
      let buff = Bin_prot.Common.create_buf len in
      Bin_prot.Common.blit_string_buf serialization buff ~len ;
      buff

    (* Get_staged_ledger_aux_and_pending_coinbases_at_hash *)

    (* let%test "Get_staged_ledger_aux_and_pending_coinbases_at_hash V1 \
                deserialize query" =
        (* serialization should fail if the query type has changed *)
        let known_good_serialization =
          "\x01\x28\x61\x68\xB6\x65\x95\x11\x82\x15\xAC\x9D\x8B\x24\x6F\x5D\x85\xE0\x3B\xE6\x5A\x27\x3C\x2930\x04\x6E\xE9\xBE\xE4\x04\xFA\xFB\x44\xF6\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01\x09\x31\x32\x37\x2E\x30\x2E\x30\x2E\x31\xFE\xDB\x59\xFE\xDA\x59"
        in
        let buff = serialization_in_buffer known_good_serialization in
        let pos_ref = ref 0 in
        let _ : query = V1.bin_read_query buff ~pos_ref in
        true
    *)

    (* Answer_sync_ledger_query *)

    (* let%test "Answer_sync_ledger_query V1 deserialize query" =
       let known_good_serialization = "\x01\x28\x3F\xA0\x11\xB1\xAF\x94\x42\x1E\x3F\x15\xDA\x35\x72\x2B\x55\xA2\x3C\xC7\xD5\x4C\xA5\xB7\x2E\x95\xD3\x45\xCA\xAE\x11\x14\x12\xD6\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x01\x01\x01\x09\x31\x32\x37\x2E\x30\x2E\x30\x2E\x31\xFE\xDB\x59\xFE\xDA\x59"
       in     
       let buff = serialization_in_buffer known_good_serialization in
       let pos_ref = ref 0 in
       let _ : query = V1.bin_read_query buff ~pos_ref in
       true
     *)

    (*
  let%test "Answer_sync_ledger_query V1 deserialize response" =
  (* serialization should fail if the response type has changed *)
  let known_good_serialization = "\x00\x01\x02\x12\x01\x28\xC7\xBC\x6A\x07\xC9\x22\x93\xFD\xA4\x57\x7D\xE2\xF0\x3E\xDC\xB4\x56\x6A\xCB\xF8\x6E\x94\xCD\xC2\x61\x72\x9A\xA5\x8E\xAD\x5D\xFD\x00\x00\x00\x00\x00\x00\x00\x00" in
  let buff = serialization_in_buffer known_good_serialization in
  let pos_ref = ref 0 in
  let _ : response = V1.bin_read_response buff ~pos_ref in
  true
    *)

    (* Get_ancestry *)

    (* let%test "Get_ancestry V1 deserialize query" =
       let known_good_serialization = "\x01\x01\xFE\x84\x00\x01\x00\x01\xFE\x90\x00\x01\x01\x20\xD4\xAA\xAE\x2D\x5A\x90\x09\x1E\x7B\x90\x37\x04\x4B\x44\xE5\xE2\x59\x21\x0E\x12\x4D\x77\xEF\x94\x3B\xDB\x6B\x20\x17\x26\xBD\x0E\x01\xFD\xC4\xD9\x98\x00\x01\x00\x01\xFE\x8A\x00\x01\x01\x01\x28\x1C\x3C\x72\x45\x81\x40\x18\xAB\x34\xF5\xD7\x69\x14\x1D\xC0\x02\x9C\x22\x65\x08\x83\x22\xB0\xB2\xC3\x4C\x65\x25\xB5\x07\x2A\x4E\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFD\x64\xD5\x98\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01\x01\x01\x28\x1C\x3C\x72\x45\x81\x40\x18\xAB\x34\xF5\xD7\x69\x14\x1D\xC0\x02\x9C\x22\x65\x08\x83\x22\xB0\xB2\xC3\x4C\x65\x25\xB5\x07\x2A\x4E\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFD\x64\xD5\x98\x00\x01\x28\x83\x79\x00\x9F\x06\xC2\x3B\x53\x89\x59\x34\x0C\xDE\xDA\x8F\x13\xE6\x7A\x3F\x48\xC4\x47\x9A\xFB\xDA\x47\x06\xBA\xE4\x81\x19\x5E\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x90\x44\x21\x2E\x57\x28\x32\x55\xE2\xBE\x18\xB8\x23\xBD\x0D\x41\xB4\x75\x70\x67\x5C\xB3\x9E\x78\xE4\x97\x7D\xF6\x40\xF2\x1B\x02\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFE\x85\x00\x01\x01\x01\x01\x28\x30\xFE\x20\x17\xD8\x15\x62\xE2\x30\xB4\x42\x4E\x9E\xE6\x1A\xF3\x13\x6E\x47\x6D\xD8\x19\xEC\xED\xCC\x67\xDE\x90\x55\x7E\x6C\xA4\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01\x09\x31\x32\x37\x2E\x30\x2E\x30\x2E\x31\xFE\xDB\x59\xFE\xDA\x59"
in
  let buff = serialization_in_buffer known_good_serialization in
  let pos_ref = ref 0 in
  let _ : query = V1.bin_read_query buff ~pos_ref in
  true
     *)

    (* let%test "Get_ancestry V1 deserialize response" =
       let known_good_serialization = "\x01\x01\x01\x01\x28\xE3\xEE\xC6\x6B\xA4\xEC\x65\xED\x53\xDB\xE8\x3B\xDE\x94\x10\xCC\x52\x87\xC4\xB6\x59\xD9\xBF\xC0\xB7\x3E\xD6\x46\xA1\x2B\xAB\xE8\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01\x01\x01\x28\x4B\x84\x41\x99\x20\x29\x41\xEF\x51\x12\x3D\xE0\x51\x23\x46\x05\x07\xB3\xC9\x5F\xCB\xC9\xAC\x3C\xDC\x0C\x95\xA6\x7A\xE2\xD6\x8D\x00\x00\x00\x00\x00\x00\x00\x00\x01\x21\x25\xC5\x06\x95\x82\x33\xF2\x00\x79\x20\xB8\xB3\xBD\x39\x2A\x15\xFB\xE4\x38\x79\x83\x02\x61\xC0\x69\xDD\x71\x63\x51\x6E\xAB\xD8\x36\x01\x20\x34\x9C\x41\x20\x1B\x62\xDB\x85\x11\x92\x66\x5C\x50\x4B\x35\x0F\xF9\x8C\x6B\x45\xFB\x62\xA8\xA2\x16\x1F\x78\xB6\x53\x4D\x8D\xE9\x01\x01\x28\x78\xBD\x2C\x62\xF2\x60\xEF\x4C\xFD\x88\x4F\x57\xD4\x52\x54\x98\xF6\xF3\xC4\x5E\xEF\x93\x6C\xC9\xED\xA1\x6C\xFB\x5A\xAB\x4A\x64\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x3F\xA0\x11\xB1\xAF\x94\x42\x1E\x3F\x15\xDA\x35\x72\x2B\x55\xA2\x3C\xC7\xD5\x4C\xA5\xB7\x2E\x95\xD3\x45\xCA\xAE\x11\x14\x12\xD6\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFC\xBE\xD1\x5A\xA0\x68\x01\x00\x00\x01\x01\x7F\x01\x00\x01\xFE\x90\x00\x01\x01\x20\xD1\xE1\x37\x85\x25\xE7\x72\xAF\xC6\x36\xE2\xA2\x21\x0D\x28\x0F\x8B\x32\x75\xF7\x3F\xC6\x56\xF8\x68\x75\xE2\xF8\xB7\x73\xD5\xEF\x01\xFD\xC4\xD9\x98\x00\x01\x00\x01\xFE\x85\x00\x01\x01\x01\x28\x1C\x3C\x72\x45\x81\x40\x18\xAB\x34\xF5\xD7\x69\x14\x1D\xC0\x02\x9C\x22\x65\x08\x83\x22\xB0\xB2\xC3\x4C\x65\x25\xB5\x07\x2A\x4E\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFD\x64\xD5\x98\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01\x01\x01\x28\x1C\x3C\x72\x45\x81\x40\x18\xAB\x34\xF5\xD7\x69\x14\x1D\xC0\x02\x9C\x22\x65\x08\x83\x22\xB0\xB2\xC3\x4C\x65\x25\xB5\x07\x2A\x4E\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFD\x64\xD5\x98\x00\x01\x28\x83\x79\x00\x9F\x06\xC2\x3B\x53\x89\x59\x34\x0C\xDE\xDA\x8F\x13\xE6\x7A\x3F\x48\xC4\x47\x9A\xFB\xDA\x47\x06\xBA\xE4\x81\x19\x5E\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x01\x47\x58\x90\x97\xAF\x28\xF1\xD7\x4C\x76\x63\xC2\x97\x2D\xD8\xA0\xB7\x60\x84\x2D\xAD\x2D\xE1\xC8\xE7\x43\xCB\x5E\x69\x85\xB3\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFE\x80\x00\x01\x01\x01\x01\x28\x30\xFE\x20\x17\xD8\x15\x62\xE2\x30\xB4\x42\x4E\x9E\xE6\x1A\xF3\x13\x6E\x47\x6D\xD8\x19\xEC\xED\xCC\x67\xDE\x90\x55\x7E\x6C\xA4\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFE\x93\x01\x30\x21\x33\xCA\x8C\x49\x65\x39\xCF\x9A\x7B\xD7\x35\x44\x23\xFA\xED\x24\xBF\xAA\x0E\x48\x7A\x60\xFB\xCB\x34\x34\x59\xF6\xAC\x4B\xA2\xE6\xEA\x0C\xA8\xAC\x02\x00\x00\xBA\xD0\xEF\xBC\xBB\x76\x2B\x7C\xDF\x0F\x39\x10\x04\x39\xE8\x5A\xED\x14\x4D\xE8\x25\x35\x40\x52\x49\xEA\x79\xAA\xAD\x14\xEF\x9B\x37\xB4\x40\x3F\xAE\x01\x00\x00\x30\x1B\x1C\x76\x25\xA3\x63\xA6\xDC\x4A\x87\xDF\xEF\x6B\x5E\x2A\x3A\x11\xEB\xF4\xE9\xDC\x8F\x3E\x63\xA3\x4C\xC2\x48\xB6\xD4\x0B\xF1\x84\x52\x8F\x8D\xDE\x00\x00\x00\x53\x64\xD8\x44\x9B\x93\xD7\x8B\xCB\xD0\xE2\x7C\x02\xF5\xAF\xAC\x37\x16\xC9\x93\xB8\xB8\x85\x8A\xE3\x24\xB4\xDB\x26\x06\x31\x15\xFE\xD7\x40\x0C\x1D\x00\x00\x00\x38\x38\x64\x12\x64\x41\x00\x99\x14\x87\xAF\x12\x27\x24\x58\x63\x86\x13\x8F\xA1\xDA\x0F\xEF\x27\x64\x8E\xEE\x5D\x23\x08\xB0\xD5\x70\x2B\x2A\x55\x4E\x01\x00\x00\x50\x7E\xC5\x77\xF5\x93\x47\xBA\x70\x27\x65\xAE\x12\x9F\xA3\x15\x5D\x6B\x0D\x74\x6D\x5A\x93\x59\xE2\x14\x55\xEB\x13\xD7\x97\x0E\xC3\xBA\x6C\xBA\x17\x03\x00\x00\xC8\x9B\x63\xC1\xD8\x0F\x06\xD9\x9D\x3D\x61\x32\x4C\x45\x07\xD5\xE1\x61\x4D\x1B\x95\x78\x09\xAA\x4E\x25\x77\xF6\x4E\x64\x92\x8F\x64\x04\xFE\x58\x41\x01\x00\x00\x7C\xD7\x1C\x2B\x44\x91\xCB\x3F\x3B\x2D\xB4\x55\xBB\x94\x2A\x2B\x99\xCC\x30\xA7\xE3\xA1\x34\xB9\xB0\x94\x23\x5B\x17\xA3\xE0\x1C\x3B\x5D\x0E\xB2\x6A\x03\x00\x00\x30\xA8\xAE\x7E\xDF\xCB\xFC\xCE\x94\xE4\xFF\x8D\x14\x7C\xD4\x1B\x10\x34\xF4\x89\xDE\xC6\x44\xB7\xAF\xBA\x4C\xE4\xFC\x3D\x04\x8C\xF0\xBC\x9C\x39\x15\xB8\x01\x00\x00\x8E\x54\x6B\xB6\xDD\xF6\x1F\xE4\xAC\x46\x8C\x17\x48\xD6\x97\x77\x1E\x07\x8B\x96\x1C\x48\xB3\xC5\xD0\x49\xCE\x69\x16\x1E\x14\xFE\xD2\x48\x7A\x9E\x5D\x01\x00\x00\x01\x01\x01\x01\x00\x02\x01\x01\x01\x28\x54\x5E\xDA\x82\x7E\xC4\x28\x38\xDF\xCD\xE8\x3C\x86\xD4\x62\x76\xDD\xBE\x91\xEF\xEF\xFB\x63\xFD\x63\xF8\xFB\x79\x83\xD0\xD7\xC8\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x5F\x69\x4B\x31\x5F\x6F\x4A\xE5\x46\xFF\xA3\x64\x35\xF6\xAA\xBE\x26\x6D\xF6\xE8\xE9\x46\x52\xD0\xCA\xE5\x17\x17\xF4\x42\x2C\x34\x00\x00\x00\x00\x00\x00\x00\x00\x01\x14\x01\x01\x01\x28\x07\x70\x5C\x3C\x86\x69\x4C\xA8\xC9\xBC\xAF\x44\x5A\x12\xF6\x0F\x0A\xCC\x15\x3F\x38\x45\x69\x4E\xE4\xAD\x87\x11\x38\xB3\x31\x6C\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x28\xD1\x89\x46\xE7\x16\x95\x69\x25\xA7\x95\x22\x17\x14\xEB\xFE\x72\x58\xF6\x42\xEC\xA6\x9B\x53\x5B\xB6\x3B\x68\x74\xBD\x14\x78\x19\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x00\x01\x00\x01\x31\x25\xE8\x31\x01\x01\x20\x0B\xE0\xAB\xAA\x7C\x06\x56\xB6\x23\x54\x3A\xE7\x40\x3C\xD7\xAD\x98\x79\x7C\xED\x14\x02\x7E\xD2\x53\x74\x15\xC2\xA8\xCA\x48\x18\x01\x01\x01\x28\x5F\x69\x4B\x31\x5F\x6F\x4A\xE5\x46\xFF\xA3\x64\x35\xF6\xAA\xBE\x26\x6D\xF6\xE8\xE9\x46\x52\xD0\xCA\xE5\x17\x17\xF4\x42\x2C\x34\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x49\xA8\xF2\xCD\xEF\xE4\x77\xC4\xF9\xE3\x8E\xA0\xF6\xA0\x12\x37\x63\xAE\xDE\x5F\xD8\xF2\x3C\xF7\x51\xFD\x10\x1D\xA7\xEB\x7D\x99\x00\x00\x00\x00\x00\x00\x00\x00\x01\x0A\x01\x01\x01\x28\xD1\x89\x46\xE7\x16\x95\x69\x25\xA7\x95\x22\x17\x14\xEB\xFE\x72\x58\xF6\x42\xEC\xA6\x9B\x53\x5B\xB6\x3B\x68\x74\xBD\x14\x78\x19\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x28\x09\xA1\x8D\xCE\x2B\x2E\xBD\xEA\xB8\xB5\x77\x14\xC3\x15\x5F\x43\x0C\x8F\xCA\xBF\xCA\x79\xF6\x7B\x75\x71\x25\x8D\x9B\x1E\x27\x70\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x00\x01\x00\x01\x63\x28\xD4\x57\x01\x01\x20\x0B\xE0\xAB\xAA\x7C\x06\x56\xB6\x23\x54\x3A\xE7\x40\x3C\xD7\xAD\x98\x79\x7C\xED\x14\x02\x7E\xD2\x53\x74\x15\xC2\xA8\xCA\x48\x18\x01\x28\x10\x4C\x28\xBB\x53\x4D\xE2\xCF\x2D\xB1\x30\x94\xEC\xFF\x89\xEC\x0F\x40\x69\x4D\x43\xA5\x18\x4E\xF2\x14\x90\x97\xA3\x7B\xB2\x1F\xF3\x9A\xAC\xDC\x9D\x02\x00\x00\x01\x00\x01\x00\x00\x01\x01\x01\x28\x49\xA8\xF2\xCD\xEF\xE4\x77\xC4\xF9\xE3\x8E\xA0\xF6\xA0\x12\x37\x63\xAE\xDE\x5F\xD8\xF2\x3C\xF7\x51\xFD\x10\x1D\xA7\xEB\x7D\x99\x00\x00\x00\x00\x00\x00\x00\x00\x01\x21\xCD\x4D\xCF\x72\xD6\xD8\x4D\xA7\xD4\xA8\x16\x0B\x04\x58\x27\x53\x57\x75\x16\x29\x8F\x02\x1A\x47\x2A\x35\xF0\x0E\x33\x0E\xB0\x9C\x36\x01\x20\x34\x9C\x41\x20\x1B\x62\xDB\x85\x11\x92\x66\x5C\x50\x4B\x35\x0F\xF9\x8C\x6B\x45\xFB\x62\xA8\xA2\x16\x1F\x78\xB6\x53\x4D\x8D\xE9\x01\x01\x28\x72\x8A\xC4\x1F\x03\xA2\x64\xA9\xFE\x80\x5F\x98\x22\x8A\xC3\xC9\xC2\x68\xCC\xFA\x2F\x38\x6E\x4D\xC9\xB3\xE8\x97\x76\xD1\x0C\xA2\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x10\x4C\x28\xBB\x53\x4D\xE2\xCF\x2D\xB1\x30\x94\xEC\xFF\x89\xEC\x0F\x40\x69\x4D\x43\xA5\x18\x4E\xF2\x14\x90\x97\xA3\x7B\xB2\x1F\xF3\x9A\xAC\xDC\x9D\x02\x00\x00\x01\x06\x01\x28\x34\xFC\xE7\x3F\x3C\x55\xCE\x21\x80\x08\x3B\x6A\xD7\xAC\xBA\x47\x78\x32\xCD\x81\x61\x52\x50\x80\xB4\xA9\x98\x13\xFA\x71\x70\x30\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x01\x35\xBA\x19\x80\x45\x19\x64\x2C\x10\xFB\x28\xAD\x7C\xDD\xBF\xB4\x19\x40\x35\x2A\x93\x62\x03\xB1\x7B\xAE\xCD\x72\xE3\x70\xA8\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xE1\xE0\x06\x52\xC3\x98\xB3\x21\xF8\x50\xF2\x8F\xD3\xC6\x44\xDC\xFC\x51\xEA\xFA\xC3\xE1\xDB\x9D\x86\xE2\xDC\x32\xBD\xD5\x29\xFB\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x4E\x12\x4D\x12\x5E\x42\x09\x71\x92\x66\x0D\x60\xDF\x56\xD2\xA7\x8E\x3F\x08\xF8\x10\x30\x3C\xB0\x87\xA7\x29\x06\xA0\x37\xE7\x47\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xE1\x21\x86\xDF\x21\xB8\x77\x41\x0F\x80\x48\xB8\xF0\xDA\x19\xD0\xDC\x2A\xEE\x23\xA5\x9A\x95\x1F\x48\xC1\xA9\x01\x5B\x89\xEE\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xE5\xEB\x40\x22\xEB\xA7\x63\xFA\x3C\x93\x59\x8A\xAC\xC6\xAA\xF3\x15\xA7\xE6\x3D\x4D\x3B\x4C\x54\xA3\xF1\x82\xFF\xEE\x9B\x05\xE2\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01\x28\x15\x73\x1D\xBE\x14\xB3\xF7\x05\x3D\xA1\xCA\xAF\x5C\x5C\xBB\xAF\x1D\x78\xDC\x42\xCC\x19\x76\x92\x28\x07\xD9\x79\xAB\x2F\x79\xCA\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01\x01\x01\x28\x68\xDF\xCC\xE3\xEC\x3A\x7E\x00\x49\x21\x93\xD5\xAA\x2C\x14\xAC\xBC\x2B\x40\x1E\x28\xC0\x2C\xD5\x71\x65\x74\xA2\xA3\x98\x1C\xF6\x00\x00\x00\x00\x00\x00\x00\x00\x01\x21\x84\x45\xB2\x6A\x0C\x80\x88\x0D\x92\x09\x68\x25\xC6\x9D\x00\xD8\x65\x09\xFB\xEF\x7B\x49\x01\xAA\xBE\x49\x21\x19\x59\x1C\xEE\xAE\x37\x01\x20\x4F\x97\xF2\xEE\xBF\x92\xCD\xE5\x8C\x10\x34\x66\x71\x2F\xA2\xF6\x5B\x10\xD0\x6F\xF8\xF1\x93\x4D\x78\xFF\x59\x2F\xA0\x57\x5E\x27\x01\x01\x28\x95\xE8\x92\x93\x7E\x5E\x9C\xA1\x3F\xD9\x20\x0A\x47\xD4\xC0\x38\xBD\x8F\x9E\x4A\xCC\x3E\xC8\x13\xE2\xC7\x3D\x3F\xDE\x3B\x54\x1B\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x3F\xA0\x11\xB1\xAF\x94\x42\x1E\x3F\x15\xDA\x35\x72\x2B\x55\xA2\x3C\xC7\xD5\x4C\xA5\xB7\x2E\x95\xD3\x45\xCA\xAE\x11\x14\x12\xD6\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFC\x10\xE9\x5A\xA0\x68\x01\x00\x00\x01\x01\xFE\x85\x00\x01\x00\x01\xFE\x90\x00\x01\x01\x20\x7F\x99\x80\x4A\xC7\xC8\xC8\xEB\xF2\x0D\x2C\x02\x00\xE6\xCB\xA8\x83\x12\x22\xE7\x00\xBF\x5D\x87\x5E\x8F\xCF\x64\x2A\x4C\x9F\x26\x01\xFD\xC4\xD9\x98\x00\x01\x00\x01\xFE\x8B\x00\x01\x01\x01\x28\x1C\x3C\x72\x45\x81\x40\x18\xAB\x34\xF5\xD7\x69\x14\x1D\xC0\x02\x9C\x22\x65\x08\x83\x22\xB0\xB2\xC3\x4C\x65\x25\xB5\x07\x2A\x4E\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFD\x64\xD5\x98\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01\x01\x01\x28\x1C\x3C\x72\x45\x81\x40\x18\xAB\x34\xF5\xD7\x69\x14\x1D\xC0\x02\x9C\x22\x65\x08\x83\x22\xB0\xB2\xC3\x4C\x65\x25\xB5\x07\x2A\x4E\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFD\x64\xD5\x98\x00\x01\x28\x83\x79\x00\x9F\x06\xC2\x3B\x53\x89\x59\x34\x0C\xDE\xDA\x8F\x13\xE6\x7A\x3F\x48\xC4\x47\x9A\xFB\xDA\x47\x06\xBA\xE4\x81\x19\x5E\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\xB9\x48\x72\xC5\x86\xF8\x96\x6B\x03\x39\xFF\x30\x43\xBD\x3E\x07\x54\xF1\x96\x2F\xFA\xCD\x81\x36\xD6\x5C\x2B\xFA\xF1\x40\xCF\xEC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x01\x47\x58\x90\x97\xAF\x28\xF1\xD7\x4C\x76\x63\xC2\x97\x2D\xD8\xA0\xB7\x60\x84\x2D\xAD\x2D\xE1\xC8\xE7\x43\xCB\x5E\x69\x85\xB3\x00\x00\x00\x00\x00\x00\x00\x00\x01\xFE\x86\x00\x01\x01\x01\x01\x28\x30\xFE\x20\x17\xD8\x15\x62\xE2\x30\xB4\x42\x4E\x9E\xE6\x1A\xF3\x13\x6E\x47\x6D\xD8\x19\xEC\xED\xCC\x67\xDE\x90\x55\x7E\x6C\xA4\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFE\x93\x01\x30\x21\x33\xCA\x8C\x49\x65\x39\xCF\x9A\x7B\xD7\x35\x44\x23\xFA\xED\x24\xBF\xAA\x0E\x48\x7A\x60\xFB\xCB\x34\x34\x59\xF6\xAC\x4B\xA2\xE6\xEA\x0C\xA8\xAC\x02\x00\x00\xBA\xD0\xEF\xBC\xBB\x76\x2B\x7C\xDF\x0F\x39\x10\x04\x39\xE8\x5A\xED\x14\x4D\xE8\x25\x35\x40\x52\x49\xEA\x79\xAA\xAD\x14\xEF\x9B\x37\xB4\x40\x3F\xAE\x01\x00\x00\x30\x1B\x1C\x76\x25\xA3\x63\xA6\xDC\x4A\x87\xDF\xEF\x6B\x5E\x2A\x3A\x11\xEB\xF4\xE9\xDC\x8F\x3E\x63\xA3\x4C\xC2\x48\xB6\xD4\x0B\xF1\x84\x52\x8F\x8D\xDE\x00\x00\x00\x53\x64\xD8\x44\x9B\x93\xD7\x8B\xCB\xD0\xE2\x7C\x02\xF5\xAF\xAC\x37\x16\xC9\x93\xB8\xB8\x85\x8A\xE3\x24\xB4\xDB\x26\x06\x31\x15\xFE\xD7\x40\x0C\x1D\x00\x00\x00\x38\x38\x64\x12\x64\x41\x00\x99\x14\x87\xAF\x12\x27\x24\x58\x63\x86\x13\x8F\xA1\xDA\x0F\xEF\x27\x64\x8E\xEE\x5D\x23\x08\xB0\xD5\x70\x2B\x2A\x55\x4E\x01\x00\x00\x50\x7E\xC5\x77\xF5\x93\x47\xBA\x70\x27\x65\xAE\x12\x9F\xA3\x15\x5D\x6B\x0D\x74\x6D\x5A\x93\x59\xE2\x14\x55\xEB\x13\xD7\x97\x0E\xC3\xBA\x6C\xBA\x17\x03\x00\x00\xC8\x9B\x63\xC1\xD8\x0F\x06\xD9\x9D\x3D\x61\x32\x4C\x45\x07\xD5\xE1\x61\x4D\x1B\x95\x78\x09\xAA\x4E\x25\x77\xF6\x4E\x64\x92\x8F\x64\x04\xFE\x58\x41\x01\x00\x00\x7C\xD7\x1C\x2B\x44\x91\xCB\x3F\x3B\x2D\xB4\x55\xBB\x94\x2A\x2B\x99\xCC\x30\xA7\xE3\xA1\x34\xB9\xB0\x94\x23\x5B\x17\xA3\xE0\x1C\x3B\x5D\x0E\xB2\x6A\x03\x00\x00\x30\xA8\xAE\x7E\xDF\xCB\xFC\xCE\x94\xE4\xFF\x8D\x14\x7C\xD4\x1B\x10\x34\xF4\x89\xDE\xC6\x44\xB7\xAF\xBA\x4C\xE4\xFC\x3D\x04\x8C\xF0\xBC\x9C\x39\x15\xB8\x01\x00\x00\x8E\x54\x6B\xB6\xDD\xF6\x1F\xE4\xAC\x46\x8C\x17\x48\xD6\x97\x77\x1E\x07\x8B\x96\x1C\x48\xB3\xC5\xD0\x49\xCE\x69\x16\x1E\x14\xFE\xD2\x48\x7A\x9E\x5D\x01\x00\x00\x01\x01\x01\x01\x00\x02\x01\x01\x01\x28\xFB\x02\xE2\xB6\xA1\x56\x87\x67\x8E\xF9\x3E\x61\xDA\x85\x68\x01\x4B\x83\x29\x81\x31\xC8\x7F\x16\xF4\x16\xAD\x09\x36\xB5\xB3\xCC\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x41\xB5\xAF\x2A\xD7\xC8\x70\x81\x31\x29\xEC\xC7\x81\xC1\xF0\x52\xEE\x8F\xDC\x85\x0C\xAA\xB7\x78\x15\x8E\x77\xFD\x21\x27\x31\x21\x00\x00\x00\x00\x00\x00\x00\x00\x01\x50\x01\x01\x01\x28\xDB\x12\xCC\xA7\xD9\x04\x53\x6D\x8C\xCB\x17\x13\xCC\x44\x54\xA4\x3E\x9A\xF4\xA2\xD6\x66\x8E\xE7\x58\x41\xE2\xD7\x3E\x6D\xEE\x69\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x28\xA5\x95\x95\x56\x6B\x02\xEA\xF4\x80\x59\xF5\xEC\xEE\x93\x9C\x6C\xDB\xB9\x63\x9D\x75\x85\x89\x64\xF8\xEF\x9F\xE1\x79\xE4\xEA\x88\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x00\x01\x00\x01\x31\x25\xE8\x31\x01\x01\x20\x0B\xE0\xAB\xAA\x7C\x06\x56\xB6\x23\x54\x3A\xE7\x40\x3C\xD7\xAD\x98\x79\x7C\xED\x14\x02\x7E\xD2\x53\x74\x15\xC2\xA8\xCA\x48\x18\x01\x01\x01\x28\x41\xB5\xAF\x2A\xD7\xC8\x70\x81\x31\x29\xEC\xC7\x81\xC1\xF0\x52\xEE\x8F\xDC\x85\x0C\xAA\xB7\x78\x15\x8E\x77\xFD\x21\x27\x31\x21\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x41\xCE\xAF\xB2\x49\x11\x53\x3D\xB3\xB8\xD2\x2D\x72\xB3\x99\x48\xA2\x3A\xF2\x79\xEF\x9C\x90\x06\x35\xE0\x7A\xD4\xFD\xF4\xAE\x59\x00\x00\x00\x00\x00\x00\x00\x00\x01\x14\x01\x01\x01\x28\x16\xBF\xE1\xB1\xE4\x02\x45\x6A\x0A\x1A\xBD\xEA\xAF\x3D\xA1\x30\x42\xD3\x5A\x90\x7A\x86\xA8\x4F\x7A\x6C\x81\x80\x93\x8D\xEB\xCA\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x28\xDD\x88\x7D\x74\x09\xDD\x84\xA3\x7F\x69\x2A\x18\xD1\x18\xB5\xB8\x1D\xED\x08\x22\x73\xA7\x54\x64\x86\xBF\x62\x6B\xCD\x74\x8E\x70\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x00\x01\x00\x01\x31\x25\xE8\x31\x01\x01\x20\x0B\xE0\xAB\xAA\x7C\x06\x56\xB6\x23\x54\x3A\xE7\x40\x3C\xD7\xAD\x98\x79\x7C\xED\x14\x02\x7E\xD2\x53\x74\x15\xC2\xA8\xCA\x48\x18\x01\x28\x10\x4C\x28\xBB\x53\x4D\xE2\xCF\x2D\xB1\x30\x94\xEC\xFF\x89\xEC\x0F\x40\x69\x4D\x43\xA5\x18\x4E\xF2\x14\x90\x97\xA3\x7B\xB2\x1F\xF3\x9A\xAC\xDC\x9D\x02\x00\x00\x01\x00\x01\x00\x00\x01\x01\x01\x28\xA6\x8C\xD9\xAF\x3C\xEB\x66\x05\x00\xEE\x71\x3A\xE4\x75\x21\xAB\xBF\xCF\xAF\x18\xAF\x6A\xEA\xED\x53\xA4\xAD\xAF\xCA\x7A\x87\x67\x00\x00\x00\x00\x00\x00\x00\x00\x01\x21\xE5\xFB\x67\x88\x04\xCC\xFB\x49\x66\x7B\xF5\x29\xCA\xCC\x74\xD1\xEA\xBE\x3F\xAB\x67\xC5\x33\x92\x1E\x4A\x81\x82\x29\x1F\xF0\xF5\x37\x01\x20\x4F\x97\xF2\xEE\xBF\x92\xCD\xE5\x8C\x10\x34\x66\x71\x2F\xA2\xF6\x5B\x10\xD0\x6F\xF8\xF1\x93\x4D\x78\xFF\x59\x2F\xA0\x57\x5E\x27\x01\x01\x28\xEB\x8D\x51\x8C\xC5\x27\x97\xA3\x69\xF8\xCA\x58\x43\xA8\xF0\x55\x20\x38\x9B\x22\xEE\xE6\x73\x1A\x7D\x60\xE3\x10\x91\x04\x79\xB3\x00\x00\x00\x00\x00\x00\x00\x00\x01\x28\x10\x4C\x28\xBB\x53\x4D\xE2\xCF\x2D\xB1\x30\x94\xEC\xFF\x89\xEC\x0F\x40\x69\x4D\x43\xA5\x18\x4E\xF2\x14\x90\x97\xA3\x7B\xB2\x1F\xF3\x9A\xAC\xDC\x9D\x02\x00\x00\x01"
in
  let buff = serialization_in_buffer known_good_serialization in
  let pos_ref = ref 0 in
  let _ : response = V1.bin_read_response buff ~pos_ref in
  true
     *)

    (* Message *)
  end )
