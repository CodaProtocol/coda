open Ppxlib
open Asttypes
open Parsetree
open Longident
open Core
open Crypto_params
open Signature_lib

let seed = "Coda_sample_keypairs"

let random_bool = Crs.create ~seed

module Group = Curve_choice.Tick_backend.Inner_curve

let bigint_of_bits bits =
  List.foldi bits ~init:Bigint.zero ~f:(fun i acc b ->
      if b then Bigint.(acc + (of_int 2 lsl i)) else acc )

let rec random_scalar () =
  let n =
    bigint_of_bits
      (List.init Tock0.Field.size_in_bits ~f:(fun _ -> random_bool ()))
  in
  if Bigint.(n < Tock0.Field.size) then
    Tock0.Bigint.(to_field (of_bignum_bigint n))
  else random_scalar ()

let keypairs =
  let n = 120 in
  List.cons
    (* FIXME #2936: remove this "precomputed VRF keypair" *)
    (* This key is also at the start of all the release ledgers. It's needed to generate a valid genesis transition *)
    (Keypair.of_private_key_exn
       (Private_key.of_base58_check_exn
          "2BRrzmLJrktDq33yDkAj6emp3gLwWbbCgkhKvDUUPRj49DDBAQfuyfWrdnqUv3DApQvYjsGgyhHLadYiX1zMuebqWqA9xbHJKxRJWxSxr3ZJyjCc7Fxtm4N1zBkcXhfTb1ffVjTvU6b2"))
    (List.init n ~f:(fun _ ->
         let sk = random_scalar () in
         Keypair.of_private_key_exn sk ))

let expr ~loc =
  let module E = Ppxlib.Ast_builder.Make (struct
    let loc = loc
  end) in
  let open E in
  let earray =
    E.pexp_array
      (List.map keypairs ~f:(fun {public_key; private_key} ->
           E.pexp_tuple
             [ estring
                 (Binable.to_string
                    (module Public_key.Compressed.Stable.Latest)
                    (Public_key.compress public_key))
             ; estring
                 (Binable.to_string
                    (module Private_key.Stable.Latest)
                    private_key) ] ))
  in
  let%expr conv (pk, sk) =
    ( Core_kernel.Binable.of_string
        (module Signature_lib.Public_key.Compressed.Stable.Latest)
        pk
    , Core_kernel.Binable.of_string
        (module Signature_lib.Private_key.Stable.Latest)
        sk )
  in
  Array.map conv [%e earray]

let structure ~loc =
  let module E = Ppxlib.Ast_builder.Make (struct
    let loc = loc
  end) in
  let open E in
  [%str let keypairs = lazy [%e expr ~loc]]

let json =
  `List
    (List.map keypairs ~f:(fun kp ->
         `Assoc
           [ ("public_key", `String (Public_key.to_base58_check kp.public_key))
           ; ( "private_key"
             , `String (Private_key.to_base58_check kp.private_key) ) ] ))

let main () =
  Out_channel.with_file "sample_keypairs.ml" ~f:(fun ml_file ->
      let fmt = Format.formatter_of_out_channel ml_file in
      Pprintast.top_phrase fmt (Ptop_def (structure ~loc:Ppxlib.Location.none))
  ) ;
  Out_channel.with_file "sample_keypairs.json" ~f:(fun json_file ->
      Yojson.pretty_to_channel json_file json ) ;
  exit 0

let () = main ()
