open Core_kernel

type ('field, 'bool) t =
  {field_elements: 'field array; bitstrings: 'bool list array}
[@@deriving sexp, compare]

let append t1 t2 =
  { field_elements= Array.append t1.field_elements t2.field_elements
  ; bitstrings= Array.append t1.bitstrings t2.bitstrings }

let field_elements x = {field_elements= x; bitstrings= [||]}

let field x = {field_elements= [|x|]; bitstrings= [||]}

let bitstring x = {field_elements= [||]; bitstrings= [|x|]}

let bitstrings x = {field_elements= [||]; bitstrings= x}

let pack_bits ~max_size ~pack {field_elements= _; bitstrings} =
  let rec pack_full_fields rev_fields bits length =
    if length >= max_size then
      let field_bits, bits = List.split_n bits max_size in
      pack_full_fields (pack field_bits :: rev_fields) bits (length - max_size)
    else (rev_fields, bits, length)
  in
  let packed_field_elements, remaining_bits, remaining_length =
    Array.fold bitstrings ~init:([], [], 0) ~f:(fun (acc, bits, n) bitstring ->
        let n = n + List.length bitstring in
        let bits = bits @ bitstring in
        let acc, bits, n = pack_full_fields acc bits n in
        (acc, bits, n) )
  in
  if remaining_length = 0 then packed_field_elements
  else pack remaining_bits :: packed_field_elements

let pack_to_fields ~size_in_bits ~pack {field_elements; bitstrings} =
  let max_size = size_in_bits - 1 in
  let packed_bits = pack_bits ~max_size ~pack {field_elements; bitstrings} in
  Array.append field_elements (Array.of_list_rev packed_bits)

let to_bits ~unpack {field_elements; bitstrings} =
  let field_bits = Array.map ~f:unpack field_elements in
  List.concat @@ Array.to_list @@ Array.append field_bits bitstrings

module Coding = struct
  (** See https://github.com/CodaProtocol/coda/blob/develop/rfcs/0038-rosetta-construction-api.md for details on schema *)

  (** Serialize a random oracle input with 32byte fields into bytes according to the RFC0038 specification *)
  let serialize ~string_of_field ~to_bool ~of_bool t =
    let len_to_string x =
      String.of_char_list
        Char.
          [ of_int_exn @@ ((x lsr 24) land 0xff)
          ; of_int_exn @@ ((x lsr 16) land 0xff)
          ; of_int_exn @@ ((x lsr 8) land 0xff)
          ; of_int_exn @@ (x land 0xff) ]
    in
    let len1 = len_to_string @@ Array.length t.field_elements in
    let fields =
      (* We only support 32byte fields *)
      let () =
        match t.field_elements with
        | [|x; _|] ->
            assert (String.length (string_of_field x) = 32)
        | _ ->
            ()
      in
      Array.map t.field_elements ~f:string_of_field |> String.concat_array
    in
    let len2 =
      len_to_string
      @@ Array.sum (module Int) t.bitstrings ~f:(fun x -> List.length x)
    in
    let packed =
      pack_bits t ~max_size:8 ~pack:(fun bs ->
          let rec go i acc = function
            | [] ->
                acc
            | b :: bs ->
                go (i + 1) ((acc * 2) + if to_bool b then 1 else 0) bs
          in
          let pad =
            List.init (8 - List.length bs) ~f:(Fn.const (of_bool false))
          in
          let combined = bs @ pad in
          assert (List.length combined = 8) ;
          go 0 0 combined )
      |> List.map ~f:Char.of_int_exn
      |> List.rev |> String.of_char_list
    in
    len1 ^ fields ^ len2 ^ packed

  module Parser = struct
    (* TODO: Before using this too much; use a solid parser library instead or beef this one up with more debugging info *)

    (* The parser is a function over this monad-fail *)
    module M = Result

    module T = struct
      type ('a, 'e) t = char list -> ('a * char list, 'e) M.t

      let return a cs = M.return (a, cs)

      let bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t =
       fun t ~f cs ->
        let open M.Let_syntax in
        let%bind a, rest = t cs in
        f a rest

      let map = `Define_using_bind
    end

    include Monad.Make2 (T)

    let run p cs =
      p cs
      |> M.bind ~f:(fun (a, cs') ->
             match cs' with [] -> M.return a | _ -> M.fail `Expected_eof )

    let fail why _ = M.fail why

    let char c = function
      | c' :: cs when Char.equal c c' ->
          M.return (c', cs)
      | c' :: _ ->
          M.fail (`Unexpected_char c')
      | [] ->
          M.fail `Unexpected_eof

    let u8 = function
      | c :: cs ->
          M.return (c, cs)
      | [] ->
          M.fail `Unexpected_eof

    let u32 =
      let open Let_syntax in
      let open Char in
      let%map a = u8 and b = u8 and c = u8 and d = u8 in
      (to_int a lsl 24) lor (to_int b lsl 16) lor (to_int c lsl 8) lor to_int d

    let eof = function [] -> M.return ((), []) | _ -> M.fail `Expected_eof

    let take n cs =
      if List.length cs < n then M.fail `Unexpected_eof
      else M.return (List.split_n cs n)

    (** p zero or more times, never fails *)
    let many p =
      (fun cs ->
        let rec go xs acc =
          match p xs with
          | Ok (a, xs) ->
              go xs (a :: acc)
          | Error _ ->
              (acc, xs)
        in
        M.return @@ go cs [] )
      |> map ~f:List.rev

    let%test_unit "many" =
      [%test_eq: (char list, [`Expected_eof]) Result.t]
        (run (many u8) ['a'; 'b'; 'c'])
        (Result.return ['a'; 'b'; 'c'])

    (** p exactly n times *)
    let exactly n p =
      (fun cs ->
        let rec go xs acc = function
          | 0 ->
              M.return (acc, xs)
          | i ->
              let open M.Let_syntax in
              let%bind a, xs = p xs in
              go xs (a :: acc) (i - 1)
        in
        go cs [] n )
      |> map ~f:List.rev

    let%test_unit "exactly" =
      [%test_eq:
        (char list * char list, [`Expected_eof | `Unexpected_eof]) Result.t]
        ((exactly 3 u8) ['a'; 'b'; 'c'; 'd'])
        (Result.return (['a'; 'b'; 'c'], ['d']))

    let return_res r cs = r |> Result.map ~f:(fun x -> (x, cs))
  end

  let bits_of_byte ~of_bool b =
    let b = Char.to_int b in
    let f x =
      of_bool
        ( match x with
        | 0 ->
            false
        | 1 ->
            true
        | _ ->
            failwith "Unexpected boolean integer" )
    in
    [ (b land (0x1 lsl 7)) lsr 7
    ; (b land (0x1 lsl 6)) lsr 6
    ; (b land (0x1 lsl 5)) lsr 5
    ; (b land (0x1 lsl 4)) lsr 4
    ; (b land (0x1 lsl 3)) lsr 3
    ; (b land (0x1 lsl 2)) lsr 2
    ; (b land (0x1 lsl 1)) lsr 1
    ; b land 0x1 ]
    |> List.map ~f

  (** Deserialize bytes into a random oracle input with 32byte fields according to the RFC0038 specification *)
  let deserialize ~field_of_string ~of_bool s =
    let field =
      let open Parser.Let_syntax in
      let%bind u8x32 = Parser.take 32 in
      let s = String.of_char_list u8x32 in
      Parser.return_res (field_of_string s)
    in
    let parser =
      let open Parser.Let_syntax in
      let%bind len1 = Parser.u32 in
      let%bind fields = Parser.exactly len1 field in
      let%bind len2 = Parser.u32 in
      let%map bytes = Parser.(many u8) in
      let bits = List.concat_map ~f:(bits_of_byte ~of_bool) bytes in
      let bitstring = List.take bits len2 in
      {field_elements= Array.of_list fields; bitstrings= [|bitstring|]}
    in
    Parser.run parser s

  (** String of field as bits *)
  let string_of_field xs =
    List.chunks_of xs ~length:8
    |> List.map ~f:(fun xs ->
           let rec go i acc = function
             | [] ->
                 acc
             | b :: bs ->
                 go (i + 1) ((acc * 2) + if b then 1 else 0) bs
           in
           let pad = List.init (8 - List.length xs) ~f:(Fn.const false) in
           let combined = xs @ pad in
           assert (List.length combined = 8) ;
           go 0 0 combined )
    |> List.map ~f:Char.of_int_exn
    |> String.of_char_list

  (** Field of string as bits *)
  let field_of_string s ~size_in_bits =
    List.concat_map (String.to_list s) ~f:(bits_of_byte ~of_bool:Fn.id)
    |> Fn.flip List.take size_in_bits
    |> Result.return
end

let%test_module "random_oracle input" =
  ( module struct
    let gen_input ?size_in_bits () =
      let open Quickcheck.Generator in
      let open Let_syntax in
      let%bind size_in_bits =
        size_in_bits |> Option.map ~f:return
        |> Option.value ~default:(Int.gen_incl 2 3000)
      in
      let%bind field_elements =
        (* Treat a field as a list of bools of length [size_in_bits]. *)
        list (list_with_length size_in_bits bool)
      in
      let%map bitstrings = list (list bool) in
      ( size_in_bits
      , { field_elements= Array.of_list field_elements
        ; bitstrings= Array.of_list bitstrings } )

    let%test_unit "field/string partial isomorphism bitstrings" =
      Quickcheck.test ~trials:300
        Quickcheck.Generator.(list_with_length 255 bool)
        ~f:(fun input ->
          let serialized = Coding.string_of_field input in
          let deserialized =
            Coding.field_of_string serialized ~size_in_bits:255
          in
          [%test_eq: (bool list, unit) Result.t] (input |> Result.return)
            deserialized )

    let%test_unit "serialize/deserialize partial isomorphism 32byte fields" =
      let size_in_bits = 255 in
      Quickcheck.test ~trials:3000 (gen_input ~size_in_bits ())
        ~f:(fun (_, input) ->
          let serialized =
            Coding.(
              serialize ~string_of_field ~to_bool:Fn.id ~of_bool:Fn.id input)
          in
          let deserialized =
            Coding.(
              deserialize
                (String.to_list serialized)
                ~field_of_string:(field_of_string ~size_in_bits)
                ~of_bool:Fn.id)
          in
          let normalized t =
            { t with
              bitstrings=
                ( t.bitstrings |> Array.to_list |> List.concat
                |> fun xs -> [|xs|] ) }
          in
          assert (
            Array.for_all input.field_elements ~f:(fun el ->
                List.length el = size_in_bits ) ) ;
          Result.iter deserialized ~f:(fun x ->
              assert (
                Array.for_all x.field_elements ~f:(fun el ->
                    List.length el = size_in_bits ) ) ) ;
          [%test_eq:
            ((bool list, bool) t, [`Expected_eof | `Unexpected_eof]) Result.t]
            (normalized input |> Result.return)
            (deserialized |> Result.map ~f:normalized) )

    let%test_unit "data is preserved by to_bits" =
      Quickcheck.test ~trials:300 (gen_input ())
        ~f:(fun (size_in_bits, input) ->
          let bits = to_bits ~unpack:Fn.id input in
          let bools_equal = [%equal: bool list] in
          (* Fields are accumulated at the front, check them first. *)
          let bitstring_bits =
            Array.fold ~init:bits input.field_elements ~f:(fun bits field ->
                (* The next chunk of [size_in_bits] bits is for the field
                         element.
                  *)
                let field_bits, rest = List.split_n bits size_in_bits in
                assert (bools_equal field_bits field) ;
                rest )
          in
          (* Bits come after. *)
          let remaining_bits =
            Array.fold ~init:bitstring_bits input.bitstrings
              ~f:(fun bits bitstring ->
                (* The next bits match the bitstring. *)
                let bitstring_bits, rest =
                  List.split_n bits (List.length bitstring)
                in
                assert (bools_equal bitstring_bits bitstring) ;
                rest )
          in
          (* All bits should have been consumed. *)
          assert (List.is_empty remaining_bits) )

    let%test_unit "data is preserved by pack_to_fields" =
      Quickcheck.test ~trials:300 (gen_input ())
        ~f:(fun (size_in_bits, input) ->
          let fields = pack_to_fields ~size_in_bits ~pack:Fn.id input in
          (* Fields are accumulated at the front, check them first. *)
          let fields = Array.to_list fields in
          let bitstring_fields =
            Array.fold ~init:fields input.field_elements
              ~f:(fun fields input_field ->
                (* The next field element should be the literal field element
                                   passed in.
                    *)
                match fields with
                | [] ->
                    failwith "Too few field elements"
                | field :: rest ->
                    assert ([%equal: bool list] field input_field) ;
                    rest )
          in
          (* Check that the remaining fields have the correct size. *)
          let final_field_idx = List.length bitstring_fields - 1 in
          List.iteri bitstring_fields ~f:(fun i field_bits ->
              if i < final_field_idx then
                (* This field should be densely packed, but should contain
                       fewer bits than the maximum field element to ensure that it
                       doesn't overflow, so we expect [size_in_bits - 1] bits for
                       maximum safe density.
                  *)
                assert (List.length field_bits = size_in_bits - 1)
              else (
                (* This field will be comprised of the remaining bits, up to a
                       maximum of [size_in_bits - 1]. It should not be empty.
                  *)
                assert (not (List.is_empty field_bits)) ;
                assert (List.length field_bits < size_in_bits) ) ) ;
          let rec go input_bitstrings packed_fields =
            match (input_bitstrings, packed_fields) with
            | [], [] ->
                (* We have consumed all bitstrings and fields in parallel, with
                   no bits left over. Success.
                *)
                ()
            | [] :: input_bitstrings, packed_fields
            | input_bitstrings, [] :: packed_fields ->
                (* We have consumed the whole of an input bitstring or the whole
                   of a packed field, move onto the next one.
                *)
                go input_bitstrings packed_fields
            | ( (bi :: input_bitstring) :: input_bitstrings
              , (bp :: packed_field) :: packed_fields ) ->
                (* Consume the next bit from the next input bitstring, and the
                   next bit from the next packed field. They must match.
                *)
                assert (Bool.equal bi bp) ;
                go
                  (input_bitstring :: input_bitstrings)
                  (packed_field :: packed_fields)
            | [], _ ->
                failwith "Packed fields contain more bits than were provided"
            | _, [] ->
                failwith
                  "There are input bits that were not present in the packed \
                   fields"
          in
          (* Check that the bits match between the input bitstring and the
                 remaining fields.
            *)
          go (Array.to_list input.bitstrings) bitstring_fields )
  end )
