open Core_kernel

(** The string that preceeds the JSON header, to identify the file kind before
    attempting to parse it.
*)
let header_string = "MINA_SNARK_KEYS\n"

module UInt64 = struct
  (* [Unsigned_extended] depends on pickles, manually include what we need here
     to break a dependency cycle

     TODO: Separate [Unsigned_extended] into snark and non-snark parts.
  *)
  type t = Unsigned.UInt64.t [@@deriving ord, equal]

  let to_yojson x = `String (Unsigned.UInt64.to_string x)

  let of_yojson = function
    | `String x ->
        Or_error.try_with (fun () -> Unsigned.UInt64.of_string x)
        |> Result.map_error ~f:(fun err ->
               sprintf
                 "Snark_keys_header.UInt64.of_yojson: Could not parse string \
                  as UInt64: %s"
                 (Error.to_string_hum err) )
    | _ ->
        Error "Snark_keys_header.UInt64.of_yojson: Expected a string"

  let sexp_of_t x = Sexp.Atom (Unsigned.UInt64.to_string x)

  let t_of_sexp = function
    | Sexp.Atom x ->
        Unsigned.UInt64.of_string x
    | _ ->
        failwith "Snark_keys_header.UInt64.t_of_sexp: Expected an atom"
end

module Kind = struct
  (** The 'kind' of data in the file.
    For example, a step proving key for the base transaction snark may have the
    kind:
{[
  {type_= "step_proving_key"; identifier= "transaction_snark_base"}
|}
  *)
  type t =
    { type_: string [@key "type"]
          (** Identifies the type of data that the file contains *)
    ; identifier: string
          (** Identifies the specific purpose of the file's data, in a
            human-readable format
        *)
    }
  [@@deriving yojson, sexp, ord, equal]
end

module Constraint_constants = struct
  module Transaction_capacity = struct
    (** Transaction pool capacity *)
    type t = Log_2 of int | Txns_per_second_x10 of int
    [@@deriving sexp, ord, equal]

    let to_yojson t : Yojson.Safe.t =
      match t with
      | Log_2 i ->
          `Assoc [("two_to_the", `Int i)]
      | Txns_per_second_x10 i ->
          `Assoc [("txns_per_second_x10", `Int i)]

    let of_yojson (json : Yojson.Safe.t) =
      match json with
      | `Assoc [("two_to_the", `Int i)] ->
          Ok (Log_2 i)
      | `Assoc [("txns_per_second_x10", `Int i)] ->
          Ok (Txns_per_second_x10 i)
      | `Assoc _ ->
          Error
            "Snark_keys_header.Constraint_constants.Transaction_capacity.of_yojson: \
             Expected a JSON object containing the field 'two_to_the' or \
             'txns_per_second_x10'"
      | _ ->
          Error
            "Snark_keys_header.Constraint_constants.Transaction_capacity.of_yojson: \
             Expected a JSON object"
  end

  module Fork_config = struct
    (** Fork data *)
    type t =
      { previous_state_hash: string
      ; previous_length: int
      ; previous_global_slot: int }
    [@@deriving yojson, sexp, ord, equal]

    let opt_to_yojson t : Yojson.Safe.t =
      match t with Some t -> to_yojson t | None -> `Assoc []

    let opt_of_yojson (json : Yojson.Safe.t) =
      match json with
      | `Assoc [] ->
          Ok None
      | _ ->
          Result.map (of_yojson json) ~f:(fun t -> Some t)
  end

  (** The constants used in the constraint system.  *)
  type t =
    { sub_windows_per_window: int
    ; ledger_depth: int
    ; work_delay: int
    ; block_window_duration_ms: int
    ; transaction_capacity: Transaction_capacity.t
    ; pending_coinbase_depth: int
    ; coinbase_amount: UInt64.t
    ; supercharged_coinbase_factor: int
    ; account_creation_fee: UInt64.t
    ; fork:
        (Fork_config.t option[@to_yojson Fork_config.opt_to_yojson]
                             [@of_yojson Fork_config.opt_of_yojson]) }
  [@@deriving yojson, sexp, ord, equal]
end

module Commits = struct
  (** Commit identifiers *)
  type t = {mina: string; marlin: string} [@@deriving yojson, sexp, ord, equal]
end

let header_version = 1

(** Header contents *)
type t =
  { header_version: int
  ; kind: Kind.t
  ; constraint_constants: Constraint_constants.t
  ; commits: Commits.t
  ; length: int
  ; commit_date: string
  ; constraint_system_hash: string
  ; identifying_hash: string }
[@@deriving yojson, sexp, ord, equal]

let prefix = "MINA_SNARK_KEYS\n"

let prefix_len = String.length prefix

let parse_prefix (lexbuf : Lexing.lexbuf) =
  let open Or_error.Let_syntax in
  Result.map_error ~f:(fun err ->
      Error.tag_arg err "Could not read prefix" ("prefix", prefix)
        [%sexp_of: string * string] )
  @@ Or_error.try_with_join (fun () ->
         (* This roughly mirrors the behavior of [Yojson.Safe.read_ident],
            except that we have a known fixed length to parse, and that it is a
            failure to read any string except the prefix. We manually update
            the lexbuf to be consistent with the output of this function.
         *)
         (* Manually step the lexbuffer forward to the [lex_curr_pos], so that
            [refill_buf] will know that we're only interested in buffer
            contents from that position onwards.
         *)
         lexbuf.lex_start_pos <- lexbuf.lex_curr_pos ;
         lexbuf.lex_last_pos <- lexbuf.lex_curr_pos ;
         lexbuf.lex_start_p <- lexbuf.lex_curr_p ;
         let%bind () =
           (* Read more if the buffer doesn't contain the whole prefix. *)
           if lexbuf.lex_buffer_len - lexbuf.lex_curr_pos >= prefix_len then
             return ()
           else if lexbuf.lex_eof_reached then
             Or_error.error_string "Unexpected end-of-file"
           else (
             lexbuf.refill_buff lexbuf ;
             if lexbuf.lex_buffer_len - lexbuf.lex_curr_pos >= prefix_len then
               return ()
             else if lexbuf.lex_eof_reached then
               Or_error.error_string "Unexpected end-of-file"
             else
               Or_error.error_string
                 "Unexpected short read: broken lexbuffer or end-of-file" )
         in
         let read_prefix =
           Lexing.sub_lexeme lexbuf lexbuf.lex_curr_pos
             (lexbuf.lex_curr_pos + prefix_len)
         in
         let%map () =
           if String.equal prefix read_prefix then return ()
           else
             Or_error.error "Incorrect prefix"
               ("read prefix", read_prefix)
               [%sexp_of: string * string]
         in
         (* Update the positions to match our end state *)
         lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + prefix_len ;
         lexbuf.lex_last_pos <- lexbuf.lex_last_pos ;
         lexbuf.lex_curr_p
         <- { lexbuf.lex_curr_p with
              pos_bol= lexbuf.lex_curr_p.pos_bol + prefix_len
            ; pos_cnum= lexbuf.lex_curr_p.pos_cnum + prefix_len } ;
         (* This matches the action given by [Yojson.Safe.read_ident]. *)
         lexbuf.lex_last_action <- 1 )

let parse_lexbuf (lexbuf : Lexing.lexbuf) =
  let open Or_error.Let_syntax in
  Result.map_error ~f:(Error.tag ~tag:"Failed to read snark key header")
  @@ let%bind () = parse_prefix lexbuf in
     Or_error.try_with (fun () ->
         let yojson_parsebuffer = Yojson.init_lexer () in
         (* We use [read_t] here rather than one of the alternatives to avoid
            'greedy' parsing that will attempt to continue and read the file's
            contents beyond the header.
         *)
         Yojson.Safe.read_t yojson_parsebuffer lexbuf )

let%test_module "Check parsing of header" =
  ( module struct
    let valid_header =
      { header_version= 1
      ; kind= {type_= "type"; identifier= "identifier"}
      ; constraint_constants=
          { sub_windows_per_window= 4
          ; ledger_depth= 8
          ; work_delay= 1000
          ; block_window_duration_ms= 1000
          ; transaction_capacity= Log_2 3
          ; pending_coinbase_depth= 12
          ; coinbase_amount= Unsigned.UInt64.of_int 1
          ; supercharged_coinbase_factor= 1
          ; account_creation_fee= Unsigned.UInt64.of_int 1
          ; fork= None }
      ; commits=
          { mina= "7e1fb2cd9138af1d0f24e78477efd40a2a0fcd07"
          ; marlin= "75836c41fc4947acce9c938da1b2f506843e90ed" }
      ; length= 4096
      ; commit_date= "2020-01-01 00:00:00.000000Z"
      ; constraint_system_hash= "ABCDEF1234567890"
      ; identifying_hash= "ABCDEF1234567890" }

    let valid_header_string = Yojson.Safe.to_string (to_yojson valid_header)

    let valid_header_with_prefix = prefix ^ valid_header_string

    module Tests (Lexing : sig
      val from_string : string -> Lexing.lexbuf
    end) =
    struct
      let%test "doesn't parse without prefix" =
        parse_lexbuf (Lexing.from_string valid_header_string)
        |> Or_error.is_error

      let%test "doesn't parse with incorrect prefix" =
        parse_lexbuf (Lexing.from_string ("BLAH" ^ valid_header_string))
        |> Or_error.is_error

      let%test "doesn't parse with matching-length prefix" =
        let fake_prefix = String.init prefix_len ~f:(fun _ -> 'a') in
        parse_lexbuf (Lexing.from_string (fake_prefix ^ valid_header_string))
        |> Or_error.is_error

      let%test "doesn't parse with partial matching prefix" =
        let partial_prefix =
          String.sub prefix ~pos:0 ~len:(prefix_len - 1) ^ " "
        in
        parse_lexbuf
          (Lexing.from_string (partial_prefix ^ valid_header_string))
        |> Or_error.is_error

      let%test "doesn't parse with short file" =
        parse_lexbuf (Lexing.from_string "BLAH") |> Or_error.is_error

      let%test "doesn't parse with prefix only" =
        parse_lexbuf (Lexing.from_string prefix) |> Or_error.is_error

      let%test_unit "parses valid header with prefix" =
        parse_lexbuf (Lexing.from_string valid_header_with_prefix)
        |> Or_error.ok_exn |> ignore

      let%test_unit "parses valid header with prefix and data" =
        parse_lexbuf
          (Lexing.from_string (valid_header_with_prefix ^ "DATADATADATA"))
        |> Or_error.ok_exn |> ignore
    end

    let%test_module "Parsing from the start of the lexbuf" =
      (module Tests (Lexing))

    let%test_module "Parsing from part-way through a lexbuf" =
      ( module struct
        include Tests (struct
          let from_string str =
            let prefix = "AAAAAAAAAA" in
            let prefix_len = String.length prefix in
            let lexbuf = Lexing.from_string (prefix ^ str) in
            lexbuf.lex_start_pos <- 0 ;
            lexbuf.lex_curr_pos <- prefix_len ;
            lexbuf.lex_last_pos <- prefix_len ;
            lexbuf
        end)
      end )

    let%test_module "Parsing with refill" =
      ( module struct
        include Tests (struct
          let from_string str =
            let init = ref true in
            let initial_prefix = "AAAAAAAAAA" in
            let initial_prefix_len = String.length initial_prefix in
            let offset = ref 0 in
            let str_len = String.length str in
            let lexbuf =
              Lexing.from_function (fun buffer length ->
                  match !init with
                  | true ->
                      init := false ;
                      (* Initial read: fill with junk up to the first character
                         of the actual prefix
                      *)
                      Bytes.From_string.blit ~src:initial_prefix ~src_pos:0
                        ~dst:buffer ~dst_pos:0 ~len:initial_prefix_len ;
                      Bytes.set buffer initial_prefix_len str.[0] ;
                      offset := 1 ;
                      initial_prefix_len + 1
                  | false ->
                      (* Subsequent read: fill the rest of the buffer. *)
                      let len = Int.min length (str_len - !offset) in
                      if len = 0 then 0
                      else (
                        Bytes.From_string.blit ~src:str ~src_pos:!offset
                          ~dst:buffer ~dst_pos:0 ~len ;
                        offset := !offset + len ;
                        len ) )
            in
            (* Load the initial content into the buffer *)
            lexbuf.refill_buff lexbuf ;
            lexbuf.lex_start_pos <- 0 ;
            lexbuf.lex_curr_pos <- initial_prefix_len ;
            lexbuf.lex_last_pos <- initial_prefix_len ;
            lexbuf
        end)
      end )
  end )

let write_with_header ~expected_max_size_log2 ~append_data header filename =
  (* In order to write the correct length here, we provide the maximum expected
     size and store that in the initial header. Once the data has been written,
     we record the length and then modify the 'length' field to hold the
     correct data.
     Happily, since the header is JSON-encoded, we can pad the calculated
     length with spaces and the header will still form a valid JSON-encoded
     object.
     This intuitively feels hacky, but the only way this can fail are if we are
     not able to write all of our data to the filesystem, or if the file is
     modified during the writing process. In either of these cases, we would
     have the same issue even if we were to pre-compute the length and do the
     write atomically.
  *)
  let length = 1 lsl expected_max_size_log2 in
  if length <= 0 then
    failwith
      "Snark_keys_header.write_header: expected_max_size_log2 is too large, \
       the resulting length underflows" ;
  let header_string = Yojson.Safe.to_string (to_yojson {header with length}) in
  (* We look for the "length" field first, to ensure that we find our length
     and not some other data that happens to match it. Due to the
     JSON-encoding, we will only find the first field named "length", which is
     the one that we want to modify.
  *)
  let length_offset =
    String.substr_index_exn header_string ~pattern:"\"length\":"
  in
  let length_string = string_of_int length in
  let length_data_offset =
    prefix_len
    + String.substr_index_exn ~pos:length_offset header_string
        ~pattern:length_string
  in
  (* We use [binary=true] to ensure that line endings aren't converted, so that
     files can be used regardless of the operating system that generated them.
  *)
  Out_channel.with_file ~binary:true filename ~f:(fun out_channel ->
      Out_channel.output_string out_channel prefix ;
      Out_channel.output_string out_channel header_string ;
      (* Newline, to allow [head -n 2 path/to/file | tail -n 1] to easily
         extract the header.
      *)
      Out_channel.output_char out_channel '\n' ) ;
  append_data filename ;
  (* Core doesn't let us open a file without appending or truncating, so we use
     stdlib instead.
  *)
  let out_channel =
    Stdlib.open_out_gen [Open_wronly; Open_binary] 0 filename
  in
  let true_length = Out_channel.length out_channel |> Int.of_int64_exn in
  if true_length > length then
    failwith
      "Snark_keys_header.write_header: 2^expected_max_size_log2 is less than \
       the true length of the file" ;
  let true_length_string = string_of_int true_length in
  let true_length_padding =
    String.init
      (String.length length_string - String.length true_length_string)
      ~f:(fun _ -> ' ')
  in
  (* Go to where we wrote the data *)
  Out_channel.seek out_channel (Int64.of_int length_data_offset) ;
  (* Pad with spaces *)
  Out_channel.output_string out_channel true_length_padding ;
  (* Output the true length *)
  Out_channel.output_string out_channel true_length_string ;
  Out_channel.close out_channel

let read_with_header ~read_data filename =
  let open Or_error.Let_syntax in
  Or_error.try_with_join (fun () ->
      (* We use [binary=true] to ensure that line endings aren't converted. *)
      let in_channel = In_channel.create ~binary:true filename in
      let file_length = In_channel.length in_channel |> Int.of_int64_exn in
      let lexbuf = Lexing.from_channel in_channel in
      let%bind header_json = parse_lexbuf lexbuf in
      let%bind header =
        of_yojson header_json |> Result.map_error ~f:Error.of_string
      in
      let offset = lexbuf.lex_curr_pos in
      let%bind () =
        In_channel.seek in_channel (Int64.of_int offset) ;
        match In_channel.input_char in_channel with
        | Some '\n' ->
            Ok ()
        | None ->
            Or_error.error_string
              "Incomplete header: the newline terminator is missing"
        | Some c ->
            Or_error.error "Header was not terminated by a newline character"
              ("character", c) [%sexp_of: string * char]
      in
      (* Bump offset for the newline terminator *)
      let offset = offset + 1 in
      In_channel.close in_channel ;
      let%bind () =
        if header.length = file_length then Ok ()
        else
          Or_error.error
            "Header length didn't match file length. Was the file only \
             partially downloaded?"
            (("header length", header.length), ("file length", file_length))
            [%sexp_of: (string * int) * (string * int)]
      in
      let%map data =
        Or_error.try_with (fun () -> read_data ~offset filename)
      in
      (header, data) )
