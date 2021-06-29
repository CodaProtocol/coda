open Core
open Logproc_lib

let invalid_line_prefix = "!!! "

let find_timezone () =
  let ch = Unix.open_process_in {|date +"%z"|} in
  let input = Option.value_exn (In_channel.input_line ch) in
  In_channel.close ch ;
  Time.Zone.of_utc_offset ~hours:(int_of_string input / 100)

let format_timestamp ~timezone time =
  let date, of_day = Time.to_date_ofday ~zone:timezone time in
  sprintf "%d-%d-%d %s" (Date.year date)
    (Month.to_int (Date.month date))
    (Date.day date)
    (Time.Ofday.to_string of_day)

let level_color =
  let open Bash_colors in
  let open Logger.Level in
  function
  | Spam | Trace ->
      cyan
  | Debug ->
      green
  | Info ->
      magenta
  | Warn ->
      yellow
  | Error ->
      red
  | Faulty_peer ->
      orange
  | Fatal ->
      bright_red

let format_msg ~interpolation_config ~timezone msg =
  let open Logger.Message in
  (* TODO: interpolation/formatting options *)
  let message, extra =
    match
      Interpolator.interpolate interpolation_config msg.message msg.metadata
    with
    | Ok x ->
        x
    | Error err ->
        let open Bash_colors in
        printf
          !"%s===== Error interpolating message: %s%s\n%!"
          bright_red err none ;
        (msg.message, [])
  in
  ( match msg.source with
  | Some source ->
      printf !"%s[%s]%s: %s%s\n" (level_color msg.level)
        (format_timestamp ~timezone msg.timestamp)
        source.module_ message Bash_colors.none
  | None ->
      printf !"%s[%s]: %s%s\n" (level_color msg.level)
        (format_timestamp ~timezone msg.timestamp)
        message Bash_colors.none ) ;
  List.iter extra ~f:(fun (k, v) -> printf !"$%s = %s\n" k v) ;
  Out_channel.(flush stdout)

let yojson_from_string_result str =
  try Ok (Yojson.Safe.from_string str) with exn -> Error (Exn.to_string exn)

let process_line ~timezone ~interpolation_config ~filter line =
  let open Result.Let_syntax in
  match
    let%bind json = yojson_from_string_result line in
    let%map msg = Logger.Message.of_yojson json in
    (json, msg)
  with
  | Error _ ->
      printf !"%s%s\n%!" invalid_line_prefix line
  | Ok (json, msg) ->
      if Filter.Interpreter.matches filter json then
        format_msg ~timezone ~interpolation_config msg

type parser_state =
  | Start_of_line
  | Processable_line of {line_start_pos: int; previous_elements: string list}
  | Unprocessable_line of {line_start_pos: int}

(* Iterates over lines prefixed by `prefix`, invoking `on_hit` for each
 * line. If a line does not start with `prefix`, `on_miss` is invoked,
 * and the input for that line is streamed directly back out over stdout.
 *)
let iter_lines_prefixed_with input_channel output_channel ~prefix ~on_hit
    ~on_miss =
  (* We use 65536 because that is the size of OCaml's IO buffers. *)
  let max_buf_len = 65536 in
  let input_buffer = Bytes.create max_buf_len in
  let extract_input_string i j =
    let buf = Bytes.create (j - i) in
    Bytes.blit ~src:input_buffer ~src_pos:i ~dst:buf ~dst_pos:0 ~len:(j - i) ;
    Bytes.to_string buf
  in
  let forward_input i j =
    Out_channel.output output_channel ~buf:input_buffer ~pos:i ~len:(j - i)
  in
  let extract_and_process i state =
    match state with
    | Start_of_line ->
        failwith "cannot extract start of line state"
    | Processable_line {line_start_pos; previous_elements} ->
        let this_element = extract_input_string line_start_pos i in
        let elements = List.rev (this_element :: previous_elements) in
        on_hit (String.concat elements ~sep:"")
    | Unprocessable_line {line_start_pos} ->
        forward_input line_start_pos i ;
        Out_channel.output_char output_channel '\n'
  in
  let carryover_state ~buf_len state =
    match state with
    | Start_of_line ->
        Start_of_line
    | Processable_line {line_start_pos; previous_elements} ->
        let this_element = extract_input_string line_start_pos buf_len in
        Processable_line
          { line_start_pos= 0
          ; previous_elements= this_element :: previous_elements }
    | Unprocessable_line {line_start_pos} ->
        forward_input line_start_pos buf_len ;
        Unprocessable_line {line_start_pos= 0}
  in
  let parse_char ~buf_len i state =
    if i >= buf_len then `Stop (carryover_state ~buf_len state)
    else
      match state with
      | Start_of_line ->
          let next_state =
            if Char.equal (Bytes.unsafe_get input_buffer i) prefix then
              Processable_line {line_start_pos= i; previous_elements= []}
            else (
              on_miss () ;
              Unprocessable_line {line_start_pos= i} )
          in
          `Continue next_state
      | Processable_line _ | Unprocessable_line _ ->
          if Char.equal (Bytes.unsafe_get input_buffer i) '\n' then `Extract
          else `Continue state
  in
  let rec parse_loop ~buf_len i state =
    match parse_char ~buf_len i state with
    | `Stop final_state ->
        final_state
    | `Continue next_state ->
        parse_loop ~buf_len (i + 1) next_state
    | `Extract ->
        extract_and_process i state ;
        parse_loop ~buf_len (i + 1) Start_of_line
  in
  let rec read_loop state =
    let buf_len =
      In_channel.input input_channel ~buf:input_buffer ~pos:0 ~len:max_buf_len
    in
    if buf_len > 0 then (
      let next_state = parse_loop ~buf_len 0 state in
      Out_channel.flush output_channel ;
      read_loop next_state )
  in
  read_loop Start_of_line

(* TODO: check for common filter errors (e.g. invalid level provided) *)
let main timezone_str interpolation_config filter_str =
  let filter =
    if String.is_empty filter_str then
      Result.ok_or_failwith (Filter.Parser.parse "true")
    else
      let error s =
        eprintf !"ERROR PARSING FILTER: %s\n%!" s ;
        exit 1
      in
      match
        try Filter.Parser.parse filter_str
        with exn -> error (Exn.to_string exn)
      with
      | Ok x ->
          x
      | Error err ->
          error err
  in
  (* let filter = Result.ok_or_failwith (Filter.Parser.parse "true") in *)
  (* let filter = Result.ok_or_failwith (Filter.Parser.parse ".level === \"Info\"") in *)
  let timezone =
    if String.is_empty timezone_str then find_timezone ()
    else Time.Zone.of_string timezone_str
  in
  iter_lines_prefixed_with In_channel.stdin stdout ~prefix:'{'
    ~on_hit:(process_line ~timezone ~interpolation_config ~filter)
    ~on_miss:(fun () -> Out_channel.output_string stdout invalid_line_prefix)

let () =
  let open Cmdliner in
  (* default: inline *)
  (* -i --interpolation-mode: hidden, inline, after *)
  (* -m --max-interpolation-length *)
  (* -p --pretty-print *)
  let interpolation_config =
    let interpolation_mode =
      let open Interpolator in
      let doc =
        "Set how interpolation of messages will be performed (valid options \
         are \"hidden\", \"inline\" and  \"after\")"
      in
      Arg.(
        value
        & opt
            (enum [("hidden", Hidden); ("inline", Inline); ("after", After)])
            Inline
        & info ["i"; "interpolation-mode"] ~docv:"MODE" ~doc)
    in
    let max_interpolation_length =
      let doc =
        "Maximum allowed length of interpolated values. Values exceeding this \
         length will be replaced with \"(...)\"."
      in
      Arg.(
        value & opt int 25
        & info
            ["m"; "max-interpolation-length"]
            ~docv:"MAX_INTERPOLATION_LENGTH" ~doc)
    in
    let pretty_print =
      let doc = "Pretty print json values." in
      Arg.(value & flag & info ["p"; "pretty-print"] ~docv:"PRETTY_PRINT" ~doc)
    in
    let lift_interpolation_config mode max_interpolation_length pretty_print =
      let open Interpolator in
      {mode; max_interpolation_length; pretty_print}
    in
    Term.(
      const lift_interpolation_config
      $ interpolation_mode $ max_interpolation_length $ pretty_print)
  in
  let timezone =
    let doc =
      "Timezone to display timestamps in. Defaults to the system's timezone."
    in
    Arg.(value & opt string "" & info ["z"; "zone"] ~docv:"TIMEZONE" ~doc)
  in
  let filter =
    let doc =
      "Filter displayed log lines. The filter language has similar syntax to \
       javascript, with a few notable differences. Similar to \"jq\", doing \
       an anymous access like \"[\"a\"]\" or \".a\" will refer to that key on \
       the javascript object being processed, which in the context of \
       logproc, is the json log entry itself. Basic literals (such as strings \
       and integers) are supported, as is structural equality (\"==\") and \
       boolean operations (\"!\", \"&&\", \"||\"). There is also support for \
       the \"in\" operator for checking existence in arrays (works like \
       javascript, not like jq). Regexes can also be expressed \
       (\"/some_regex/\") and can be matched on using a special \"match\" \
       operator. See examples for more information."
    in
    Arg.(value & opt string "" & info ["f"; "filter"] ~docv:"FILTER" ~doc)
  in
  let main_term =
    Term.(const main $ timezone $ interpolation_config $ filter)
  in
  let main_info =
    let doc = "Process coda log statements from standard input" in
    let man =
      [ `S Manpage.s_description
      ; `P
          "Logproc processes coda logs from stdin and is capable of filtering \
           and reformating logs in a user friendly fashion. A javascript-like \
           filter language is included for defining boolean predicates on log \
           statements. Logproc will also attempt to interpolate logging \
           strings."
      ; `S Manpage.s_examples
      ; `I ("pretty print logs with default interpolation:", {|logproc|})
      ; `I ("pretty print logs and show all metadata:", {|logproc -i after|})
      ; `I
          ( "filter logs in specific levels:"
          , {|logproc -f '.level in ["Warn", "Error", "Faulty_peer"]|} )
      ; `I
          ( "filter all logs from Gossip_net:"
          , {|logproc -f '.source.module == "Gossip_net"'|} )
      ; `I
          ( "filter all logs from a specific pid:"
          , {|logproc -f '.metadata.pid == 1337'|} )
      ; `I
          ( "filter all logs matching a regex:"
          , {|logproc -f '.message match /[Vv]rf/'|} )
      ; `I
          ( "a complex filter:"
          , {|logproc -f '.message match /broadcast/ && .metadata.peer.host == "182.9.63.3" || .metadata.peer.discover_port == 8302|}
          ) ]
    in
    Term.info ~version:"0.1" ~doc ~exits:Term.default_exits ~man "logproc"
  in
  Term.(exit @@ eval (main_term, main_info))
