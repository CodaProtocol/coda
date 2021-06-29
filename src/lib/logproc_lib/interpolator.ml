open Core_kernel

type mode = Hidden | Inline | After

type config = {mode: mode; max_interpolation_length: int; pretty_print: bool}

let rec result_fold_left ls ~init ~f =
  match ls with
  | [] ->
      Ok init
  | h :: t -> (
    match f init h with
    | Ok init' ->
        result_fold_left t ~init:init' ~f
    | Error err ->
        Error err )

let parser =
  let open Angstrom in
  let not_f f x = not (f x) in
  let or_f f g x = f x || g x in
  let is_alpha = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
        true
    | _ ->
        false
  in
  let is_numeric = function '0' .. '9' -> true | _ -> false in
  let interpolation =
    lift2
      (fun c s -> String.of_char c ^ s)
      (char '$' *> commit *> satisfy is_alpha)
      (take_while (or_f is_alpha is_numeric))
  in
  let message =
    many1
      (choice
         [ (take_while1 (not_f (Char.equal '$')) >>| fun x -> `Raw x)
         ; (interpolation >>| fun x -> `Interpolate x) ])
  in
  message <* end_of_input

let parse = Angstrom.parse_string parser

(* map and concat vs. fold: which is better for strings? *)
let render ~max_interpolation_length ~format_json metadata items =
  let open Result.Let_syntax in
  let%map msg, extra =
    result_fold_left items ~init:("", []) ~f:(fun (msg_acc, extra_acc) el ->
        match el with
        | `Raw str ->
            Ok (msg_acc ^ str, extra_acc)
        | `Interpolate id ->
            let%map json =
              String.Map.find metadata id
              |> Result.of_option
                   ~error:(sprintf "bad interpolation for %s" id)
            in
            let str = format_json json in
            if String.length str > max_interpolation_length then
              (msg_acc ^ "$" ^ id, (id, str) :: extra_acc)
            else (msg_acc ^ str, extra_acc) )
  in
  (msg, List.rev extra)

let interpolate {mode; max_interpolation_length; pretty_print} msg metadata =
  let open Result.Let_syntax in
  let format_json =
    if pretty_print then Yojson.Safe.pretty_to_string
    else Yojson.Safe.to_string ?buf:None ?len:None
  in
  match mode with
  | Hidden ->
      Ok (msg, [])
  | Inline ->
      let%bind items = parse msg in
      render ~max_interpolation_length ~format_json metadata items
  | After ->
      Ok
        ( msg
        , List.map (String.Map.to_alist metadata) ~f:(fun (k, v) ->
              (k, format_json v) ) )
