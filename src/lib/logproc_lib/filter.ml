open Core_kernel

module Ast = struct
  type value = Bool of bool | String of string | Int of int

  let bool x = Bool x

  let string x = String x

  let int x = Int x

  type value_exp =
    | Value_lit of value
    | Value_this
    | Value_list of value_exp list
    | Value_access_string of value_exp * string
    | Value_access_int of value_exp * int

  let value_lit x = Value_lit x

  let value_this = Value_this

  let value_list x = Value_list x

  let value_access_string x y = Value_access_string (x, y)

  let value_access_int x y = Value_access_int (x, y)

  type cmp_exp =
    | Cmp_eq of value_exp * value_exp
    | Cmp_neq of value_exp * value_exp
    | Cmp_in of value_exp * value_exp
    | Cmp_match of value_exp * Re2.regex

  let cmp_eq x y = Cmp_eq (x, y)

  let cmp_neq x y = Cmp_neq (x, y)

  let cmp_in x y = Cmp_in (x, y)

  let cmp_match x y = Cmp_match (x, y)

  type bool_exp =
    | Bool_lit of bool
    | Bool_cmp of cmp_exp
    | Bool_not of bool_exp
    | Bool_and of bool_exp * bool_exp
    | Bool_or of bool_exp * bool_exp

  let bool_lit x = Bool_lit x

  let bool_cmp x = Bool_cmp x

  let bool_not x = Bool_not x

  let bool_and x y = Bool_and (x, y)

  let bool_or x y = Bool_or (x, y)

  type t = bool_exp
end

module Parser = struct
  open Angstrom

  let is_whitespace = function ' ' | '\n' -> true | _ -> false

  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

  let is_numeric = function '0' .. '9' -> true | _ -> false

  let is_text = function
    | ' '
    | '\t'
    | '\n'
    | '\r'
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '('
    | ')'
    | '*'
    | '+'
    | ','
    | '-'
    | '.'
    | '/'
    | ':'
    | ';'
    | '<'
    | '='
    | '>'
    | '?'
    | '@'
    | '['
    | '\\'
    | ']'
    | '^'
    | '_'
    | '`'
    | '{'
    | '|'
    | '}'
    | '~'
    | '0' .. '9'
    | 'A' .. 'Z'
    | 'a' .. 'z' ->
        true
    | _ ->
        false

  let parens = (char '(', char ')')

  let brackets = (char '[', char ']')

  let ws = skip_while is_whitespace

  (* let ws1 = satisfy is_whitespace *> ws *)
  let alpha_char = satisfy is_alpha

  let numeric_char = satisfy is_numeric

  let text_char = satisfy is_text

  let pad b p = b *> p <* b

  let wrap (l, r) p = l *> p <* r

  let maybe p = p >>| Option.some <|> return None

  (* string w/ commit on first char *)
  let stringc s =
    assert (String.length s > 0) ;
    let h = s.[0] in
    let t = String.sub s ~pos:1 ~len:(String.length s - 1) in
    lift2 (fun h' t' -> String.of_char h' ^ t') (char h <* commit) (string t)

  let infix p op =
    p
    >>= fun l ->
    maybe (op <* commit) >>= function Some f -> p >>| f l | None -> return l

  let bool =
    choice [string "true" *> return true; string "false" *> return false]
    <?> "bool"

  let int = take_while1 is_numeric >>| int_of_string <?> "int"

  let text_escape =
    choice (List.map ~f:char ['"'; '\\'; '/'; 'b'; 'n'; 'r'; 't'])
    >>| fun c -> String.of_char_list ['\\'; c]

  let text_component =
    char '\\' *> text_escape <|> (text_char >>| String.of_char)

  let text = many text_component >>| String.concat ~sep:"" <?> "text"

  (* let text1 = many1 text_component >>| String.concat ~sep:"" *)

  let ident =
    lift2
      (fun h t -> String.of_char_list (h :: t))
      alpha_char
      (many (alpha_char <|> numeric_char))
    <?> "ident"

  let str = pad (char '"') text <?> "str"

  let literal =
    choice [bool >>| Ast.bool; int >>| Ast.int; str >>| Ast.string]
    <?> "literal"

  let value_exp =
    fix (fun value_exp ->
        let base =
          choice
            [ literal >>| Ast.value_lit
            ; wrap brackets (pad ws (sep_by (pad ws (char ',')) value_exp))
              >>| Ast.value_list ]
        in
        let rec access parent =
          choice
            [ char '.' *> (ident <|> wrap brackets (pad ws str))
              >>| Ast.value_access_string parent
            ; wrap brackets (pad ws int) >>| Ast.value_access_int parent ]
          >>= fun parent' -> access parent' <|> return parent'
        in
        maybe base
        >>= function
        | Some base ->
            access base <|> return base
        | None ->
            access Ast.value_this )
    <?> "value_exp"

  let cmp_exp =
    let regex =
      let inner =
        fix (fun inner ->
            choice
              [ lift2 List.cons (string {|\/|}) inner
              ; char '/' *> return []
              ; lift2 List.cons (take 1) inner ] )
        >>| String.concat ~sep:""
      in
      char '/' *> commit *> inner
      <* commit >>| (* TODO: handle gracefully *) Re2.create_exn
    in
    lift2
      (fun value f -> f value)
      (value_exp <* commit)
      (choice
         [ pad ws (stringc "==") *> value_exp >>| Fn.flip Ast.cmp_eq
         ; pad ws (stringc "!=") *> value_exp >>| Fn.flip Ast.cmp_neq
         ; pad ws (stringc "in") *> value_exp >>| Fn.flip Ast.cmp_in
         ; pad ws (stringc "match") *> regex >>| Fn.flip Ast.cmp_match ])
    <* commit <?> "cmp_exp"

  let bool_exp =
    fix (fun bool_exp ->
        let main =
          choice
            [ wrap parens (pad ws bool_exp)
            ; bool >>| Ast.bool_lit
            ; char '!' *> ws *> bool_exp >>| Ast.bool_not
            ; cmp_exp >>| Ast.bool_cmp ]
        in
        let infix_op =
          choice
            [ stringc "&&" *> return Ast.bool_and
            ; stringc "||" *> return Ast.bool_or ]
        in
        infix main (pad ws infix_op) )
    <?> "bool_exp"

  let parser = ws *> bool_exp <* ws <* end_of_input

  let parse str =
    Result.map_error (parse_string parser str) ~f:(fun err ->
        let msg =
          match err with
          | ": end_of_input" ->
              "expected end of input, found more characters"
          | _ ->
              err
        in
        sprintf "invalid syntax (%s)" msg )
end

module Interpreter = struct
  open Ast
  open Option.Let_syntax

  let option_list_map ls ~f =
    let rec loop acc = function
      | [] ->
          Some acc
      | h :: t ->
          let%bind el = f h in
          loop (el :: acc) t
    in
    loop [] ls >>| List.rev

  let json_value = function
    | Bool b ->
        `Bool b
    | String s ->
        `String s
    | Int i ->
        `Int i

  let access_string json str =
    match json with
    | `Assoc ls ->
        List.Assoc.find ~equal:String.equal ls str
    | _ ->
        None

  let access_int json i =
    match json with `List ls -> List.nth ls i | _ -> None

  let rec interpret_value_exp (json : Yojson.Safe.t) = function
    | Value_lit v ->
        Some (json_value v)
    | Value_list ls ->
        let%map ls' = option_list_map ls ~f:(interpret_value_exp json) in
        `List ls'
    | Value_this ->
        Some json
    | Value_access_string (parent, s) ->
        interpret_value_exp json parent >>= (Fn.flip access_string) s
    | Value_access_int (parent, i) ->
        interpret_value_exp json parent >>= (Fn.flip access_int) i

  let interpret_cmp_exp json = function
    | Cmp_eq (x, y) ->
        Option.map2
          (interpret_value_exp json x)
          (interpret_value_exp json y)
          ~f:Yojson.Safe.equal
        |> Option.value ~default:false
    | Cmp_neq (x, y) ->
        Option.map2 (interpret_value_exp json x) (interpret_value_exp json y)
          ~f:(fun json1 json2 -> not (Yojson.Safe.equal json1 json2))
        |> Option.value ~default:false
    | Cmp_in (x, y) ->
        Option.map2 (interpret_value_exp json x) (interpret_value_exp json y)
          ~f:(fun scalar list ->
            match list with
            | `List items ->
                List.exists items ~f:(Yojson.Safe.equal scalar)
            | _ ->
                (* TODO: filter warnings *) false )
        |> Option.value ~default:false
    | Cmp_match (x, regex) ->
        Option.map (interpret_value_exp json x) ~f:(fun value ->
            match value with
            | `String str ->
                Re2.matches regex str
            | _ ->
                false )
        |> Option.value ~default:false

  let rec interpret_bool_exp json = function
    | Bool_lit b ->
        b
    | Bool_cmp cmp ->
        interpret_cmp_exp json cmp
    | Bool_not x ->
        not (interpret_bool_exp json x)
    | Bool_and (x, y) ->
        interpret_bool_exp json x && interpret_bool_exp json y
    | Bool_or (x, y) ->
        interpret_bool_exp json x || interpret_bool_exp json y

  let matches filter json = interpret_bool_exp json filter
end

let%test_module "filter tests" =
  ( module struct
    let test ~filter ~data ~expect =
      let filter' = Result.ok_or_failwith (Parser.parse filter) in
      let data' = Yojson.Safe.from_string data in
      [%test_result: bool] ~expect (Interpreter.matches filter' data')

    let%test_unit "negation + in" =
      let filter = {|! .source.module in ["a", "c"]|} in
      test ~filter ~data:{|{ "source": { "module": "b" }}|} ~expect:true ;
      test ~filter ~data:{|{ "source": { "module": "a" }}|} ~expect:false
  end )
