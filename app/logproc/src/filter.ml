open Core_kernel

type sexp_expr =
  | Attribute of string
  | Int of int
  | String of string
  | Sexp of Sexp.t
  | Null

type t =
  | And of t * t
  | Or of t * t
  | Not of t
  | Sexp_equal of sexp_expr * sexp_expr
  | True
  | False

let eval_sexp_expr e (m : Logger.Message.t) =
  match e with
  | Null -> None
  | Attribute s -> Map.find m.attributes s
  | Int n -> Some ([%sexp_of: int] n)
  | String s -> Some ([%sexp_of: string] s)
  | Sexp s -> Some s

let rec eval t (m : Logger.Message.t) =
  match t with
  | True -> true
  | False -> false
  | Not t' -> not (eval t' m)
  | And (t1, t2) -> eval t1 m && eval t2 m
  | Or (t1, t2) -> eval t1 m || eval t2 m
  | Sexp_equal (e1, e2) ->
    eval_sexp_expr e1 m = eval_sexp_expr e2 m
