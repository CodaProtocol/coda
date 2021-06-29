open Core

type t = Left | Right [@@deriving sexp, equal]

let of_bool = function false -> Left | true -> Right

let map ~left ~right = function Left -> left | Right -> right

let to_bool = map ~left:false ~right:true

let to_int = map ~left:0 ~right:1

let of_int = function 0 -> Some Left | 1 -> Some Right | _ -> None

let of_int_exn value =
  of_int value
  |> Option.value_exn
       ~message:(sprintf "Cannot convert integer %d into a direction" value)

let flip = map ~left:Right ~right:Left

let gen = Quickcheck.Let_syntax.(Quickcheck.Generator.bool >>| of_bool)

let gen_var_length_list ?(start = 0) depth =
  let open Quickcheck.Generator in
  Int.gen_incl start (depth - 1) >>= fun l -> list_with_length l gen

let gen_list depth = Quickcheck.Generator.list_with_length depth gen

let shrinker =
  Quickcheck.Shrinker.create (fun dir ->
      Sequence.unfold ~init:dir ~f:(function
        | Left ->
            None
        | Right ->
            Some (Left, Left) ) )
