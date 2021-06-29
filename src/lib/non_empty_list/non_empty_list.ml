open Core_kernel

(* A non-empty list is a tuple of the head and the rest (as a list) *)
[%%versioned
module Stable = struct
  module V1 = struct
    type 'a t = 'a * 'a list [@@deriving sexp, compare, equal, hash]
  end
end]

let init x xs = (x, xs)

let singleton x = (x, [])

let uncons = Fn.id

let cons x' (x, xs) = (x', x :: xs)

let head (x, _) = x

let tail (_, xs) = xs

let last (x, xs) = if List.is_empty xs then x else List.last_exn xs

let of_list_opt = function [] -> None | x :: xs -> Some (x, xs)

let tail_opt t = of_list_opt (tail t)

let map (x, xs) ~f = (f x, List.map ~f xs)

let rev (x, xs) = List.fold xs ~init:(singleton x) ~f:(Fn.flip cons)

(* As the Base.Container docs state, we'll add each function from C explicitly
 * rather than including C *)
module C = Container.Make (struct
  type nonrec 'a t = 'a t

  let fold (x, xs) ~init ~f = List.fold xs ~init:(f init x) ~f

  let iter = `Custom (fun (x, xs) ~f -> f x ; List.iter xs ~f)

  let length = `Define_using_fold
end)

[%%define_locally
C.(find, find_map, iter, length)]

let fold (x, xs) ~init ~f = List.fold xs ~init:(init x) ~f

let to_list (x, xs) = x :: xs

let append (x, xs) ys = (x, xs @ to_list ys)

let take (x, xs) = function
  | 0 ->
      None
  | 1 ->
      Some (x, [])
  | n ->
      Some (x, List.take xs (n - 1))

let min_elt ~compare (x, xs) =
  Option.value_map ~default:x (List.min_elt ~compare xs) ~f:(fun mininum ->
      if compare x mininum < 0 then x else mininum )

let max_elt ~compare (x, xs) =
  Option.value_map ~default:x (List.max_elt ~compare xs) ~f:(fun maximum ->
      if compare x maximum > 0 then x else maximum )

let rec iter_deferred (x, xs) ~f =
  let open Async_kernel in
  let%bind () = f x in
  match xs with [] -> return () | h :: t -> iter_deferred (h, t) ~f
