open Core_kernel

type tt = Z | S of t

and t = tt Lazy.t

let to_int (n : t) =
  let rec go acc = function Z -> acc | S n -> go (acc + 1) (Lazy.force n) in
  go 0 (Lazy.force n)

let rec at_least (n : t) (k : int) =
  match (Lazy.force n, k) with
  | Z, 0 ->
      true
  | Z, _ ->
      false
  | S _, 0 ->
      true
  | S n, k ->
      at_least n (k - 1)

let take (n0 : t) (k0 : int) : [`Failed_after of int | `Ok] =
  let rec go acc n k =
    match (Lazy.force n, k) with
    | Z, 0 ->
        `Ok
    | Z, _ ->
        `Failed_after acc
    | S _, 0 ->
        `Ok
    | S n, k ->
        go (acc + 1) n (k - 1)
  in
  go 0 n0 k0

let at_least n k = if k < 0 then true else at_least n k

let rec min (xs : t list) : tt =
  match
    with_return (fun {return} ->
        `S
          (List.map xs ~f:(fun x ->
               match Lazy.force x with Z -> return `Z | S n -> n )) )
  with
  | `Z ->
      Z
  | `S xs ->
      S (lazy (min xs))
