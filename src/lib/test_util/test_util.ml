open Core
open Fold_lib

module Make (Impl : Snarky_backendless.Snark_intf.S) = struct
  let triple_string trips =
    let to_string b = if b then "1" else "0" in
    String.concat ~sep:" "
      (List.map trips ~f:(fun (b1, b2, b3) ->
           to_string b1 ^ to_string b2 ^ to_string b3 ))

  let checked_to_unchecked typ1 typ2 checked input =
    let open Impl in
    let (), checked_result =
      run_and_check
        (let%bind input = exists typ1 ~compute:(As_prover.return input) in
         let%map result = checked input in
         As_prover.read typ2 result)
        ()
      |> Or_error.ok_exn
    in
    checked_result

  let test_to_triples typ fold var_to_triples input =
    let open Impl in
    let (), checked =
      run_and_check
        (let%bind input = exists typ ~compute:(As_prover.return input) in
         let%map result = var_to_triples input in
         As_prover.all
           (List.map result
              ~f:
                (As_prover.read
                   (Typ.tuple3 Boolean.typ Boolean.typ Boolean.typ))))
        ()
      |> Or_error.ok_exn
    in
    let unchecked = Fold.to_list (fold input) in
    if not ([%equal: (bool * bool * bool) list] checked unchecked) then
      failwithf
        !"Got %s (%d)\nexpected %s (%d)"
        (triple_string checked) (List.length checked) (triple_string unchecked)
        (List.length unchecked) ()

  let test_equal ?(equal = Poly.( = )) typ1 typ2 checked unchecked input =
    let checked_result = checked_to_unchecked typ1 typ2 checked input in
    assert (equal checked_result (unchecked input))

  let arbitrary_string ~len =
    String.init (Random.int len) ~f:(fun _ ->
        Char.of_int_exn (Random.int_incl 0 255) )

  let with_randomness r f =
    let s = Caml.Random.get_state () in
    Random.init r ;
    try
      let x = f () in
      Caml.Random.set_state s ; x
    with e -> Caml.Random.set_state s ; raise e
end

include Make (Snark_params.Tick)
