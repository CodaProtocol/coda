(* quickcheck_lib.ml *)

[%%import
"/src/config.mlh"]

open Core_kernel
open Quickcheck.Generator
open Quickcheck.Let_syntax

[%%ifndef
consensus_mechanism]

module Currency = Currency_nonconsensus.Currency

[%%endif]

let of_array array = Quickcheck.Generator.of_list @@ Array.to_list array

let rec map_gens ls ~f =
  match ls with
  | [] ->
      return []
  | h :: t ->
      let%bind h' = f h in
      let%map t' = map_gens t ~f in
      h' :: t'

let replicate_gen g n = map_gens (List.init n ~f:Fn.id) ~f:(Fn.const g)

let init_gen ~f n =
  let rec go : 'a list -> int -> 'a list Quickcheck.Generator.t =
   fun xs n' ->
    if n' < n then f n' >>= fun x -> go (x :: xs) (n' + 1)
    else return @@ List.rev xs
  in
  go [] 0

let init_gen_array ~f n = map ~f:Array.of_list @@ init_gen ~f n

let gen_pair g =
  let%map a = g and b = g in
  (a, b)

let shuffle_arr_inplace arr =
  (* Fisher-Yates shuffle, you need fast swaps for decent performance, so we
     want an array if we're not getting unnecessarily fancy. *)
  let rec go n =
    if n < Array.length arr then (
      let%bind swap_idx = Int.gen_uniform_incl n (Array.length arr - 1) in
      Array.swap arr n swap_idx ;
      go (n + 1) )
    else return arr
  in
  go 0

let shuffle_arr arr = shuffle_arr_inplace @@ Array.copy arr

let shuffle list =
  Array.of_list list |> shuffle_arr_inplace |> map ~f:Array.to_list

(* Generate a list with a Dirichlet distribution, used for coming up with random
   splits of a quantity. Symmetric Dirichlet distribution with alpha = 1.
*)
let gen_symm_dirichlet : int -> float list Quickcheck.Generator.t =
 fun n ->
  let open Quickcheck.Generator.Let_syntax in
  let%map gammas =
    map_gens
      (List.init n ~f:(Fn.const ()))
      ~f:(fun _ ->
        let open Quickcheck.Generator.Let_syntax in
        (* technically this should be (0, 1] and not (0, 1) but I expect it
           doesn't matter for our purposes. *)
        let%map uniform = Float.gen_uniform_excl 0. 1. in
        Float.log uniform )
  in
  let sum = List.fold gammas ~init:0. ~f:(fun x y -> x +. y) in
  List.map gammas ~f:(fun gamma -> gamma /. sum)

module type Int_s = sig
  type t

  val zero : t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( > ) : t -> t -> bool

  val of_int : int -> t

  val to_int : t -> int
end

let gen_division_generic (type t) (module M : Int_s with type t = t) (n : t)
    (k : int) : M.t list Quickcheck.Generator.t =
  if k = 0 then Quickcheck.Generator.return []
  else
    let open Quickcheck.Generator.Let_syntax in
    (* Using a symmetric Dirichlet distribution with concentration parameter 1
       defined above gives a distribution with uniform probability density over
       all possible splits of the quantity. See the Wikipedia article for some
       more detail: https://en.wikipedia.org/wiki/Dirichlet_distribution,
       particularly the sections about the flat Dirichlet distribution and
       string cutting.
    *)
    let%bind dirichlet = gen_symm_dirichlet k in
    let n_float = Float.of_int @@ M.to_int n in
    let float_to_mt : float -> t =
     fun fl ->
      match Float.iround_down fl with
      | Some int ->
          M.of_int int
      | None ->
          failwith "gen_division_generic: out of range"
    in
    let res = List.map dirichlet ~f:(fun x -> float_to_mt @@ (x *. n_float)) in
    let total = List.fold res ~f:M.( + ) ~init:M.zero in
    return
      ( match res with
      | [] ->
          failwith
            "empty result list in gen_symm_dirichlet, this should be \
             impossible. "
      | head :: rest ->
          (* Going through floating point land may have caused some rounding error. We
           tack it onto the first result so that the sum of the output is equal to n.
         *)
          if M.( > ) n total then M.(head + (n - total)) :: rest
          else M.(head - (total - n)) :: rest )

let gen_division = gen_division_generic (module Int)

let gen_division_currency =
  gen_division_generic
    ( module struct
      include Currency.Amount

      let ( + ) a b = Option.value_exn (a + b)

      let ( - ) a b = Option.value_exn (a - b)
    end )

let imperative_fixed_point root ~f =
  let%map f' = fixed_point f in
  f' root

let gen_imperative_rose_tree ?(p = 0.75) (root_gen : 'a t)
    (node_gen : ('a -> 'a) t) =
  let%bind root = root_gen in
  imperative_fixed_point root ~f:(fun self ->
      match%bind size with
      | 0 ->
          return (fun parent -> Rose_tree.T (parent, []))
      | n ->
          let%bind fork_count = geometric ~p 1 >>| Int.max n in
          let%bind fork_sizes = gen_division n fork_count in
          let positive_fork_sizes =
            List.filter fork_sizes ~f:(fun s -> s > 0)
          in
          let%map forks =
            map_gens positive_fork_sizes ~f:(fun s ->
                tuple2 node_gen (with_size ~size:(s - 1) self) )
          in
          fun parent ->
            Rose_tree.T
              (parent, List.map forks ~f:(fun (this, f) -> f (this parent))) )

let gen_imperative_ktree ?(p = 0.75) (root_gen : 'a t)
    (node_gen : ('a -> 'a) t) =
  let%bind root = root_gen in
  imperative_fixed_point root ~f:(fun self ->
      match%bind size with
      | 0 ->
          return (fun _ -> [])
      (* this case is optional but more effecient *)
      | 1 ->
          let%map this = node_gen in
          fun parent -> [this parent]
      | n ->
          let%bind this = node_gen in
          let%bind fork_count = geometric ~p 1 >>| Int.max n in
          let%bind fork_sizes = gen_division (n - 1) fork_count in
          let%map forks =
            map_gens fork_sizes ~f:(fun s -> with_size ~size:s self)
          in
          fun parent ->
            let x = this parent in
            x :: List.bind forks ~f:(fun f -> f x) )

let gen_imperative_list (root_gen : 'a t) (node_gen : ('a -> 'a) t) =
  let%bind root = root_gen in
  imperative_fixed_point root ~f:(fun self ->
      match%bind size with
      | 0 ->
          return (fun _ -> [])
      | n ->
          let%bind this = node_gen in
          let%map f = with_size ~size:(n - 1) self in
          fun parent -> parent :: f (this parent) )

let%test_module "Quickcheck lib tests" =
  ( module struct
    let%test_unit "gen_imperative_list" =
      let increment = ( + ) 2 in
      let root = 1 in
      let root_gen = return root in
      let gen =
        Int.gen_incl 2 100
        >>= fun size ->
        Quickcheck.Generator.with_size ~size
          (gen_imperative_list root_gen (return increment))
      in
      Quickcheck.test gen ~f:(fun list ->
          match list with
          | [] ->
              failwith "We assume that our list has at least one element"
          | x :: xs ->
              assert (x = root) ;
              let result =
                List.fold_result xs ~init:x ~f:(fun elem next_elem ->
                    if next_elem = increment elem then Result.return next_elem
                    else
                      Or_error.errorf
                        !"elements do not add up correctly %d %d"
                        elem next_elem )
              in
              assert (Result.is_ok result) )
  end )
