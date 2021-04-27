module Nat = Nat
open Core_kernel

type 'a adjacency_list = ('a * 'a list) list

module Make (V : Comparable.S) = struct
  module G = struct
    type t = V.Set.t V.Map.t
  end

  let connected_component (g : G.t) v0 =
    let rec go seen next =
      match next with
      | [] ->
          seen
      | v :: next ->
          go (Set.add seen v)
            ( Option.value_map ~default:[]
                ~f:(fun neighbors -> Set.to_list (Set.diff neighbors seen))
                (Map.find g v)
            @ next )
    in
    go V.Set.empty [v0]

  let choose (g : G.t) : V.t option =
    with_return (fun {return} ->
        Map.iteri g ~f:(fun ~key ~data:_ -> return (Some key)) ;
        None )

  let connected (g : G.t) : bool =
    match choose g with
    | None ->
        (* G is empty *)
        true
    | Some v ->
        Int.equal (Map.length g) (Set.length (connected_component g v))

  let remove_vertex (g : G.t) v : G.t =
    Map.remove g v |> Map.map ~f:(Fn.flip Set.remove v)

  (* Naive algorithm for computing the minimum number of vertices required to disconnect
   a graph as

   connectivity G =
    if not (connected G)
    then 0
    else 1 + min_{v in V(G)} connectivity (G - v)

   The function returns a lazy nat, because the algorithm naturally can
   be seen as a search over sequences of vertices of increasing length,
   and upon finishing examining all sequences of a given length, we learn 
   one more piece of information about the return value (i.e., whether it
   is bigger than that length or equal to it.)

   This means that the caller of this function can control how far this
   search goes based on how much information they need. For example, if
   they just want to know whether the graph is connected, they only need
   to force the outermost constructor. In general, if they want to know
   whether the connectivity is >= n, they only need to force the first
   n + 1 constructors.
*)

  let rec connectivity (g : G.t) : Nat.t =
    lazy
      ( if not (connected g) then Z
      else
        S
          ( lazy
            (Nat.min
               (List.map (Map.keys g) ~f:(fun v ->
                    connectivity (remove_vertex g v) ))) ) )
end

let connectivity (type a) (module V : Comparable.S with type t = a)
    (adj : V.t adjacency_list) =
  let module M = Make (V) in
  let g : M.G.t =
    V.Map.of_alist_exn (List.map adj ~f:(fun (x, xs) -> (x, V.Set.of_list xs)))
  in
  M.connectivity g

let%test_unit "tree connectivity" =
  (*
        0
      /  \
      1   2
      |  / \
      3  4 5
  *)
  let tree =
    [(0, [1; 2]); (1, [3; 0]); (2, [0; 4; 5]); (3, [1]); (4, [2]); (5, [2])]
  in
  [%test_eq: int] 1 (Nat.to_int (connectivity (module Int) tree))

let%test_unit "complete graph connectivity" =
  let complete_graph n =
    let all = List.init n ~f:Fn.id in
    List.init n ~f:(fun i -> (i, all))
  in
  (* Complete graph has infinite connectivity. *)
  assert (Nat.at_least (connectivity (module Int) (complete_graph 4)) 10000)
