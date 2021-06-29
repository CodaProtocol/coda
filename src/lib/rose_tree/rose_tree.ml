open Core_kernel

type 'a t = T of 'a * 'a t list

type 'a display = {value: 'a; children: 'a display list} [@@deriving yojson]

let rec to_display (T (value, children)) =
  {value; children= List.map ~f:to_display children}

let rec of_display {value; children} =
  T (value, List.map ~f:of_display children)

let to_yojson conv t = display_to_yojson conv (to_display t)

let of_yojson conv json =
  Result.map ~f:of_display (display_of_yojson conv json)

let root (T (value, _)) = value

let children (T (_, children)) = children

let rec print ?(whitespace = 0) ~element_to_string (T (root, branches)) =
  Printf.printf "%s- %s\n"
    (String.make whitespace ' ')
    (element_to_string root) ;
  List.iter branches ~f:(print ~whitespace:(whitespace + 2) ~element_to_string)

let rec of_list_exn ?(subtrees = []) = function
  | [] ->
      raise
        (Invalid_argument
           "Rose_tree.of_list_exn: cannot construct rose tree from empty list")
  | [h] ->
      T (h, subtrees)
  | h :: t ->
      T (h, [of_list_exn t ~subtrees])

let of_non_empty_list ?(subtrees = []) =
  Fn.compose
    (Non_empty_list.fold
       ~init:(fun x -> T (x, subtrees))
       ~f:(fun acc x -> T (x, [acc])))
    Non_empty_list.rev

let rec equal ~f (T (value1, children1)) (T (value2, children2)) =
  f value1 value2 && List.equal (equal ~f) children1 children2

let subset ~f xs ys = List.for_all xs ~f:(fun x -> List.mem ys x ~equal:f)

let bag_equiv ~f xs ys = subset ~f xs ys && subset ~f ys xs

let rec equiv ~f (T (x1, ts1)) (T (x2, ts2)) =
  f x1 x2 && bag_equiv ~f:(equiv ~f) ts1 ts2

module type Monad_intf = sig
  include Monad.S

  module List : sig
    val iter : 'a list -> f:('a -> unit t) -> unit t

    val map : 'a list -> f:('a -> 'b t) -> 'b list t
  end
end

module type Ops_intf = sig
  module Monad : Monad_intf

  val iter : 'a t -> f:('a -> unit Monad.t) -> unit Monad.t

  val map : 'a t -> f:('a -> 'b Monad.t) -> 'b t Monad.t

  val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b Monad.t) -> 'b t Monad.t

  val fold_map_over_subtrees :
    'a t -> init:'b -> f:('b -> 'a t -> 'b Monad.t) -> 'b t Monad.t
end

module Make_ops (Monad : Monad_intf) : Ops_intf with module Monad := Monad =
struct
  open Monad.Let_syntax

  let rec iter (T (base, successors)) ~f =
    let%bind () = f base in
    Monad.List.iter successors ~f:(iter ~f)

  let rec map (T (base, successors)) ~f =
    let%bind base' = f base in
    let%map successors' = Monad.List.map successors ~f:(map ~f) in
    T (base', successors')

  let rec fold_map (T (base, successors)) ~init ~f =
    let%bind base' = f init base in
    let%map successors' =
      Monad.List.map successors ~f:(fold_map ~init:base' ~f)
    in
    T (base', successors')

  let rec fold_map_over_subtrees (T (_, successors) as subtree) ~init ~f =
    let%bind base' = f init subtree in
    let%map successors' =
      Monad.List.map successors ~f:(fold_map_over_subtrees ~init:base' ~f)
    in
    T (base', successors')
end

include Make_ops (struct
  include Monad.Ident
  module List = List
end)

let rec flatten (T (x, ts)) = x :: List.concat_map ts ~f:flatten

module Deferred = struct
  open Async_kernel

  include Make_ops (struct
    include (Deferred : Monad.S with type +'a t = 'a Deferred.t)

    module List = struct
      open Deferred.List

      let iter ls ~f = iter ~how:`Sequential ls ~f

      let map ls ~f = map ~how:`Sequential ls ~f
    end
  end)

  let rec all (T (x', ts')) =
    let open Deferred.Let_syntax in
    let%bind x = x' in
    let%bind ts = Deferred.all @@ List.map ~f:all ts' in
    return @@ T (x, ts)

  module Or_error = Make_ops (struct
    include (
      Deferred.Or_error : Monad.S with type +'a t = 'a Deferred.Or_error.t )

    module List = struct
      open Deferred.Or_error.List

      let iter ls ~f = iter ~how:`Sequential ls ~f

      let map ls ~f = map ~how:`Sequential ls ~f
    end
  end)
end

module Or_error = Make_ops (struct
  include Or_error

  module List = struct
    open Or_error.Let_syntax

    let iter ls ~f =
      List.fold_left ls ~init:(return ()) ~f:(fun or_error x ->
          let%bind () = or_error in
          f x )

    let map ls ~f =
      let%map ls' =
        List.fold_left ls ~init:(return []) ~f:(fun or_error x ->
            let%bind t = or_error in
            let%map x' = f x in
            x' :: t )
      in
      List.rev ls'
  end
end)
