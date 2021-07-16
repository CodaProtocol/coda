open Core_kernel

module Tree = struct
  type 'a t = Empty | Leaf of 'a | Node of 'a t * 'a t [@@deriving sexp]

  let to_list (t : 'a t) : 'a list =
    let rec go acc t =
      match t with
      | Empty ->
          acc
      | Leaf x ->
          x :: acc
      | Node (l, r) ->
          go (go acc l) r
    in
    List.rev (go [] t)

  let fold t ~init ~f =
    let rec go acc t =
      match t with
      | Empty ->
          acc
      | Leaf x ->
          f acc x
      | Node (l, r) ->
          go (go acc l) r
    in
    go init t

  let return x = Leaf x

  let append t1 t2 =
    match t1 with
    | Empty ->
        t2
    | _ -> (
        match t2 with Empty -> t1 | _ -> Node (t1, t2) )
end

type ('a, 'x, 'e) t0 = ('a * 'x Tree.t, 'e) Result.t

module type S = sig
  include Monad.S3

  val of_result : ('a, 'e) Result.t -> ('a, 'x, 'e) t

  val write : 'x -> (unit, 'x, 'e) t

  val write_all : 'x Tree.t -> (unit, 'x, 'e) t

  val lift : ('a, 'x, 'e) t0 -> ('a, 'x, 'e) t

  val catch :
    ('a, 'x, 'e) t -> f:(('a, 'x, 'e) t0 -> ('b, 'x, 'e) t) -> ('b, 'x, 'e) t
end

module T = struct
  type ('a, 'x, 'e) t = ('a * 'x Tree.t, 'e) Result.t

  let map (type a b x e) (t : (a, x, e) t) ~(f : a -> b) : (b, x, e) t =
    Result.map t ~f:(fun (x, w) -> (f x, w))

  let return (type a x e) (x : a) : (a, x, e) t = Ok (x, Empty)

  let map = `Custom map

  let bind (type a b x e) (t : (a, x, e) t) ~(f : a -> (b, x, e) t) :
      (b, x, e) t =
    Result.bind t ~f:(fun (a, w1) ->
        Result.map (f a) ~f:(fun (b, w2) -> (b, Tree.append w1 w2)))
end

include T
include Monad.Make3 (T)

let catch (type a b x e) (t : (a, x, e) t) ~(f : (a, x, e) t0 -> (b, x, e) t) :
    (b, x, e) t =
  f t

let lift = Fn.id

let write (type x e) (x : x) : (unit, x, e) t = Ok ((), Leaf x)

let write_all (type x e) (x : x Tree.t) : (unit, x, e) t = Ok ((), x)

let run (t : ('a, 'x, 'e) t) : ('a * 'x Tree.t, 'e) Result.t = t

let of_result (type a e) (t : (a, e) Result.t) : (a, _, e) t =
  Result.map t ~f:(fun x -> (x, Tree.Empty))

module Deferred = struct
  module T = struct
    (* We special case undeferred values for efficiency. It is not semantically
       necessary (i.e., we could get away with just using

       ('a * 'x Tree.t, 'e) Async.Deferred.Result.t

       if we didn't care about efficiency.
    *)
    type ('a, 'x, 'e) t =
      | Undeferred of ('a * 'x Tree.t, 'e) Result.t
      | Deferred of ('a * 'x Tree.t, 'e) Async.Deferred.Result.t

    let map (type a b x e) (t : (a, x, e) t) ~(f : a -> b) : (b, x, e) t =
      match t with
      | Undeferred t ->
          Undeferred (map t ~f)
      | Deferred t ->
          Deferred (Async.Deferred.map t ~f:(map ~f))

    let return (type a x e) (x : a) : (a, x, e) t = Undeferred (return x)

    let map = `Custom map

    let bind (type a b x e) (t : (a, x, e) t) ~(f : a -> (b, x, e) t) :
        (b, x, e) t =
      let g w1 (b, w2) = (b, Tree.append w1 w2) in
      match t with
      | Undeferred (Error e) ->
          Undeferred (Error e)
      | Undeferred (Ok (a, w1)) -> (
          match f a with
          | Undeferred rb ->
              Undeferred (Result.map rb ~f:(g w1))
          | Deferred drb ->
              Deferred (Async.Deferred.Result.map drb ~f:(g w1)) )
      | Deferred d ->
          let open Async in
          let open Deferred.Result.Let_syntax in
          Deferred
            (let%bind a, w1 = d in
             match f a with
             | Undeferred rb ->
                 Deferred.return (Result.map rb ~f:(g w1))
             | Deferred drb ->
                 Deferred.Result.map drb ~f:(g w1))

    let lift (type a x e) (t : (a, x, e) T.t) : (a, x, e) t = Undeferred t
  end

  include T
  include Monad.Make3 (T)

  let catch (type a b x e) (t : (a, x, e) t) ~(f : (a, x, e) t0 -> (b, x, e) t)
      : (b, x, e) t =
    match t with
    | Undeferred t ->
        f t
    | Deferred t ->
        Deferred
          (Async.Deferred.bind t ~f:(fun t ->
               match f t with Undeferred t -> Async.return t | Deferred t -> t))

  let write x = lift (write x)

  let write_all x = lift (write_all x)

  let of_result (type a e) (t : (a, e) Result.t) : (a, _, e) t =
    lift (of_result t)

  let run (t : ('a, 'x, 'e) t) : ('a * 'x Tree.t, 'e) Async.Deferred.Result.t =
    match t with Undeferred t -> Async.Deferred.return t | Deferred t -> t
end
