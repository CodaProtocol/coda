open Core_kernel

type ('a, 'e, 's) t = 'e -> 's -> ('s * 'a)

module type S = sig
  type env

  include Monad.S2 with type ('a, 's) t = ('a, env, 's) t

  val run : ('a, 's) t -> env -> 's -> ('s * 'a)

  val get_state : ('s, 's) t

  val set_state : 's -> (unit, 's) t

  val modify_state : ('s -> 's) -> (unit, 's) t

  val map2 : ('a, 's) t -> ('b, 's) t -> f:('a -> 'b -> 'c) -> ('c, 's) t
end

module T = struct
  let map t ~f =
    fun tbl s ->
      let (s', x) = t tbl s in
      (s', f x)
  ;;

  let bind t ~f =
    fun tbl s ->
      let (s', x) = t tbl s in
      f x tbl s'
  ;;

  let return x = fun _ s -> (s, x)
  ;;

  let run t tbl s = t tbl s

  let get_state = fun _tbl s -> (s, s)
  ;;

  let read_var v = fun tbl s -> (s, tbl v)
  ;;

  let set_state s = fun tbl _ -> (s, ())
  ;;

  let modify_state f =
    fun _tbl s -> (f s, ())
  ;;

  let map2 x y ~f =
    fun tbl s ->
      let (s, x) = x tbl s in
      let (s, y) = y tbl s in
      (s, f x y)
  ;;
end

module Make (Env : sig type t end) = struct
  type nonrec ('a, 's) t = ('a, Env.t, 's) t

  include T

  module T = struct
    type nonrec ('a, 's) t = ('a, 's) t
    let map = `Custom map
    let bind = bind
    let return = return
  end
  include Monad.Make2(T)

  (* TODO: Delete
  type ('a, 'prover_state) as_prover = ('a, 'prover_state) t

  module Array = struct
    include Array

    let map (t : 'a array) ~(f : 'a -> ('b, 's) as_prover) : ('b array, 's) as_prover =
      fun tbl s0 ->
        let s = ref s0 in
        let res =
          Array.map t ~f:(fun x ->
            let (s', y) = f x tbl !s in
            s := s';
            y)
        in
        (!s, res)
    ;;
  end

  *)
end

include T

include Monad.Make3(struct
    type nonrec ('a, 'e, 's) t = ('a, 'e, 's) t
    let map = `Custom map
    let bind = bind
    let return = return
  end)

