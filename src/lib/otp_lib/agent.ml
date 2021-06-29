open Core_kernel

type read_write

type read_only

type _ flag = Read_write : read_write flag | Read_only : read_only flag

type 'a t_ =
  { mutable a: 'a
  ; mutable on_update: 'a -> unit
  ; mutable dirty: bool
  ; mutable subscribers: 'a t_ list }

type ('flag, 'a) t = 'a t_ constraint 'flag = _ flag

let create ~(f : 'a -> 'b) x : (_ flag, 'b) t =
  {a= f x; on_update= Fn.ignore; dirty= false; subscribers= []}

let get (t : (_ flag, 'a) t) =
  if t.dirty then (
    t.dirty <- false ;
    (t.a, `Different) )
  else (t.a, `Same)

let rec update (t : (read_write flag, 'a) t) a =
  t.a <- a ;
  t.dirty <- true ;
  t.on_update a ;
  List.iter t.subscribers ~f:(fun subscriber -> update subscriber a)

let on_update (t : (_ flag, 'a) t) ~f = t.on_update <- (fun a -> f a)

let num_subscribers t = List.length t.subscribers

let read_only (t : (read_write flag, 'a) t) : (read_only flag, 'a) t =
  let read_only_copy =
    {a= t.a; on_update= t.on_update; dirty= t.dirty; subscribers= []}
  in
  t.subscribers <- read_only_copy :: t.subscribers ;
  read_only_copy

let%test_module "Agent" =
  ( module struct
    let%test "Doing an update will also affect read_only copies" =
      let intial_value = 1 in
      let agent = create ~f:Fn.id intial_value in
      let read_only_agent = read_only agent in
      let is_touched = ref false in
      on_update read_only_agent ~f:(fun _ -> is_touched := true) ;
      let new_value = intial_value + 2 in
      update agent new_value ;
      let equal = [%equal: int * [`Same | `Different]] in
      !is_touched
      && 1 = num_subscribers agent
      && 0 = num_subscribers read_only_agent
      && equal (new_value, `Different) (get read_only_agent)
      && equal (new_value, `Different) (get agent)
  end )
