open Core

(*  *)

module type Strategy = sig
  type 'a t

  type 'a with_init_args

  val cps_create : f:(_ t -> 'b) -> 'b with_init_args

  val touch : 'a t -> 'a -> 'a list

  val remove : 'a t -> 'a -> unit

  val clear : 'a t -> unit
end

module Memoized = struct
  type 'a t = ('a, exn) Result.t

  let return : 'a t -> 'a = function
    | Result.Ok x ->
        x
    | Result.Error e ->
        raise e

  let create ~f arg =
    try Result.Ok (f arg) with
    | Sys.Break as e ->
        raise e
    | e ->
        Result.Error e
end

module type Store = sig
  type ('k, 'v) t

  type 'a with_init_args

  val cps_create : f:((_, _) t -> 'b) -> 'b with_init_args

  val clear : (_, _) t -> unit

  val set : ('k, 'v) t -> key:'k -> data:'v -> unit

  val find : ('k, 'v) t -> 'k -> 'v option

  val data : (_, 'v) t -> 'v list

  val remove : ('k, _) t -> 'k -> unit
end

module type S = sig
  type ('a, 'b) t

  type 'a with_init_args

  type ('a, 'b) memo = ('a, ('b, exn) Result.t) t

  val find : ('k, 'v) t -> 'k -> 'v option

  val add : ('k, 'v) t -> key:'k -> data:'v -> unit

  val remove : ('k, _) t -> 'k -> unit

  val clear : (_, _) t -> unit

  val create : destruct:('v -> unit) option -> ('k, 'v) t with_init_args

  val call_with_cache : cache:('a, 'b) memo -> ('a -> 'b) -> 'a -> 'b

  val memoize :
       ?destruct:('b -> unit)
    -> ('a -> 'b)
    -> (('a, 'b) memo * ('a -> 'b)) with_init_args
end

module Make (Strat : Strategy) (Store : Store) :
  S with type 'a with_init_args = 'a Store.with_init_args Strat.with_init_args =
struct
  type 'a with_init_args = 'a Store.with_init_args Strat.with_init_args

  type ('k, 'v) t =
    { destruct: ('v -> unit) option
          (** Function to be called on removal of values from the store *)
    ; strat: 'k Strat.t
    ; store: ('k, 'v) Store.t  (** The actual key value store*) }

  type ('a, 'b) memo = ('a, ('b, exn) Result.t) t

  let clear_from_store cache key =
    match Store.find cache.store key with
    | None ->
        failwith
          "Cache.Make: strategy wants to remove a key which isn't in the store"
    | Some v ->
        Option.call ~f:cache.destruct v ;
        Store.remove cache.store key

  let touch_key cache key =
    List.iter (Strat.touch cache.strat key) ~f:(fun k ->
        clear_from_store cache k )

  let find cache k =
    let res = Store.find cache.store k in
    if Option.is_some res then touch_key cache k ;
    res

  let add cache ~key ~data =
    touch_key cache key ;
    Store.set cache.store ~key ~data

  let remove cache key =
    Option.iter (Store.find cache.store key) ~f:(fun v ->
        Strat.remove cache.strat key ;
        Option.call ~f:cache.destruct v ;
        Store.remove cache.store key )

  let clear cache =
    Option.iter cache.destruct ~f:(fun destruct ->
        List.iter (Store.data cache.store) ~f:destruct ) ;
    Strat.clear cache.strat ;
    Store.clear cache.store

  let create ~destruct =
    Strat.cps_create ~f:(fun strat ->
        Store.cps_create ~f:(fun store -> {strat; destruct; store}) )

  let call_with_cache ~cache f arg =
    match find cache arg with
    | Some v ->
        Memoized.return v
    | None ->
        touch_key cache arg ;
        let rval = Memoized.create ~f arg in
        Store.set cache.store ~key:arg ~data:rval ;
        Memoized.return rval

  let memoize ?destruct f =
    Strat.cps_create ~f:(fun strat ->
        Store.cps_create ~f:(fun store ->
            let destruct = Option.map destruct ~f:(fun f -> Result.iter ~f) in
            let cache = {strat; destruct; store} in
            let memd_f arg = call_with_cache ~cache f arg in
            (cache, memd_f) ) )
end

module Strategy = struct
  module Lru = struct
    type 'a t =
      { (* sorted in order of descending recency *)
        list: 'a Doubly_linked.t
      ; (* allows fast lookup in the list above *)
        table: ('a, 'a Doubly_linked.Elt.t) Hashtbl.t
      ; mutable maxsize: int
      ; mutable size: int }

    type 'a with_init_args = int -> 'a

    let kill_extra lru =
      let extra = ref [] in
      while lru.size > lru.maxsize do
        let key = Option.value_exn (Doubly_linked.remove_last lru.list) in
        Hashtbl.remove lru.table key ;
        (* remove from table *)
        lru.size <- lru.size - 1 ;
        (* reduce size by 1 *)
        extra := key :: !extra
      done ;
      !extra

    let touch lru x =
      let el = Doubly_linked.insert_first lru.list x in
      match Hashtbl.find lru.table x with
      | Some old_el ->
          Doubly_linked.remove lru.list old_el ;
          Hashtbl.set lru.table ~key:x ~data:el ;
          []
      | None ->
          Hashtbl.set lru.table ~key:x ~data:el ;
          lru.size <- lru.size + 1 ;
          kill_extra lru

    let remove lru x =
      Option.iter (Hashtbl.find lru.table x) ~f:(fun el ->
          Doubly_linked.remove lru.list el ;
          Hashtbl.remove lru.table x )

    let create maxsize =
      { list= Doubly_linked.create ()
      ; table= Hashtbl.Poly.create () ~size:100
      ; maxsize
      ; size= 0 }

    let cps_create ~f maxsize = f (create maxsize)

    let clear lru =
      lru.size <- 0 ;
      Hashtbl.clear lru.table ;
      Doubly_linked.clear lru.list
  end

  module Keep_all = struct
    type 'a t = unit

    type 'a with_init_args = 'a

    let cps_create ~f = f ()

    let touch () _ = []

    let remove () _ = ()

    let clear () = ()
  end
end

module Store = struct
  module Table = struct
    include Hashtbl

    type 'a with_init_args = 'a

    let cps_create ~f = f (Hashtbl.Poly.create () ~size:16)
  end
end

module Keep_all = Make (Strategy.Keep_all) (Store.Table)
module Lru = Make (Strategy.Lru) (Store.Table)

let keep_one ?(destruct = ignore) f =
  let v = ref None in
  () ;
  fun x ->
    match !v with
    | Some (x', y) when Poly.( = ) x' x ->
        Memoized.return y
    | _ ->
        Option.iter !v ~f:(fun (_, y) -> Result.iter ~f:destruct y) ;
        v := None ;
        let res = Memoized.create ~f x in
        v := Some (x, res) ;
        Memoized.return res

let memoize ?destruct ?(expire = `Keep_all) f =
  match expire with
  | `Lru size ->
      snd (Lru.memoize ?destruct f size)
  | `Keep_all ->
      snd (Keep_all.memoize ?destruct f)
  | `Keep_one ->
      keep_one ?destruct f

let unit f =
  let l = Lazy.from_fun f in
  fun () -> Lazy.force l
