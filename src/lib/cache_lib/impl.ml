open Async_kernel
open Core_kernel

module type Inputs_intf = sig
  val handle_unconsumed_cache_item :
    logger:Logger.t -> cache_name:string -> unit
end

module Make (Inputs : Inputs_intf) : Intf.Main.S = struct
  module rec Cache : sig
    include
      Intf.Cache.S with type ('t, 'cache_t) cached := ('t, 'cache_t) Cached.t

    val logger : _ t -> Logger.t

    val remove : 'elt t -> [`Consumed | `Unconsumed | `Failure] -> 'elt -> unit
  end = struct
    type 'a t =
      { name: string
      ; on_add: 'a -> unit
      ; on_remove: [`Consumed | `Unconsumed | `Failure] -> 'a -> unit
      ; set: ('a, 'a Intf.final_state) Hashtbl.t
      ; logger: Logger.t }

    let name {name; _} = name

    let logger {logger; _} = logger

    let create (type elt) ~name ~logger ~on_add ~on_remove
        (module Elt : Hashtbl.Key_plain with type t = elt) : elt t =
      let set = Hashtbl.create ~growth_allowed:true ?size:None (module Elt) in
      let logger = Logger.extend logger [("cache", `String name)] in
      {name; on_add; on_remove; set; logger}

    let final_state t x = Hashtbl.find t.set x

    let register_exn t x =
      let final_state = Ivar.create () in
      Hashtbl.add_exn t.set ~key:x ~data:final_state ;
      t.on_add x ;
      Cached.create t x final_state

    let mem t x = Hashtbl.mem t.set x

    let remove t reason x = Hashtbl.remove t.set x ; t.on_remove reason x

    let to_list t = Hashtbl.keys t.set
  end

  and Cached : sig
    include Intf.Cached.S

    val create :
      'elt Cache.t -> 'elt -> 'elt Intf.final_state -> ('elt, 'elt) t
  end = struct
    type (_, _) t =
      | Base :
          { data: 'a
          ; cache: 'a Cache.t
          ; mutable transformed: bool
          ; final_state: 'a Intf.final_state }
          -> ('a, 'a) t
      | Derivative :
          { original: 'a
          ; mutant: 'b
          ; cache: 'a Cache.t
          ; mutable transformed: bool
          ; final_state: 'a Intf.final_state }
          -> ('b, 'a) t
      | Pure : 'a -> ('a, _) t

    let pure x = Pure x

    let is_pure : type a b. (a, b) t -> bool = function
      | Base _ ->
          false
      | Derivative _ ->
          false
      | Pure _ ->
          true

    let cache : type a b. (a, b) t -> b Cache.t = function
      | Base x ->
          x.cache
      | Derivative x ->
          x.cache
      | Pure _ ->
          failwith "cannot access cache of pure Cached.t"

    let value : type a b. (a, b) t -> a = function
      | Base x ->
          x.data
      | Derivative x ->
          x.mutant
      | Pure x ->
          x

    let original : type a b. (a, b) t -> b = function
      | Base x ->
          x.data
      | Derivative x ->
          x.original
      | Pure _ ->
          failwith "cannot access original of pure Cached.t"

    let final_state : type a b. (a, b) t -> b Intf.final_state = function
      | Base x ->
          x.final_state
      | Derivative x ->
          x.final_state
      | Pure _ ->
          failwith "cannot access consumed state of pure Cached.t"

    let was_consumed : type a b. (a, b) t -> bool = function
      | Base x ->
          Ivar.is_full x.final_state || x.transformed
      | Derivative x ->
          Ivar.is_full x.final_state || x.transformed
      | Pure _ ->
          false

    let was_finalized : type a b. (a, b) t -> bool = function
      | Base x ->
          Ivar.is_full x.final_state
      | Derivative x ->
          Ivar.is_full x.final_state
      | Pure _ ->
          false

    let mark_failed : type a b. (a, b) t -> unit = function
      | Base x ->
          if Ivar.is_full x.final_state then
            [%log' error (Logger.create ())] "Ivar.fill bug is here!" ;
          Ivar.fill x.final_state `Failed
      | Derivative x ->
          if Ivar.is_full x.final_state then
            [%log' error (Logger.create ())] "Ivar.fill bug is here!" ;
          Ivar.fill x.final_state `Failed
      | Pure _ ->
          failwith "cannot set consumed state of pure Cached.t"

    let mark_success : type a b. (a, b) t -> unit = function
      | Base x ->
          if Ivar.is_full x.final_state then
            [%log' error (Logger.create ())] "Ivar.fill bug is here!" ;
          Ivar.fill x.final_state (`Success x.data)
      | Derivative x ->
          if Ivar.is_full x.final_state then
            [%log' error (Logger.create ())] "Ivar.fill bug is here!" ;
          Ivar.fill x.final_state (`Success x.original)
      | Pure _ ->
          failwith "cannot set consumed state of pure Cached.t"

    let attach_finalizer t =
      Gc.Expert.add_finalizer (Heap_block.create_exn t) (fun block ->
          let t = Heap_block.value block in
          if not (was_consumed t) then (
            let cache = cache t in
            Cache.remove cache `Unconsumed (original t) ;
            Inputs.handle_unconsumed_cache_item ~logger:(Cache.logger cache)
              ~cache_name:(Cache.name cache) ) ) ;
      t

    let create cache data final_state =
      attach_finalizer (Base {data; cache; transformed= false; final_state})

    let assert_not_consumed t msg =
      let open Error in
      if was_consumed t then raise (of_string msg)

    let assert_not_finalized t msg =
      let open Error in
      if was_finalized t then raise (of_string msg)

    let peek (type a b) (t : (a, b) t) : a =
      assert_not_finalized t "cannot peek at finalized Cached.t" ;
      value t

    let mark_transformed : type a b. (a, b) t -> unit = function
      | Base x ->
          x.transformed <- true
      | Derivative x ->
          x.transformed <- true
      | Pure _ ->
          failwith "cannot set transformed status for pure Cached.t"

    let transform (type a b) (t : (a, b) t) ~(f : a -> 'c) : ('c, b) t =
      assert_not_consumed t "cannot consume Cached.t twice" ;
      mark_transformed t ;
      attach_finalizer
        (Derivative
           { original= original t
           ; mutant= f (value t)
           ; cache= cache t
           ; transformed= false
           ; final_state= final_state t })

    let invalidate_with_failure (type a b) (t : (a, b) t) : a =
      assert_not_finalized t "Cached item has already been finalized" ;
      mark_failed t ;
      Cache.remove (cache t) `Failure (original t) ;
      value t

    let invalidate_with_success (type a b) (t : (a, b) t) : a =
      assert_not_finalized t "Cached item has already been finalized" ;
      mark_success t ;
      Cache.remove (cache t) `Consumed (original t) ;
      value t

    let sequence_deferred (type a b) (t : (a Deferred.t, b) t) :
        (a, b) t Deferred.t =
      let open Deferred.Let_syntax in
      let%map x = peek t in
      transform t ~f:(Fn.const x)

    let sequence_result (type a b) (t : ((a, 'e) Result.t, b) t) :
        ((a, b) t, 'e) Result.t =
      match peek t with
      | Ok x ->
          Ok (transform t ~f:(Fn.const x))
      | Error err ->
          [%log' error (Cache.logger (cache t))]
            "Cached.sequence_result called on an already consumed Cached.t" ;
          ignore (invalidate_with_failure t : (a, 'e) Result.t) ;
          Error err
  end

  module Transmuter_cache :
    Intf.Transmuter_cache.F
    with module Cached := Cached
     and module Cache := Cache = struct
    module Make
        (Transmuter : Intf.Transmuter.S)
        (Registry : Intf.Registry.S with type element := Transmuter.Target.t)
        (Name : Intf.Constant.S with type t := string) :
      Intf.Transmuter_cache.S
      with module Cached := Cached
       and module Cache := Cache
       and type source = Transmuter.Source.t
       and type target = Transmuter.Target.t = struct
      type source = Transmuter.Source.t

      type target = Transmuter.Target.t

      type t = Transmuter.Target.t Cache.t

      let create ~logger =
        Cache.create ~logger ~name:Name.t ~on_add:Registry.element_added
          ~on_remove:Registry.element_removed
          (module Transmuter.Target)

      let register_exn t x =
        let target = Cache.register_exn t (Transmuter.transmute x) in
        Cached.transform target ~f:(Fn.const x)

      let final_state t x = Cache.final_state t (Transmuter.transmute x)

      let mem t x = Cache.mem t (Transmuter.transmute x)
    end
  end
end

let%test_module "cache_lib test instance" =
  ( module struct
    let dropped_cache_items = ref 0

    include Make (struct
      let handle_unconsumed_cache_item ~logger:_ ~cache_name:_ =
        incr dropped_cache_items
    end)

    let setup () = dropped_cache_items := 0

    let with_item ~f =
      Bytes.create 10 |> Bytes.to_string
      |> String.map ~f:(fun _ -> Char.of_int_exn (Random.bits () land 0xff))
      |> f

    let with_cache ~logger ~f =
      Cache.create ~name:"test" ~logger ~on_add:ignore
        ~on_remove:(fun _ _ -> ())
        (module String)
      |> f

    let%test_unit "cached objects do not trigger unconsumption hook when \
                   invalidated" =
      setup () ;
      let logger = Logger.null () in
      with_cache ~logger ~f:(fun cache ->
          with_item ~f:(fun data ->
              let x = Cache.register_exn cache data in
              ignore (Cached.invalidate_with_success x : string) ) ;
          Gc.full_major () ;
          assert (!dropped_cache_items = 0) )

    let%test_unit "cached objects are garbage collected independently of caches"
        =
      setup () ;
      let logger = Logger.null () in
      with_cache ~logger ~f:(fun cache ->
          with_item ~f:(fun data ->
              ignore (Cache.register_exn cache data : (string, string) Cached.t)
          ) ;
          Gc.full_major () ;
          assert (!dropped_cache_items = 1) )

    let%test_unit "cached objects are garbage collected independently of data"
        =
      setup () ;
      let logger = Logger.null () in
      with_item ~f:(fun data ->
          with_cache ~logger ~f:(fun cache ->
              ignore (Cache.register_exn cache data : (string, string) Cached.t)
          ) ;
          Gc.full_major () ;
          assert (!dropped_cache_items = 1) )

    let%test_unit "cached objects are not unexpectedly garbage collected" =
      setup () ;
      let logger = Logger.null () in
      with_cache ~logger ~f:(fun cache ->
          with_item ~f:(fun data ->
              let cached = Cache.register_exn cache data in
              Gc.full_major () ;
              assert (!dropped_cache_items = 0) ;
              ignore (Cached.invalidate_with_success cached : string) ) ) ;
      Gc.full_major () ;
      assert (!dropped_cache_items = 0)

    let%test_unit "garbage collection of derived cached objects do not \
                   trigger unconsumption handler for parents" =
      setup () ;
      let logger = Logger.null () in
      with_cache ~logger ~f:(fun cache ->
          with_item ~f:(fun data ->
              ignore
                ( Cache.register_exn cache data
                  |> Cached.transform ~f:(Fn.const 5)
                  |> Cached.transform ~f:(Fn.const ())
                  : (unit, string) Cached.t ) ) ;
          Gc.full_major () ;
          assert (!dropped_cache_items = 1) )

    let%test_unit "properly invalidated derived cached objects do not trigger \
                   any unconsumption handler calls" =
      setup () ;
      let logger = Logger.null () in
      with_cache ~logger ~f:(fun cache ->
          with_item ~f:(fun data ->
              Cache.register_exn cache data
              |> Cached.transform ~f:(Fn.const 5)
              |> Cached.transform ~f:(Fn.const ())
              |> Cached.invalidate_with_success ) ;
          Gc.full_major () ;
          assert (!dropped_cache_items = 0) )

    let%test_unit "invalidate original cached object would also remove the \
                   derived cached object" =
      setup () ;
      let logger = Logger.null () in
      with_cache ~logger ~f:(fun cache ->
          with_item ~f:(fun data ->
              let src = Cache.register_exn cache data in
              let _der =
                src
                |> Cached.transform ~f:(Fn.const 5)
                |> Cached.transform ~f:(Fn.const ())
              in
              ignore (Cached.invalidate_with_success src : string) ) ;
          Gc.full_major () ;
          assert (!dropped_cache_items = 0) )

    let%test_unit "deriving a cached object inhabits its parent's final_state"
        =
      setup () ;
      with_cache ~logger:(Logger.null ()) ~f:(fun cache ->
          with_item ~f:(fun data ->
              let src = Cache.register_exn cache data in
              let der = Cached.transform src ~f:(Fn.const 5) in
              let src_final_state = Cached.final_state src in
              let der_final_state = Cached.final_state der in
              assert (Ivar.equal src_final_state der_final_state) ) )
  end )
