(* memory_stats.ml -- log OCaml, jemalloc memory data *)

open Core_kernel
open Async
open Gc.Stat

let ocaml_memory_stats () =
  let bytes_per_word = Sys.word_size / 8 in
  let stat = Gc.stat () in
  [ ("heap_size_bytes", `Int (stat.heap_words * bytes_per_word))
  ; ("heap_chunks", `Int stat.heap_chunks)
  ; ("max_heap_size_bytes", `Int (stat.top_heap_words * bytes_per_word))
  ; ("live_size_bytes", `Int (stat.live_words * bytes_per_word))
  ; ("live_blocks", `Int stat.live_blocks)
  ; ("fragments", `Int stat.fragments)
  ]

let jemalloc_memory_stats () =
  let { Jemalloc.active; resident; allocated; mapped } =
    Jemalloc.get_memory_stats ()
  in
  [ ("active", `Int active)
  ; ("resident", `Int resident)
  ; ("allocated", `Int allocated)
  ; ("mapped", `Int mapped)
  ]

let log_memory_stats logger ~process =
  don't_wait_for
    ((* Curve points are allocated in C++ and deallocated with finalizers.
             The points on the C++ heap are much bigger than the OCaml heap
             objects that point to them, which makes the GC underestimate how
             much memory has been allocated since the last collection and not
             run major GCs often enough, which means the finalizers don't run
             and we use way too much memory. As a band-aid solution, we run a
             major GC cycle every ten minutes.
     *)
     let gc_method =
       Option.value ~default:"full" @@ Unix.getenv "MINA_GC_HACK_MODE"
     in
     (* Doing Gc.major is known to work, but takes quite a bit of time.
             Running a single slice might be sufficient, but I haven't tested it
             and the documentation isn't super clear. *)
     let gc_fun =
       match gc_method with
       | "full" ->
           Gc.major
       | "slice" ->
           fun () -> ignore (Gc.major_slice 0)
       | other ->
           failwithf
             "MINA_GC_HACK_MODE was %s, it should be full or slice. Default is \
              full."
             other
     in
     let interval =
       Time.Span.of_sec
       @@ Option.(
            value ~default:600.
              (map ~f:Float.of_string @@ Unix.getenv "MINA_GC_HACK_INTERVAL"))
     in
     let log_stats suffix =
       let proc = ("process", `String process) in
       [%log debug] "OCaml memory statistics, %s" suffix
         ~metadata:(proc :: ocaml_memory_stats ()) ;
       [%log debug] "Jemalloc memory statistics (in bytes)"
         ~metadata:(proc :: jemalloc_memory_stats ())
     in
     let rec loop () =
       log_stats "before major gc" ;
       gc_fun () ;
       log_stats "after major gc" ;
       let%bind () = after interval in
       loop ()
     in
     loop ())
