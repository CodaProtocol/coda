open Async

let mina = Mina_plugins.get_mina_lib ()

let read_input =
  let stdout = Lazy.force Writer.stdout in
  let stdin = Lazy.force Reader.stdin in
  let rec go buffer len i =
    if i >= len then return (i, false)
    else
      let%bind c = Reader.read_char stdin in
      match c with
      | `Eof ->
          return (i, true)
      | `Ok c ->
          Bytes.set buffer i c ;
          if c = '\n' then return (i + 1, false) else go buffer len (i + 1)
  in
  fun prompt buffer len ->
    Writer.write stdout prompt ;
    Thread_safe.block_on_async_exn (fun () ->
        let%bind () = Writer.flushed stdout in
        go buffer len 0 )

let () =
  let config = Mina_lib.config mina in
  [%log' info config.logger] "Hi from toplevel plugin!" ;
  (* TODO: Load relevant interfaces into the environment as
     [toplevel_startup_hook].
  *)
  (* TODO: Place [mina] in the toplevel environment once the interfaces -- and
     thus its type -- has been loaded.
  *)
  for i = 1 to Array.length Sys.argv - 1 do
    (* We need to not have junk in the argv otherwise [Opttopmain] will attempt
       to parse it. Additionally, whatever is in the initfile (if it exists)
       will probably not be relevant here, and may refer to directives or
       modules that aren't available.
    *)
    Sys.argv.(i) <- "-noinit"
  done ;
  Ocamlnat_lib.Opttoploop.read_interactive_input := read_input ;
  don't_wait_for @@ In_thread.run Ocamlnat_lib.Opttopmain.main
