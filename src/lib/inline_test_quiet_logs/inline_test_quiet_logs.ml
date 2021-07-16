(** Including this library will overwrite the callbacks used by
    ppx_inline_test, allowing us to silence the logger for successful tests.
*)

module Ppx_inline_test_lib = struct
  module Runtime = struct
    include Ppx_inline_test_lib.Runtime

    (* The toplevel stdout file descriptor.
       This is captured by the outermost call in each nested hierarchy of calls
       instead of at startup. This allows us to perform other stdout redirects
       before running tests and still have them apply to tests using this
       module.
    *)
    let stdout_orig_toplevel = ref None

    let redirect_to_newfile () =
      Random.self_init () ;
      (* Save original stdout file descriptor.
         Note that this may differ from the toplevel stdout file descriptor, if
         it has e.g. been overridden by a [test_module] wrapping this test.
      *)
      let stdout_orig = Unix.dup Unix.stdout in
      let has_stdout =
        match !stdout_orig_toplevel with
        | None ->
            stdout_orig_toplevel := Some stdout_orig ;
            true
        | Some _ ->
            false
      in
      let tempfile =
        "temp-stdout.test." ^ string_of_int (Random.int ((1 lsl 30) - 1))
      in
      let tempfile_channel = open_out tempfile in
      (* Overwrite original stdout file descriptor *)
      Unix.dup2 (Unix.descr_of_out_channel tempfile_channel) Unix.stdout ;
      (tempfile, tempfile_channel, stdout_orig, has_stdout)

    let tidy_up ~dump_out (tempfile, tempfile_channel, stdout_orig, has_stdout)
        =
      close_out tempfile_channel ;
      if dump_out then (
        (* Write to the toplevel stdout instead of the original one that we
           saw, in case we are nested within calls. This ensures that our
           output prints in the correct order compared to the test failure
           messages, which appears on stderr.
        *)
        let stdout_for_output =
          match !stdout_orig_toplevel with
          | Some stdout ->
              stdout
          | None ->
              stdout_orig
        in
        Unix.dup2 stdout_for_output Unix.stdout ;
        let tempfile_channel = open_in tempfile in
        let buf_len = 1024 in
        let buf = Stdlib.Bytes.create buf_len in
        let rec go () =
          let len = input tempfile_channel buf 0 buf_len in
          if len > 0 then (output stdout buf 0 len ; go ())
        in
        go () ;
        (* Flush to ensure that the data is printed before the next test
           begins.
        *)
        flush stdout ) ;
      (* If this originally captured the toplevel stdout, un-capture it. *)
      if has_stdout then stdout_orig_toplevel := None ;
      Unix.dup2 stdout_orig Unix.stdout ;
      Unix.unlink tempfile

    let test ~config ~descr ~tags ~filename ~line_number ~start_pos ~end_pos f =
      let f () =
        let redirect_data = redirect_to_newfile () in
        try
          let b = f () in
          tidy_up ~dump_out:(not b) redirect_data ;
          b
        with exn ->
          tidy_up ~dump_out:true redirect_data ;
          raise exn
      in
      test ~config ~descr ~tags ~filename ~line_number ~start_pos ~end_pos f

    let test_unit ~config ~descr ~tags ~filename ~line_number ~start_pos
        ~end_pos f =
      let f () =
        let redirect_data = redirect_to_newfile () in
        try
          f () ;
          tidy_up ~dump_out:false redirect_data
        with exn ->
          tidy_up ~dump_out:true redirect_data ;
          raise exn
      in
      test_unit ~config ~descr ~tags ~filename ~line_number ~start_pos ~end_pos
        f

    let test_module ~config ~descr ~tags ~filename ~line_number ~start_pos
        ~end_pos f =
      let f () =
        let redirect_data = redirect_to_newfile () in
        try
          f () ;
          tidy_up ~dump_out:false redirect_data
        with exn ->
          tidy_up ~dump_out:true redirect_data ;
          raise exn
      in
      test_module ~config ~descr ~tags ~filename ~line_number ~start_pos
        ~end_pos f
  end
end
