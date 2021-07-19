(* main.ml -- run inline benchmarks for given libraries *)

(* you can control which libraries are benchmarked using the environment variable BENCHMARK_LIBRARIES,
   which is either "all" or a comma-delimited list of libraries; if the variable is not present, or empty,
   run benchmarks for all libraries
 *)

open Core_kernel

let available_libraries = ["consensus"; "vrf_lib_tests"; "mina_base"]

let run_benchmarks_in_lib libname =
  Core.printf "Running inline tests in library \"%s\"\n%!" libname ;
  Inline_benchmarks_public.Runner.main ~libname

let () =
  match Sys.getenv_opt "BENCHMARK_LIBRARIES" with
  | None | Some "" | Some "all" ->
      List.iter available_libraries ~f:run_benchmarks_in_lib
  | Some libs ->
      let chosen_libraries = String.split libs ~on:',' in
      let bad_libnames = ref [] in
      List.iter chosen_libraries ~f:(fun lib ->
          if not (List.mem available_libraries lib ~equal:String.equal) then
            bad_libnames := lib :: !bad_libnames ) ;
      if not (List.is_empty !bad_libnames) then (
        eprintf "These libraries not available for benchmarking: %s\n%!"
          (String.concat (List.rev !bad_libnames) ~sep:",") ;
        exit 1 ) ;
      List.iter chosen_libraries ~f:run_benchmarks_in_lib
