open Core_kernel
open Async_kernel

(** The is a monad which is conceptually similar to `Deferred.Or_error.t`,
 *  except that there are 2 types of errors which can be returned at each bind
 *  point in a computation: soft errors, and hard errors. Soft errors do not
 *  effect the control flow of the monad, and are instead accumulated for later
 *  extraction. Hard errors effect the control flow of the monad in the same
 *  way an `Error` constructor for `Or_error.t` would.
 It remains similar to Deferred.Or_error.t in that it is a specialization of Deferred.Result.t
 *)

module Error_accumulator = Test_error.Error_accumulator

module Hard_fail = struct
  type t =
    { (* Most of the time, there is only one hard error, but we can have multiple when joining lists of monads (concurrency) *)
      hard_errors: Test_error.internal_error Error_accumulator.t
    ; soft_errors: Test_error.internal_error Error_accumulator.t }
  [@@deriving eq, sexp_of, compare]

  (* INVARIANT: hard_errors should always have at least 1 error *)
  let check_invariants {hard_errors; _} =
    Error_accumulator.error_count hard_errors > 0

  let add_soft_errors {hard_errors; soft_errors} new_soft_errors =
    { hard_errors
    ; soft_errors= Error_accumulator.merge soft_errors new_soft_errors }

  let of_hard_errors hard_errors =
    {hard_errors; soft_errors= Error_accumulator.empty}

  let contextualize context {hard_errors; soft_errors} =
    { hard_errors=
        Error_accumulator.contextualize' context hard_errors
          ~time_of_error:Test_error.occurrence_time
    ; soft_errors=
        Error_accumulator.contextualize' context soft_errors
          ~time_of_error:Test_error.occurrence_time }
end

module Result_accumulator = struct
  type 'a t =
    { computation_result: 'a
    ; soft_errors: Test_error.internal_error Error_accumulator.t }
  [@@deriving eq, sexp_of, compare]

  let create computation_result soft_errors = {computation_result; soft_errors}

  let return a = {computation_result= a; soft_errors= Error_accumulator.empty}

  let is_ok {soft_errors; _} = Error_accumulator.error_count soft_errors = 0

  let contextualize context acc =
    { acc with
      soft_errors=
        Error_accumulator.contextualize' context acc.soft_errors
          ~time_of_error:Test_error.occurrence_time }
end

type 'a t = ('a Result_accumulator.t, Hard_fail.t) Deferred.Result.t

module T = Monad.Make (struct
  type nonrec 'a t = 'a t

  let return a =
    a |> Result_accumulator.return |> Result.return |> Deferred.return

  let bind res ~f =
    let open Result_accumulator in
    match%bind res with
    | Ok {computation_result= prev_result; soft_errors} -> (
        match%map f prev_result with
        | Ok {computation_result; soft_errors= new_soft_errors} ->
            Ok
              { computation_result
              ; soft_errors=
                  Error_accumulator.merge soft_errors new_soft_errors }
        | Error hard_fail ->
            Error (Hard_fail.add_soft_errors hard_fail soft_errors) )
    | Error hard_fail ->
        if not (Hard_fail.check_invariants hard_fail) then
          failwith
            "Malleable_error invariant broken: got a hard fail without an error"
        else Deferred.return (Error hard_fail)

  let map = `Define_using_bind
end)

include T

let lift = Deferred.bind ~f:return

let soft_error ~value error =
  error |> Test_error.internal_error |> Error_accumulator.singleton
  |> Result_accumulator.create value
  |> Result.return |> Deferred.return

let hard_error error =
  error |> Test_error.internal_error |> Error_accumulator.singleton
  |> Hard_fail.of_hard_errors |> Result.fail |> Deferred.return

let contextualize context m =
  let open Deferred.Let_syntax in
  match%map m with
  | Ok acc ->
      Ok (Result_accumulator.contextualize context acc)
  | Error hard_fail ->
      Error (Hard_fail.contextualize context hard_fail)

let soften_error m =
  let open Deferred.Let_syntax in
  match%map m with
  | Ok acc ->
      Ok acc
  | Error {Hard_fail.soft_errors; hard_errors} ->
      Ok
        (Result_accumulator.create ()
           (Error_accumulator.merge soft_errors hard_errors))

let is_ok = function Ok acc -> Result_accumulator.is_ok acc | _ -> false

let ok_unit = return ()

let ok_if_true ?(error_type = `Hard) ~error b =
  if b then Result_accumulator.return () |> Result.return |> Deferred.return
  else
    match error_type with
    | `Soft ->
        soft_error ~value:() error
    | `Hard ->
        hard_error error

let or_soft_error ~value or_error =
  match or_error with
  | Ok x ->
      return x
  | Error error ->
      soft_error ~value error

let soft_error_string ~value = Fn.compose (soft_error ~value) Error.of_string

let soft_error_format ~value format =
  Printf.ksprintf (soft_error_string ~value) format

let or_hard_error or_error =
  match or_error with Ok x -> return x | Error error -> hard_error error

let hard_error_string = Fn.compose hard_error Error.of_string

let hard_error_format format = Printf.ksprintf hard_error_string format

let combine_errors (malleable_errors : 'a t list) : 'a list t =
  let open T.Let_syntax in
  let%map values =
    List.fold_left malleable_errors ~init:(return []) ~f:(fun acc el ->
        let%bind t = acc in
        let%map h = el in
        h :: t )
  in
  List.rev values

let lift_error_set (type a) (m : a t) :
    ( a * Test_error.internal_error Test_error.Set.t
    , Test_error.internal_error Test_error.Set.t )
    Deferred.Result.t =
  let open Deferred.Let_syntax in
  let error_set hard_errors soft_errors =
    {Test_error.Set.hard_errors; soft_errors}
  in
  match%map m with
  | Ok {computation_result; soft_errors} ->
      Ok (computation_result, error_set Error_accumulator.empty soft_errors)
  | Error {hard_errors; soft_errors} ->
      Error (error_set hard_errors soft_errors)

let lift_error_set_unit (m : unit t) :
    Test_error.internal_error Test_error.Set.t Deferred.t =
  let open Deferred.Let_syntax in
  match%map lift_error_set m with
  | Ok ((), errors) ->
      errors
  | Error errors ->
      errors

module List = struct
  let rec iter ls ~f =
    let open T.Let_syntax in
    match ls with
    | [] ->
        return ()
    | h :: t ->
        let%bind () = f h in
        iter t ~f

  let rec map ls ~f =
    let open T.Let_syntax in
    match ls with
    | [] ->
        return []
    | h :: t ->
        let%bind h' = f h in
        let%map t' = map t ~f in
        h' :: t'

  let rec fold ls ~init ~f =
    let open T.Let_syntax in
    match ls with
    | [] ->
        return init
    | h :: t ->
        let%bind init' = f init h in
        fold t ~init:init' ~f

  let rec fold_left_while ls ~init ~f =
    let open T.Let_syntax in
    match ls with
    | [] ->
        return init
    | h :: t -> (
        match%bind f init h with
        | `Stop init' ->
            return init'
        | `Continue init' ->
            fold_left_while t ~init:init' ~f )

  let iteri ls ~f =
    let open T.Let_syntax in
    let%map _ =
      fold ls ~init:0 ~f:(fun i x ->
          let%map () = f i x in
          i + 1 )
    in
    ()
end

let%test_module "malleable error unit tests" =
  ( module struct
    (* we derive custom equality and comparisions for our result type, as the
   * default behavior of ppx_assert is to use polymorphic equality and comparisons
   * for results (as to why, I have no clue) *)
    type 'a inner = ('a Result_accumulator.t, Hard_fail.t) Result.t
    [@@deriving sexp_of]

    let equal_inner equal a b =
      match (a, b) with
      | Ok a', Ok b' ->
          Result_accumulator.equal equal a' b'
      | Error a', Error b' ->
          Hard_fail.equal a' b'
      | _ ->
          false

    let compare_inner compare a b =
      match (a, b) with
      | Ok a', Ok b' ->
          Result_accumulator.compare compare a' b'
      | Error a', Error b' ->
          Hard_fail.compare a' b'
      | Ok _, Error _ ->
          -1
      | Error _, Ok _ ->
          1

    let%test_unit "malleable error test 1: completes int computation when no \
                   errors" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          let open Deferred.Let_syntax in
          let%bind actual =
            let open T.Let_syntax in
            let f nm =
              let%bind n = nm in
              return (n + 1)
            in
            f (f (f (f (f (return 0)))))
          in
          let%map expected = T.return 5 in
          [%test_eq: int inner] ~equal:(equal_inner Int.equal) actual expected
      )

    let%test_unit "malleable error test 2: completes string computation when \
                   no errors" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          let open Deferred.Let_syntax in
          let%bind actual =
            let open T.Let_syntax in
            let%bind () = return () in
            return "123"
          in
          let%map expected = T.return "123" in
          [%test_eq: string inner] ~equal:(equal_inner String.equal) actual
            expected )

    let%test_unit "malleable error test 3: ok result that accumulates soft \
                   errors" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          let open Deferred.Let_syntax in
          let%map actual =
            let open T.Let_syntax in
            let%bind () = soft_error ~value:() (Error.of_string "a") in
            soft_error ~value:"123" (Error.of_string "b")
          in
          let expected =
            let errors =
              Base.List.map ["a"; "b"]
                ~f:(Fn.compose Test_error.internal_error Error.of_string)
            in
            Result.return
              { Result_accumulator.computation_result= "123"
              ; soft_errors=
                  {Error_accumulator.empty with from_current_context= errors}
              }
          in
          [%test_eq: string inner] ~equal:(equal_inner String.equal) actual
            expected )

    let%test_unit "malleable error test 4: do a basic hard error" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          let open Deferred.Let_syntax in
          let%map actual =
            let open T.Let_syntax in
            let%bind () = return () in
            hard_error (Error.of_string "xyz")
          in
          let expected =
            Result.fail
              { Hard_fail.hard_errors=
                  Error_accumulator.singleton
                    (Test_error.internal_error (Error.of_string "xyz"))
              ; soft_errors= Error_accumulator.empty }
          in
          [%test_eq: string inner] ~equal:(equal_inner String.equal) actual
            expected )

    let%test_unit "malleable error test 5: hard error that accumulates a soft \
                   error" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          let open Deferred.Let_syntax in
          let%map actual =
            let open T.Let_syntax in
            let%bind () = soft_error ~value:() (Error.of_string "a") in
            let%bind () = hard_error (Error.of_string "xyz") in
            return "hello world"
          in
          let expected =
            Result.fail
              { Hard_fail.hard_errors=
                  Error_accumulator.singleton
                    (Test_error.internal_error (Error.of_string "xyz"))
              ; soft_errors=
                  Error_accumulator.singleton
                    (Test_error.internal_error (Error.of_string "a")) }
          in
          [%test_eq: string inner] ~equal:(equal_inner String.equal) actual
            expected )

    let%test_unit "malleable error test 6: hard error with multiple soft \
                   errors accumulating" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          let open Deferred.Let_syntax in
          let%map actual =
            let open T.Let_syntax in
            let%bind () = soft_error ~value:() (Error.of_string "a") in
            let%bind () = soft_error ~value:() (Error.of_string "b") in
            hard_error (Error.of_string "xyz")
          in
          let expected =
            Result.fail
              { Hard_fail.hard_errors=
                  Error_accumulator.singleton
                    (Test_error.internal_error (Error.of_string "xyz"))
              ; soft_errors=
                  { Error_accumulator.empty with
                    from_current_context=
                      [ Test_error.internal_error (Error.of_string "b")
                      ; Test_error.internal_error (Error.of_string "a") ] } }
          in
          [%test_eq: string inner] ~equal:(equal_inner String.equal) actual
            expected )
  end )
