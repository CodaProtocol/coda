open Core

type remote_error = {node_id: string; error_message: Logger.Message.t}

(* NB: equality on internal errors ignores timestamp *)
type internal_error =
  { occurrence_time: Time.t sexp_opaque
        [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
  ; error: Error.t
        [@equal
          fun a b ->
            String.equal (Error.to_string_hum a) (Error.to_string_hum b)]
        [@compare
          fun a b ->
            String.compare (Error.to_string_hum a) (Error.to_string_hum b)] }
[@@deriving eq, sexp, compare]

let internal_error error = {occurrence_time= Time.now (); error}

let occurrence_time {occurrence_time; _} = occurrence_time

let compare_time a b = Time.compare (occurrence_time a) (occurrence_time b)

(* currently a flat set of contexts mapped to errors, but perhaps a tree (for nested contexts) is better *)
(* TODO: consider switching to explicit context "enters/exits", recording introduction time upon entrance *)
module Error_accumulator = struct
  type 'error contextualized_errors =
    {introduction_time: Time.t; errors_by_time: 'error list Time.Map.t}
  [@@deriving eq, sexp_of, compare]

  type 'error t =
    { from_current_context: 'error list
    ; contextualized_errors: 'error contextualized_errors String.Map.t }
  [@@deriving eq, sexp_of, compare]

  let empty_contextualized_errors () =
    {introduction_time= Time.now (); errors_by_time= Time.Map.empty}

  let empty =
    {from_current_context= []; contextualized_errors= String.Map.empty}

  let record_errors map context new_errors ~time_of_error =
    String.Map.update map context ~f:(fun errors_opt ->
        let errors =
          Option.value errors_opt ~default:(empty_contextualized_errors ())
        in
        let errors_by_time =
          List.fold new_errors ~init:errors.errors_by_time ~f:(fun acc error ->
              Time.Map.add_multi acc ~key:(time_of_error error) ~data:error )
        in
        {errors with errors_by_time} )

  let error_count {from_current_context; contextualized_errors} =
    let num_current_context = List.length from_current_context in
    let num_contextualized =
      String.Map.fold contextualized_errors ~init:0 ~f:(fun ~key:_ ~data sum ->
          Time.Map.length data.errors_by_time + sum )
    in
    num_current_context + num_contextualized

  let all_errors {from_current_context; contextualized_errors} =
    let context_errors =
      String.Map.data contextualized_errors
      |> List.bind ~f:(fun {errors_by_time; _} -> Time.Map.data errors_by_time)
      |> List.concat
    in
    from_current_context @ context_errors

  let contextualize' context {from_current_context; contextualized_errors}
      ~time_of_error =
    { empty with
      contextualized_errors=
        record_errors contextualized_errors context from_current_context
          ~time_of_error }

  let contextualize = contextualize' ~time_of_error:occurrence_time

  let singleton x = {empty with from_current_context= [x]}

  let of_context_free_list ls = {empty with from_current_context= ls}

  let of_contextualized_list' context ls ~time_of_error =
    { empty with
      contextualized_errors=
        record_errors String.Map.empty context ls ~time_of_error }

  let of_contextualized_list =
    of_contextualized_list' ~time_of_error:occurrence_time

  let add t error =
    {t with from_current_context= error :: t.from_current_context}

  let add_to_context t context error ~time_of_error =
    { t with
      contextualized_errors=
        record_errors t.contextualized_errors context [error] ~time_of_error }

  let map {from_current_context; contextualized_errors} ~f =
    { from_current_context= List.map from_current_context ~f
    ; contextualized_errors=
        String.Map.map contextualized_errors ~f:(fun errors ->
            { errors with
              errors_by_time=
                Time.Map.map errors.errors_by_time ~f:(List.map ~f) } ) }

  (* This only iterates over contextualized errors. You must check errors in the current context manually *)
  let iter_contexts {from_current_context= _; contextualized_errors} ~f =
    let contexts_by_time =
      contextualized_errors |> String.Map.to_alist
      |> List.map ~f:(fun (ctx, errors) -> (errors.introduction_time, ctx))
      |> Time.Map.of_alist_exn
    in
    Time.Map.iter contexts_by_time ~f:(fun context ->
        let {errors_by_time; _} =
          String.Map.find_exn contextualized_errors context
        in
        errors_by_time |> Time.Map.data |> List.concat |> f context )

  let merge a b =
    let from_current_context =
      a.from_current_context @ b.from_current_context
    in
    let contextualized_errors =
      let merge_maps (type a key comparator_witness)
          (map_a : (key, a, comparator_witness) Map.t)
          (map_b : (key, a, comparator_witness) Map.t)
          ~(resolve_conflict : a -> a -> a) :
          (key, a, comparator_witness) Map.t =
        Map.fold map_b ~init:map_a ~f:(fun ~key ~data acc ->
            Map.update acc key ~f:(function
              | None ->
                  data
              | Some data' ->
                  resolve_conflict data' data ) )
      in
      let merge_contextualized_errors a_errors b_errors =
        { introduction_time=
            Time.min a_errors.introduction_time b_errors.introduction_time
        ; errors_by_time=
            merge_maps a_errors.errors_by_time b_errors.errors_by_time
              ~resolve_conflict:( @ ) }
      in
      merge_maps a.contextualized_errors b.contextualized_errors
        ~resolve_conflict:merge_contextualized_errors
    in
    {from_current_context; contextualized_errors}

  let combine = List.fold ~init:empty ~f:merge

  let partition {from_current_context; contextualized_errors} ~f =
    let from_current_context_a, from_current_context_b =
      List.partition_tf from_current_context ~f
    in
    let contextualized_errors_a, contextualized_errors_b =
      let partition_map (type key a w) (cmp : (key, w) Map.comparator)
          (map : (key, a, w) Map.t) ~(f : a -> a * a) :
          (key, a, w) Map.t * (key, a, w) Map.t =
        Map.fold map
          ~init:(Map.empty cmp, Map.empty cmp)
          ~f:(fun ~key ~data (left, right) ->
            let l, r = f data in
            (Map.add_exn left ~key ~data:l, Map.add_exn right ~key ~data:r) )
      in
      partition_map
        (module String)
        contextualized_errors
        ~f:(fun ctx_errors ->
          let l, r =
            partition_map
              (module Time)
              ctx_errors.errors_by_time ~f:(List.partition_tf ~f)
          in
          ( {ctx_errors with errors_by_time= l}
          , {ctx_errors with errors_by_time= r} ) )
    in
    let a =
      { from_current_context= from_current_context_a
      ; contextualized_errors= contextualized_errors_a }
    in
    let b =
      { from_current_context= from_current_context_b
      ; contextualized_errors= contextualized_errors_b }
    in
    (a, b)
end

module Set = struct
  type nonrec 'error t =
    { soft_errors: 'error Error_accumulator.t
    ; hard_errors: 'error Error_accumulator.t }

  let empty =
    {soft_errors= Error_accumulator.empty; hard_errors= Error_accumulator.empty}

  let max_severity {soft_errors; hard_errors} =
    let num_soft = Error_accumulator.error_count soft_errors in
    let num_hard = Error_accumulator.error_count hard_errors in
    if num_hard > 0 then `Hard else if num_soft > 0 then `Soft else `None

  let all_errors {soft_errors; hard_errors} =
    Error_accumulator.merge soft_errors hard_errors

  let soft_singleton err =
    {empty with soft_errors= Error_accumulator.singleton err}

  let hard_singleton err =
    {empty with hard_errors= Error_accumulator.singleton err}

  let of_soft_or_error = function
    | Ok () ->
        empty
    | Error err ->
        soft_singleton (internal_error err)

  let of_hard_or_error = function
    | Ok () ->
        empty
    | Error err ->
        hard_singleton (internal_error err)

  let add_soft err t =
    {t with soft_errors= Error_accumulator.add t.soft_errors err}

  let add_hard err t =
    {t with hard_errors= Error_accumulator.add t.soft_errors err}

  let merge a b =
    { soft_errors= Error_accumulator.merge a.soft_errors b.soft_errors
    ; hard_errors= Error_accumulator.merge a.hard_errors b.hard_errors }

  let combine = List.fold_left ~init:empty ~f:merge

  let partition {soft_errors; hard_errors} ~f =
    let soft_errors_a, soft_errors_b =
      Error_accumulator.partition soft_errors ~f
    in
    let hard_errors_a, hard_errors_b =
      Error_accumulator.partition hard_errors ~f
    in
    let a = {soft_errors= soft_errors_a; hard_errors= hard_errors_a} in
    let b = {soft_errors= soft_errors_b; hard_errors= hard_errors_b} in
    (a, b)
end
