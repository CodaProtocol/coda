open Core_kernel
open Async_kernel

module T = struct
  type 'a t = [`Active of 'a | `Bootstrapping | `Waiting_for_genesis]

  let return value = `Active value

  let bind x ~(f : 'a -> 'b t) =
    match x with
    | `Active value ->
        f value
    | `Bootstrapping ->
        `Bootstrapping
    | `Waiting_for_genesis ->
        `Waiting_for_genesis

  let map = `Define_using_bind
end

include T
include Monad.Make (T)

module Option = struct
  module T = struct
    type 'a t = 'a option T.t

    let return value = `Active (Some value)

    let bind value_option_status ~f =
      match value_option_status with
      | `Active (Some value_option) ->
          f value_option
      | `Active None ->
          `Active None
      | `Bootstrapping ->
          `Bootstrapping
      | `Waiting_for_genesis ->
          `Waiting_for_genesis

    let map = `Define_using_bind
  end

  include Monad.Make (T)
end

let active = function
  | `Active x ->
      Some x
  | `Bootstrapping | `Waiting_for_genesis ->
      None

let bootstrap_err_msg = "Node is still bootstrapping"

let waiting_err_msg = "Node is waiting for the genesis (or fork) start time"

let active_exn = function
  | `Active x ->
      x
  | `Bootstrapping ->
      failwith bootstrap_err_msg
  | `Waiting_for_genesis ->
      failwith waiting_err_msg

let active_error = function
  | `Active x ->
      Ok x
  | `Bootstrapping ->
      Or_error.error_string bootstrap_err_msg
  | `Waiting_for_genesis ->
      Or_error.error_string waiting_err_msg

let to_deferred_or_error : 'a Deferred.t t -> 'a Deferred.Or_error.t = function
  | `Active x ->
      Deferred.map ~f:Or_error.return x
  | `Bootstrapping ->
      Deferred.Or_error.error_string bootstrap_err_msg
  | `Waiting_for_genesis ->
      Deferred.Or_error.error_string waiting_err_msg

let rec sequence (list : 'a T.t List.t) : 'a List.t T.t =
  match list with
  | [] ->
      return []
  | [participating_state] ->
      bind participating_state ~f:(fun value -> return [value])
  | participating_state :: participating_states ->
      bind participating_state ~f:(fun x ->
          map (sequence participating_states) ~f:(fun sub_result ->
              x :: sub_result ) )
