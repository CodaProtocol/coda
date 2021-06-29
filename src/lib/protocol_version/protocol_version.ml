(* protocol_version.ml -- use semantic versioning *)

open Core_kernel

[%%versioned
module Stable = struct
  module V1 = struct
    type t = {major: int; minor: int; patch: int} [@@deriving compare, sexp]

    let to_latest = Fn.id
  end
end]

let (current_protocol_version : t option ref) = ref None

let (proposed_protocol_version_opt : t option ref) = ref None

let set_current t = current_protocol_version := Some t

let set_proposed_opt t_opt = proposed_protocol_version_opt := t_opt

(* we set current protocol version on daemon startup, so we should not see errors
   due to current_protocol_version = None in get_current and create_exn
 *)
let get_current () = Option.value_exn !current_protocol_version

let get_proposed_opt () = !proposed_protocol_version_opt

let create_exn ~major ~minor ~patch =
  if major < 0 || minor < 0 || patch < 0 then
    failwith
      "Protocol_version.create: major, minor, and patch must be nonnegative" ;
  {major; minor; patch}

let create_opt ~major ~minor ~patch =
  try Some (create_exn ~major ~minor ~patch) with _ -> None

let zero = create_exn ~major:0 ~minor:0 ~patch:0

let compatible_with_daemon current_protocol_version =
  Int.equal current_protocol_version.major (get_current ()).major

let to_string t = sprintf "%u.%u.%u" t.major t.minor t.patch

let is_digit_string s =
  String.fold s ~init:true ~f:(fun accum c -> accum && Char.is_digit c)

let of_string_exn s =
  match String.split s ~on:'.' with
  | [major; minor; patch] ->
      if
        not
          ( is_digit_string major && is_digit_string minor
          && is_digit_string patch )
      then
        failwith
          "Protocol_version.of_string_exn: unexpected nondigits in input" ;
      { major= Int.of_string major
      ; minor= Int.of_string minor
      ; patch= Int.of_string patch }
  | _ ->
      failwith
        "Protocol_version.of_string_exn: expected string of form nn.nn.nn"

let of_string_opt s = try Some (of_string_exn s) with _ -> None

(* when an external transition is deserialized, might contain
   negative numbers
*)
let is_valid t = t.major >= 0 && t.minor >= 0 && t.patch >= 0
