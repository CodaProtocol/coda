open Core_kernel
open Pickles_types
module Max_state_size = Nat.N8

module V = struct
  (* Think about versioning here! These vector types *will* change
     serialization if the numbers above change, and so will require a new
     version number. Thus, it's important that these are modules with new
     versioned types, and not just module aliases to the corresponding vector
     implementation.
  *)
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type 'a t = 'a Vector.Vector_8.Stable.V1.t
      [@@deriving compare, yojson, sexp, hash, equal]
    end
  end]

  type 'a t = 'a Vector.Vector_8.t
  [@@deriving compare, yojson, sexp, hash, equal]

  let map = Vector.map

  let of_list_exn = Vector.Vector_8.of_list_exn

  let to_list = Vector.to_list
end

let () =
  let _f : type a.
      unit -> (a V.t, a Vector.With_length(Max_state_size).t) Type_equal.t =
   fun () -> Type_equal.T
  in
  ()

let typ t = Vector.typ t Max_state_size.n

open Core_kernel

module Value = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t = Snapp_basic.F.Stable.V1.t V.Stable.V1.t
      [@@deriving sexp, equal, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  type t = Snapp_basic.F.t V.t [@@deriving sexp, equal, yojson, hash, compare]

  let () =
    let _f : unit -> (t, Stable.Latest.t) Type_equal.t =
     fun () -> Type_equal.T
    in
    ()
end

let to_input (t : _ V.t) ~f =
  Vector.(reduce_exn (map t ~f) ~f:Random_oracle_input.append)
