open Pickles_types
open Core_kernel
module Limbs = Nat.N4

module Constant = struct
  include Limb_vector.Constant.Make (Limbs)

  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t =
        Limb_vector.Constant.Hex64.Stable.V1.t Vector.Vector_4.Stable.V1.t
      [@@deriving compare, sexp, yojson, hash, equal]

      let to_latest = Fn.id
    end
  end]

  (* Force the typechecker to verify that these types are equal. *)
  let () =
    let _f : unit -> (t, Stable.Latest.t) Type_equal.t =
     fun () -> Type_equal.T
    in
    ()

  open Backend

  let to_tick_field x = Tick.Field.of_bits (to_bits x)

  let to_tock_field x = Tock.Field.of_bits (to_bits x)

  let of_tick_field x = of_bits (Tick.Field.to_bits x)
end

module Make (Impl : Snarky_backendless.Snark_intf.Run) = struct
  open Impl

  type t = Field.t

  let to_bits = Field.choose_preimage_var ~length:Field.size_in_bits

  module Unsafe = struct
    let to_bits_unboolean x =
      with_label __LOC__ (fun () ->
          let length = Field.size_in_bits in
          let res =
            exists
              (Typ.list Boolean.typ_unchecked ~length)
              ~compute:As_prover.(fun () -> Field.Constant.unpack (read_var x))
          in
          Field.Assert.equal x (Field.project res) ;
          res )
  end

  let () = assert (Field.size_in_bits < 64 * Nat.to_int Limbs.n)

  module Constant = struct
    include Constant

    let to_bits x = List.take (to_bits x) Field.size_in_bits
  end

  let typ =
    Typ.transport Field.typ
      ~there:(Fn.compose Field.Constant.project Constant.to_bits)
      ~back:(Fn.compose Constant.of_bits Field.Constant.unpack)
end
