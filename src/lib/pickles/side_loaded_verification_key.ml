open Core_kernel
open Pickles_types
open Common
open Import
module V = Pickles_base.Side_loaded_verification_key

include (
  V :
    module type of V
      with module Width := V.Width
       and module Domains := V.Domains )

let bits = V.bits

let input_size ~of_int ~add ~mul w =
  let open Composition_types in
  (* This should be an affine function in [a]. *)
  let size a =
    let (T (typ, conv)) =
      Impls.Step.input ~branching:a ~wrap_rounds:Backend.Tock.Rounds.n
    in
    Impls.Step.Data_spec.size [ typ ]
  in
  let f0 = size Nat.N0.n in
  let slope = size Nat.N1.n - f0 in
  add (of_int f0) (mul (of_int slope) w)

module Width : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t = V.Width.Stable.V1.t
      [@@deriving sexp, equal, compare, hash, yojson]
    end
  end]

  val of_int_exn : int -> t

  val to_int : t -> int

  val to_bits : t -> bool list

  val zero : t

  open Impls.Step

  module Checked : sig
    type t

    val to_field : t -> Field.t

    val to_bits : t -> Boolean.var list
  end

  val typ : (Checked.t, t) Typ.t

  module Max = Nat.N2

  module Max_vector : Vector.With_version(Max).S

  module Max_at_most : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type 'a t = ('a, Max.n) At_most.t
        [@@deriving compare, sexp, yojson, hash, equal]
      end
    end]
  end

  module Length : Nat.Add.Intf_transparent
end = struct
  include V.Width
  open Impls.Step

  module Checked = struct
    (* A "width" is represented by a 4 bit integer. *)
    type t = (Boolean.var, Length.n) Vector.t

    let to_field : t -> Field.t = Fn.compose Field.project Vector.to_list

    let to_bits = Vector.to_list
  end

  let typ : (Checked.t, t) Typ.t =
    Typ.transport
      (Vector.typ Boolean.typ Length.n)
      ~there:(fun x ->
        let x = to_int x in
        Vector.init Length.n ~f:(fun i -> (x lsr i) land 1 = 1))
      ~back:(fun v ->
        Vector.foldi v ~init:0 ~f:(fun i acc b ->
            if b then acc lor (1 lsl i) else acc)
        |> of_int_exn)
end

module Domain = struct
  type 'a t = Pow_2_roots_of_unity of 'a [@@deriving sexp]

  let log2_size (Pow_2_roots_of_unity x) = x
end

module Domains = struct
  include V.Domains

  let typ =
    let open Impls.Step in
    let dom =
      Typ.transport Typ.field
        ~there:(fun (Plonk_checks.Domain.Pow_2_roots_of_unity n) ->
          Field.Constant.of_int n)
        ~back:(fun _ -> assert false)
      |> Typ.transport_var
           ~there:(fun (Domain.Pow_2_roots_of_unity n) -> n)
           ~back:(fun n -> Domain.Pow_2_roots_of_unity n)
    in
    Typ.of_hlistable [ dom ] ~var_to_hlist:to_hlist ~value_to_hlist:to_hlist
      ~var_of_hlist:of_hlist ~value_of_hlist:of_hlist
end

(* TODO: Probably better to have these match the step rounds. *)
let max_domains = { Domains.h = Domain.Pow_2_roots_of_unity 20 }

let max_domains_with_x =
  let conv (Domain.Pow_2_roots_of_unity n) =
    Plonk_checks.Domain.Pow_2_roots_of_unity n
  in
  let x =
    Plonk_checks.Domain.Pow_2_roots_of_unity
      (Int.ceil_log2
         (input_size ~of_int:Fn.id ~add:( + ) ~mul:( * )
            (Nat.to_int Width.Max.n)))
  in
  { Ds.h = conv max_domains.h; x }

module Vk = struct
  type t = (Impls.Wrap.Verification_key.t[@sexp.opaque]) [@@deriving sexp]

  let to_yojson _ = `String "opaque"

  let of_yojson _ = Error "Vk: yojson not supported"

  let hash _ = Unit.hash ()

  let hash_fold_t s _ = Unit.hash_fold_t s ()

  let equal _ _ = true

  let compare _ _ = 0
end

module R = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Backend.Tock.Curve.Affine.Stable.V1.t Repr.Stable.V1.t

      let to_latest = Fn.id
    end
  end]
end

[%%versioned_binable
module Stable = struct
  module V1 = struct
    type t = (Backend.Tock.Curve.Affine.t, Vk.t) Poly.Stable.V1.t
    [@@deriving sexp, equal, compare, hash, yojson]

    let to_latest = Fn.id

    include Binable.Of_binable
              (R.Stable.V1)
              (struct
                type nonrec t = t

                module Repr_conv = struct
                  include Vk

                  let of_repr
                      { Repr.Stable.V1.step_data; max_width; wrap_index = c } :
                      Impls.Wrap.Verification_key.t =
                    let d = Common.wrap_domains.h in
                    let log2_size = Import.Domain.log2_size d in
                    let max_quot_size =
                      Common.max_quot_size_int (Import.Domain.size d)
                    in
                    { domain =
                        { log_size_of_group = log2_size
                        ; group_gen =
                            Backend.Tock.Field.domain_generator log2_size
                        }
                    ; max_poly_size = 1 lsl Nat.to_int Backend.Tock.Rounds.n
                    ; max_quot_size
                    ; urs = Backend.Tock.Keypair.load_urs ()
                    ; evals =
                        Plonk_verification_key_evals.map c ~f:(fun unshifted ->
                            { Marlin_plonk_bindings.Types.Poly_comm.shifted =
                                None
                            ; unshifted =
                                Array.of_list_map unshifted ~f:(fun x ->
                                    Or_infinity.Finite x)
                            })
                    ; shifts = Common.tock_shifts ~log2_size
                    }
                end

                let to_binable
                    { Poly.step_data; max_width; wrap_index; wrap_vk = _ } =
                  { Repr.Stable.V1.step_data; max_width; wrap_index }

                let of_binable
                    ( { Repr.Stable.V1.step_data; max_width; wrap_index = c } as
                    t ) =
                  { Poly.step_data
                  ; max_width
                  ; wrap_index = c
                  ; wrap_vk = Some (Repr_conv.of_repr t)
                  }
              end)
  end
end]

let dummy : t =
  { step_data = At_most.[]
  ; max_width = Width.zero
  ; wrap_index =
      (let g = [ Backend.Tock.Curve.(to_affine_exn one) ] in
       { sigma_comm_0 = g
       ; sigma_comm_1 = g
       ; sigma_comm_2 = g
       ; ql_comm = g
       ; qr_comm = g
       ; qo_comm = g
       ; qm_comm = g
       ; qc_comm = g
       ; rcm_comm_0 = g
       ; rcm_comm_1 = g
       ; rcm_comm_2 = g
       ; psm_comm = g
       ; add_comm = g
       ; mul1_comm = g
       ; mul2_comm = g
       ; emul1_comm = g
       ; emul2_comm = g
       ; emul3_comm = g
       })
  ; wrap_vk = None
  }

module Checked = struct
  open Step_main_inputs
  open Impl

  type t =
    { step_domains : (Field.t Domain.t Domains.t, Max_branches.n) Vector.t
    ; step_widths : (Width.Checked.t, Max_branches.n) Vector.t
    ; max_width : Width.Checked.t
    ; wrap_index : Inner_curve.t array Plonk_verification_key_evals.t
    ; num_branches : (Boolean.var, Max_branches.Log2.n) Vector.t
    }
  [@@deriving hlist, fields]

  let to_input =
    let open Random_oracle_input in
    let map_reduce t ~f = Array.map t ~f |> Array.reduce_exn ~f:append in
    fun { step_domains; step_widths; max_width; wrap_index; num_branches } :
        _ Random_oracle_input.t ->
      List.reduce_exn ~f:append
        [ map_reduce (Vector.to_array step_domains) ~f:(fun { Domains.h } ->
              map_reduce [| h |] ~f:(fun (Domain.Pow_2_roots_of_unity x) ->
                  bitstring (Field.unpack x ~length:max_log2_degree)))
        ; Array.map (Vector.to_array step_widths) ~f:Width.Checked.to_bits
          |> bitstrings
        ; bitstring (Width.Checked.to_bits max_width)
        ; wrap_index_to_input
            (Array.concat_map
               ~f:(Fn.compose Array.of_list Inner_curve.to_field_elements))
            wrap_index
        ; bitstring (Vector.to_list num_branches)
        ]
end

let%test_unit "input_size" =
  List.iter
    (List.range 0 (Nat.to_int Width.Max.n) ~stop:`inclusive ~start:`inclusive)
    ~f:(fun n ->
      [%test_eq: int]
        (input_size ~of_int:Fn.id ~add:( + ) ~mul:( * ) n)
        (let (T a) = Nat.of_int n in
         let (T (typ, conv)) =
           Impls.Step.input ~branching:a ~wrap_rounds:Backend.Tock.Rounds.n
         in
         Impls.Step.Data_spec.size [ typ ]))

let typ : (Checked.t, t) Impls.Step.Typ.t =
  let open Step_main_inputs in
  let open Impl in
  Typ.of_hlistable
    [ Vector.typ Domains.typ Max_branches.n
    ; Vector.typ Width.typ Max_branches.n
    ; Width.typ
    ; Plonk_verification_key_evals.typ
        (Typ.array Inner_curve.typ
           ~length:
             (index_commitment_length ~max_degree:Max_degree.wrap
                Common.wrap_domains.h))
    ; Vector.typ Boolean.typ Max_branches.Log2.n
    ]
    ~var_to_hlist:Checked.to_hlist ~var_of_hlist:Checked.of_hlist
    ~value_of_hlist:(fun _ ->
      failwith "Side_loaded_verification_key: value_of_hlist")
    ~value_to_hlist:(fun { Poly.step_data; wrap_index; max_width; _ } ->
      [ At_most.extend_to_vector
          (At_most.map step_data ~f:fst)
          dummy_domains Max_branches.n
      ; At_most.extend_to_vector
          (At_most.map step_data ~f:snd)
          dummy_width Max_branches.n
      ; max_width
      ; Plonk_verification_key_evals.map ~f:Array.of_list wrap_index
      ; (let n = At_most.length step_data in
         Vector.init Max_branches.Log2.n ~f:(fun i -> (n lsr i) land 1 = 1))
      ])
