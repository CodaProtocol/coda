open Core

module Context = struct
  type ('message, 'pk) t = {message: 'message; public_key: 'pk}
  [@@deriving sexp, hlist]
end

module Evaluation = struct
  module Discrete_log_equality = struct
    module Poly = struct
      [%%versioned
      module Stable = struct
        module V1 = struct
          type 'scalar t = {c: 'scalar; s: 'scalar} [@@deriving sexp]

          let to_latest = Fn.id
        end
      end]
    end
  end

  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('group, 'dleq) t =
          {discrete_log_equality: 'dleq; scaled_message_hash: 'group}
        [@@deriving sexp]
      end
    end]
  end
end

module Make
    (Impl : Snarky_backendless.Snark_intf.S) (Scalar : sig
        type t [@@deriving equal, sexp]

        val random : unit -> t

        val add : t -> t -> t

        val mul : t -> t -> t

        type var

        val typ : (var, t) Impl.Typ.t

        module Checked : sig
          open Impl

          val to_bits : var -> Boolean.var Bitstring_lib.Bitstring.Lsb_first.t

          module Assert : sig
            val equal : var -> var -> (unit, _) Checked.t
          end
        end
    end) (Group : sig
      type t [@@deriving sexp]

      val add : t -> t -> t

      val negate : t -> t

      val scale : t -> Scalar.t -> t

      val generator : t

      type var

      val typ : (var, t) Impl.Typ.t

      module Checked :
        Snarky_curves.Weierstrass_checked_intf
        with module Impl := Impl
         and type unchecked := t
         and type t = var
    end) (Message : sig
      open Impl

      type value [@@deriving sexp]

      type var

      val typ : (var, value) Typ.t

      (* This hash function can be merely collision-resistant *)

      val hash_to_group : value -> Group.t

      module Checked : sig
        val hash_to_group : var -> (Group.var, _) Checked.t
      end
    end) (Output_hash : sig
      type t

      type var

      (* I believe this has to be a random oracle *)

      val hash : Message.value -> Group.t -> t

      module Checked : sig
        val hash : Message.var -> Group.var -> (var, _) Impl.Checked.t
      end
    end) (Hash : sig
      (* I believe this has to be a random oracle *)

      val hash_for_proof :
        Message.value -> Group.t -> Group.t -> Group.t -> Scalar.t

      module Checked : sig
        val hash_for_proof :
             Message.var
          -> Group.var
          -> Group.var
          -> Group.var
          -> (Scalar.var, _) Impl.Checked.t
      end
    end) : sig
  module Public_key : sig
    type t = Group.t

    type var = Group.var
  end

  module Private_key : sig
    type t = Scalar.t

    type var = Scalar.var
  end

  module Context : sig
    type t = (Message.value, Public_key.t) Context.t [@@deriving sexp]

    type var = (Message.var, Public_key.var) Context.t

    val typ : (var, t) Impl.Typ.t
  end

  module Evaluation : sig
    type t =
      ( Group.t
      , Scalar.t Evaluation.Discrete_log_equality.Poly.t )
      Evaluation.Poly.t
    [@@deriving sexp]

    type var

    val typ : (var, t) Impl.Typ.t

    val create : Private_key.t -> Message.value -> t

    val verified_output : t -> Context.t -> Output_hash.t option

    module Checked : sig
      val verified_output :
           (module Group.Checked.Shifted.S with type t = 'shifted)
        -> var
        -> Context.var
        -> (Output_hash.var, _) Impl.Checked.t
    end
  end
end = struct
  module Public_key = Group

  module Context = struct
    type t = (Message.value, Public_key.t) Context.t [@@deriving sexp]

    type var = (Message.var, Public_key.var) Context.t

    let typ =
      Impl.Typ.of_hlistable
        [Message.typ; Public_key.typ]
        ~var_to_hlist:Context.to_hlist ~var_of_hlist:Context.of_hlist
        ~value_to_hlist:Context.to_hlist ~value_of_hlist:Context.of_hlist
  end

  module Private_key = Scalar

  module Evaluation = struct
    module Discrete_log_equality = struct
      type 'scalar t_ = 'scalar Evaluation.Discrete_log_equality.Poly.t =
        {c: 'scalar; s: 'scalar}
      [@@deriving sexp, hlist]

      type t = Scalar.t t_ [@@deriving sexp]

      type var = Scalar.var t_

      open Impl

      let typ : (var, t) Typ.t =
        Typ.of_hlistable [Scalar.typ; Scalar.typ] ~var_to_hlist:t__to_hlist
          ~var_of_hlist:t__of_hlist ~value_to_hlist:t__to_hlist
          ~value_of_hlist:t__of_hlist
    end

    type ('group, 'dleq) t_ = ('group, 'dleq) Evaluation.Poly.t =
      {discrete_log_equality: 'dleq; scaled_message_hash: 'group}
    [@@deriving sexp]

    type t = (Group.t, Discrete_log_equality.t) t_ [@@deriving sexp]

    type var = (Group.var, Discrete_log_equality.var) t_

    let typ : (var, t) Impl.Typ.t =
      let open Snarky_backendless.H_list in
      Impl.Typ.of_hlistable
        [Discrete_log_equality.typ; Group.typ]
        ~var_to_hlist:(fun {discrete_log_equality; scaled_message_hash} ->
          [discrete_log_equality; scaled_message_hash] )
        ~value_to_hlist:(fun {discrete_log_equality; scaled_message_hash} ->
          [discrete_log_equality; scaled_message_hash] )
        ~value_of_hlist:(fun [discrete_log_equality; scaled_message_hash] ->
          {discrete_log_equality; scaled_message_hash} )
        ~var_of_hlist:(fun [discrete_log_equality; scaled_message_hash] ->
          {discrete_log_equality; scaled_message_hash} )

    let create (k : Private_key.t) message : t =
      let public_key = Group.scale Group.generator k in
      let message_hash = Message.hash_to_group message in
      let discrete_log_equality : Discrete_log_equality.t =
        let r = Scalar.random () in
        let c =
          Hash.hash_for_proof message public_key
            Group.(scale generator r)
            Group.(scale message_hash r)
        in
        {c; s= Scalar.(add r (mul k c))}
      in
      {discrete_log_equality; scaled_message_hash= Group.scale message_hash k}

    let verified_output
        ({scaled_message_hash; discrete_log_equality= {c; s}} : t)
        ({message; public_key} : Context.t) =
      let g = Group.generator in
      let ( + ) = Group.add in
      let ( * ) s g = Group.scale g s in
      let message_hash = Message.hash_to_group message in
      let dleq =
        Scalar.equal c
          (Hash.hash_for_proof message public_key
             ((s * g) + (c * Group.negate public_key))
             ((s * message_hash) + (c * Group.negate scaled_message_hash)))
      in
      if dleq then Some (Output_hash.hash message scaled_message_hash)
      else None

    module Checked = struct
      let verified_output (type shifted)
          ((module Shifted) as shifted :
            (module Group.Checked.Shifted.S with type t = shifted))
          ({scaled_message_hash; discrete_log_equality= {c; s}} : var)
          ({message; public_key} : Context.var) =
        let open Impl.Checked in
        let%bind () =
          let%bind a =
            (* s * g - c * public_key *)
            let%bind sg =
              Group.Checked.scale_known shifted Group.generator
                (Scalar.Checked.to_bits s) ~init:Shifted.zero
            in
            Group.Checked.(
              scale shifted (negate public_key) (Scalar.Checked.to_bits c)
                ~init:sg)
            >>= Shifted.unshift_nonzero
          and b =
            (* s * H(m) - c * scaled_message_hash *)
            let%bind sx =
              let%bind message_hash = Message.Checked.hash_to_group message in
              Group.Checked.scale shifted message_hash
                (Scalar.Checked.to_bits s) ~init:Shifted.zero
            in
            Group.Checked.(
              scale shifted
                (negate scaled_message_hash)
                (Scalar.Checked.to_bits c) ~init:sx)
            >>= Shifted.unshift_nonzero
          in
          Hash.Checked.hash_for_proof message public_key a b
          >>= Scalar.Checked.Assert.equal c
        in
        (* TODO: This could just hash (message_hash, message_hash^k) instead
          if it were cheaper *)
        Output_hash.Checked.hash message scaled_message_hash
    end
  end
end

open Core

module Bigint_scalar
    (Impl : Snarky_backendless.Snark_intf.S) (M : sig
        val modulus : Bigint.t

        val random : unit -> Bigint.t
    end) =
struct
  let pack bs =
    let pack_char bs =
      Char.of_int_exn
        (List.foldi bs ~init:0 ~f:(fun i acc b ->
             if b then acc lor (1 lsl i) else acc ))
    in
    String.of_char_list (List.map ~f:pack_char (List.chunks_of ~length:8 bs))
    |> Z.of_bits |> Bigint.of_zarith_bigint

  include Bigint
  include M

  let gen = gen_incl zero (modulus - one)

  let test_bit t i = shift_right t i land one = one

  let add x y =
    let z = x + y in
    if z < modulus then z else z - modulus

  let%test_unit "add is correct" =
    Quickcheck.test (Quickcheck.Generator.tuple2 gen gen) ~f:(fun (x, y) ->
        assert (equal (add x y) ((x + y) % modulus)) )

  let mul x y = x * y % modulus

  let length_in_bits = Z.log2up (Bigint.to_zarith_bigint (modulus - one))

  let to_bits n = List.init length_in_bits ~f:(test_bit n)

  let of_bits bs =
    List.fold_left bs ~init:(zero, one) ~f:(fun (acc, pt) b ->
        ((if b then add acc pt else acc), add pt pt) )
    |> fst

  let%test_unit "of_bits . to_bits = identity" =
    Quickcheck.test gen ~f:(fun x -> assert (equal x (of_bits (to_bits x))))

  open Impl

  type var = Boolean.var list

  let typ : (var, t) Typ.t =
    let open Typ in
    transport
      (list ~length:length_in_bits Boolean.typ)
      ~there:(fun n ->
        List.init length_in_bits ~f:(Z.testbit (to_zarith_bigint n)) )
      ~back:pack

  module Checked = struct
    let equal = Bitstring_checked.equal

    module Assert = struct
      let equal = Bitstring_checked.Assert.equal
    end
  end
end
