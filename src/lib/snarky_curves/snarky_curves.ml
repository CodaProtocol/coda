module Bignum_bigint = Bigint
open Core_kernel

module type Params_intf = sig
  type field

  val a : field

  val b : field
end

module type Scalar_intf = sig
  type (_, _) typ

  type (_, _) checked

  type boolean_var

  type var

  type t [@@deriving equal, sexp]

  val typ : (var, t) typ

  val length_in_bits : int

  val test_bit : t -> int -> bool

  module Checked : sig
    val equal : var -> var -> (boolean_var, _) checked

    module Assert : sig
      val equal : var -> var -> (unit, _) checked
    end
  end
end

module type Shifted_intf = sig
  type (_, _) checked

  type boolean_var

  type curve_var

  type t

  val zero : t

  val add : t -> curve_var -> (t, _) checked

  (* This is only safe if the result is guaranteed to not be zero. *)

  val unshift_nonzero : t -> (curve_var, _) checked

  val if_ : boolean_var -> then_:t -> else_:t -> (t, _) checked

  module Assert : sig
    val equal : t -> t -> (unit, _) checked
  end
end

module type Weierstrass_checked_intf = sig
  module Impl : Snarky_backendless.Snark_intf.S

  open Impl

  type unchecked

  type t

  val typ : (t, unchecked) Typ.t

  module Shifted : sig
    module type S =
      Shifted_intf
      with type ('a, 'b) checked := ('a, 'b) Checked.t
       and type curve_var := t
       and type boolean_var := Boolean.var

    type 'a m = (module S with type t = 'a)

    val create : unit -> ((module S), _) Checked.t
  end

  val negate : t -> t

  val constant : unchecked -> t

  val add_unsafe :
    t -> t -> ([`I_thought_about_this_very_carefully of t], _) Checked.t

  val if_ : Boolean.var -> then_:t -> else_:t -> (t, _) Checked.t

  val double : t -> (t, _) Checked.t

  val if_value : Boolean.var -> then_:unchecked -> else_:unchecked -> t

  val scale :
       's Shifted.m
    -> t
    -> Boolean.var Bitstring_lib.Bitstring.Lsb_first.t
    -> init:'s
    -> ('s, _) Checked.t

  val scale_known :
       's Shifted.m
    -> unchecked
    -> Boolean.var Bitstring_lib.Bitstring.Lsb_first.t
    -> init:'s
    -> ('s, _) Checked.t

  val sum : 's Shifted.m -> t list -> init:'s -> ('s, _) Checked.t

  module Assert : sig
    val on_curve : t -> (unit, _) Checked.t

    val equal : t -> t -> (unit, _) Checked.t
  end
end

module Make_weierstrass_checked
    (F : Snarky_field_extensions.Intf.S) (Scalar : sig
        type t

        val of_int : int -> t
    end) (Curve : sig
      type t

      val random : unit -> t

      val to_affine_exn : t -> F.Unchecked.t * F.Unchecked.t

      val of_affine : F.Unchecked.t * F.Unchecked.t -> t

      val double : t -> t

      val ( + ) : t -> t -> t

      val negate : t -> t

      val scale : t -> Scalar.t -> t
    end)
    (Params : Params_intf with type field := F.Unchecked.t) (Override : sig
        val add :
          (F.t * F.t -> F.t * F.t -> (F.t * F.t, _) F.Impl.Checked.t) option
    end) :
  Weierstrass_checked_intf
  with module Impl := F.Impl
   and type unchecked := Curve.t
   and type t = F.t * F.t = struct
  open F.Impl

  type t = F.t * F.t

  let assert_on_curve (x, y) =
    let open F in
    let%bind x2 = square x in
    let%bind x3 = x2 * x in
    let%bind ax = constant Params.a * x in
    assert_square y (x3 + ax + constant Params.b)

  let typ : (t, Curve.t) Typ.t =
    let unchecked =
      Typ.transport
        Typ.(tuple2 F.typ F.typ)
        ~there:Curve.to_affine_exn ~back:Curve.of_affine
    in
    {unchecked with check= assert_on_curve}

  let negate ((x, y) : t) : t = (x, F.negate y)

  let constant (t : Curve.t) : t =
    let x, y = Curve.to_affine_exn t in
    F.(constant x, constant y)

  let assert_equal (x1, y1) (x2, y2) =
    let%map () = F.assert_equal x1 x2 and () = F.assert_equal y1 y2 in
    ()

  module Assert = struct
    let on_curve = assert_on_curve

    let equal = assert_equal
  end

  open Let_syntax

  let%snarkydef add' ~div (ax, ay) (bx, by) =
    let open F in
    let%bind lambda = div (by - ay) (bx - ax) in
    let%bind cx =
      exists typ
        ~compute:
          (let open As_prover in
          let open Let_syntax in
          let%map ax = read typ ax
          and bx = read typ bx
          and lambda = read typ lambda in
          Unchecked.(square lambda - (ax + bx)))
    in
    let%bind () =
      (* lambda^2 = cx + ax + bx
            cx = lambda^2 - (ax + bc)
        *)
      assert_square lambda F.(cx + ax + bx)
    in
    let%bind cy =
      exists typ
        ~compute:
          (let open As_prover in
          let open Let_syntax in
          let%map ax = read typ ax
          and ay = read typ ay
          and cx = read typ cx
          and lambda = read typ lambda in
          Unchecked.((lambda * (ax - cx)) - ay))
    in
    let%map () = assert_r1cs lambda (ax - cx) (cy + ay) in
    (cx, cy)

  let add' ~div p1 p2 =
    match Override.add with Some add -> add p1 p2 | None -> add' ~div p1 p2

  (* This function MUST NOT be called UNLESS you are certain the two points
   on which it is called are not equal. If it is called on equal points,
   the prover can return almost any curve point they want to from this function. *)
  let add_unsafe p q =
    let%map r = add' ~div:F.div_unsafe p q in
    `I_thought_about_this_very_carefully r

  let add_exn p q = add' ~div:(fun x y -> F.inv_exn y >>= F.(( * ) x)) p q

  (* TODO-someday: Make it so this doesn't have to compute both branches *)
  let if_ b ~then_:(tx, ty) ~else_:(ex, ey) =
    let%map x = F.if_ b ~then_:tx ~else_:ex
    and y = F.if_ b ~then_:ty ~else_:ey in
    (x, y)

  module Shifted = struct
    module type S =
      Shifted_intf
      with type ('a, 'b) checked := ('a, 'b) Checked.t
       and type curve_var := t
       and type boolean_var := Boolean.var

    type 'a m = (module S with type t = 'a)

    module Make (M : sig
      val shift : t
    end) : S = struct
      open M

      type nonrec t = t

      let zero = shift

      let if_ = if_

      let unshift_nonzero shifted = add_exn (negate shift) shifted

      let add shifted x = add_exn shifted x

      module Assert = struct
        let equal = assert_equal
      end
    end

    let create () : ((module S), _) Checked.t =
      let%map shift =
        exists typ ~compute:As_prover.(map (return ()) ~f:Curve.random)
      in
      let module M = Make (struct
        let shift = shift
      end) in
      (module M : S)
  end

  let%snarkydef double (ax, ay) =
    let open F in
    let%bind x_squared = square ax in
    let%bind lambda =
      exists typ
        ~compute:
          As_prover.(
            map2 (read typ x_squared) (read typ ay) ~f:(fun x_squared ay ->
                let open F.Unchecked in
                (x_squared + x_squared + x_squared + Params.a) * inv (ay + ay)
            ))
    in
    let%bind bx =
      exists typ
        ~compute:
          As_prover.(
            map2 (read typ lambda) (read typ ax) ~f:(fun lambda ax ->
                let open F.Unchecked in
                square lambda - (ax + ax) ))
    in
    let%bind by =
      exists typ
        ~compute:
          (let open As_prover in
          let open Let_syntax in
          let%map lambda = read typ lambda
          and ax = read typ ax
          and ay = read typ ay
          and bx = read typ bx in
          F.Unchecked.((lambda * (ax - bx)) - ay))
    in
    let two = Field.of_int 2 in
    let%map () =
      assert_r1cs (F.scale lambda two) ay
        (F.scale x_squared (Field.of_int 3) + F.constant Params.a)
    and () = assert_square lambda (bx + F.scale ax two)
    and () = assert_r1cs lambda (ax - bx) (by + ay) in
    (bx, by)

  let if_value (cond : Boolean.var) ~then_ ~else_ =
    let x1, y1 = Curve.to_affine_exn then_ in
    let x2, y2 = Curve.to_affine_exn else_ in
    let cond = (cond :> Field.Var.t) in
    let choose a1 a2 =
      let open Field.Checked in
      F.map2_ a1 a2 ~f:(fun a1 a2 ->
          (a1 * cond) + (a2 * (Field.Var.constant Field.one - cond)) )
    in
    (choose x1 x2, choose y1 y2)

  let%snarkydef scale (type shifted)
      (module Shifted : Shifted.S with type t = shifted) t
      (c : Boolean.var Bitstring_lib.Bitstring.Lsb_first.t) ~(init : shifted) :
      (shifted, _) Checked.t =
    let c = Bitstring_lib.Bitstring.Lsb_first.to_list c in
    let open Let_syntax in
    let rec go i bs0 acc pt =
      match bs0 with
      | [] ->
          return acc
      | b :: bs ->
          let%bind acc' =
            with_label (sprintf "acc_%d" i)
              (let%bind add_pt = Shifted.add acc pt in
               let don't_add_pt = acc in
               Shifted.if_ b ~then_:add_pt ~else_:don't_add_pt)
          and pt' = double pt in
          go (i + 1) bs acc' pt'
    in
    go 0 c init t

  (* This 'looks up' a field element from a lookup table of size 2^2 = 4 with
   a 2 bit index.  See https://github.com/zcash/zcash/issues/2234#issuecomment-383736266 for
   a discussion of this trick.
*)
  let lookup_point (b0, b1) (t1, t2, t3, t4) =
    let%map b0_and_b1 = Boolean.( && ) b0 b1 in
    let lookup_one (a1, a2, a3, a4) =
      let open F.Unchecked in
      let ( * ) x b = F.map_ x ~f:(fun x -> Field.Var.scale b x) in
      let ( +^ ) = F.( + ) in
      F.constant a1
      +^ ((a2 - a1) * (b0 :> Field.Var.t))
      +^ ((a3 - a1) * (b1 :> Field.Var.t))
      +^ ((a4 + a1 - a2 - a3) * (b0_and_b1 :> Field.Var.t))
    in
    let x1, y1 = Curve.to_affine_exn t1
    and x2, y2 = Curve.to_affine_exn t2
    and x3, y3 = Curve.to_affine_exn t3
    and x4, y4 = Curve.to_affine_exn t4 in
    (lookup_one (x1, x2, x3, x4), lookup_one (y1, y2, y3, y4))

  (* Similar to the above, but doing lookup in a size 1 table *)
  let lookup_single_bit (b : Boolean.var) (t1, t2) =
    let lookup_one (a1, a2) =
      let open F in
      constant a1
      + map_ Unchecked.(a2 - a1) ~f:(Field.Var.scale (b :> Field.Var.t))
    in
    let x1, y1 = Curve.to_affine_exn t1 and x2, y2 = Curve.to_affine_exn t2 in
    (lookup_one (x1, x2), lookup_one (y1, y2))

  let scale_known (type shifted)
      (module Shifted : Shifted.S with type t = shifted) (t : Curve.t)
      (b : Boolean.var Bitstring_lib.Bitstring.Lsb_first.t) ~init =
    let b = Bitstring_lib.Bitstring.Lsb_first.to_list b in
    let sigma = t in
    let n = List.length b in
    let sigma_count = (n + 1) / 2 in
    (* = ceil (n / 2.0) *)
    (* We implement a complicated optimzation so that in total
       this costs roughly (1 + 3) * (n / 2) constraints, rather than
       the naive 4*n + 3*n. If scalars were represented with some
       kind of signed digit representation we could probably get it
       down to 2 * (n / 3) + 3 * (n / 3).
    *)
    (* Assume n is even *)
    (* Define
       to_term_unshifted i (b0, b1) =
       match b0, b1 with
       | false, false -> oo
       | true, false -> 2^i * t
       | false, true -> 2^{i+1} * t
       | true, true -> 2^i * t + 2^{i + 1} t

       to_term i (b0, b1) =
       sigma + to_term_unshifted i (b0, b1) =
       match b0, b1 with
       | false, false -> sigma
       | true, false -> sigma + 2^i * t
       | false, true -> sigma + 2^{i+1} * t
       | true, true -> sigma + 2^i * t + 2^{i + 1} t
    *)
    let to_term ~two_to_the_i ~two_to_the_i_plus_1 bits =
      lookup_point bits
        ( sigma
        , Curve.(sigma + two_to_the_i)
        , Curve.(sigma + two_to_the_i_plus_1)
        , Curve.(sigma + two_to_the_i + two_to_the_i_plus_1) )
    in
    (*
       Say b = b0, b1, .., b_{n-1}.
       We compute

       (to_term 0 (b0, b1)
       + to_term 2 (b2, b3)
       + to_term 4 (b4, b5)
       + ...
       + to_term (n-2) (b_{n-2}, b_{n-1}))
       - (n/2) * sigma
       =
       (n/2)*sigma + (b0*2^0 + b1*21 + ... + b_{n-1}*2^{n-1}) t - (n/2) * sigma
       =
       (n/2)*sigma + b * t - (n/2)*sigma
       = b * t
    *)
    let open Let_syntax in
    (* TODO #1152
        Can get away with using an unsafe add if we modify this a bit. *)
    let rec go acc two_to_the_i bits =
      match bits with
      | [] ->
          return acc
      | [b_i] ->
          let term =
            lookup_single_bit b_i (sigma, Curve.(sigma + two_to_the_i))
          in
          Shifted.add acc term
      | b_i :: b_i_plus_1 :: rest ->
          let two_to_the_i_plus_1 = Curve.double two_to_the_i in
          let%bind term =
            to_term ~two_to_the_i ~two_to_the_i_plus_1 (b_i, b_i_plus_1)
          in
          let%bind acc = Shifted.add acc term in
          go acc (Curve.double two_to_the_i_plus_1) rest
    in
    let%bind result_with_shift = go init t b in
    let unshift =
      Curve.scale (Curve.negate sigma) (Scalar.of_int sigma_count)
    in
    Shifted.add result_with_shift (constant unshift)

  let to_constant (x, y) =
    let open Option.Let_syntax in
    let%map x = F.to_constant x and y = F.to_constant y in
    Curve.of_affine (x, y)

  let scale m t c ~init =
    match to_constant t with
    | Some t ->
        scale_known m t c ~init
    | None ->
        scale m t c ~init

  let sum (type shifted) (module Shifted : Shifted.S with type t = shifted) xs
      ~init =
    let open Let_syntax in
    let rec go acc = function
      | [] ->
          return acc
      | t :: ts ->
          let%bind acc' = Shifted.add acc t in
          go acc' ts
    in
    go init xs
end
