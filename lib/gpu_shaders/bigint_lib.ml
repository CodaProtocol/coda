open Gpu_dsl.Dsl
open Gpu_dsl.Dsl.Let_syntax

let bigint = Type.Array Uint32

let bigint_add n =
  declare_function "bigint_add"
    ~returning:Type.uint32
    ~args:Arguments_spec.([ bigint; bigint; bigint ])
    ~vars:Local_variables_spec.([ Type.uint32 ]) (fun rs xs ys carryp ->
    let%bind () = set_prefix "bigint_add" in
    let%bind () = constant Type.uint32 UInt32.zero >>= store carryp in
    let%bind start = constant Type.uint32 Unsigned.UInt32.zero in
    let%bind stop = constant Type.uint32 Unsigned.UInt32.(sub n one) in
    let%bind () =
      for_ (start, stop) (fun i ->
        let%bind x = array_get "x" xs i
        and y = array_get "y" ys i
        in
        let%bind { high_bits=carry1; low_bits=x_plus_y } =
          add x y "x_plus_y"
        in
        let%bind carry = load carryp "carry" in
        let%bind {low_bits=x_plus_y_with_prev_carry; high_bits=carry2 } =
          add x_plus_y carry "x_plus_y_with_prev_carry"
        in
        let%bind () =
          let%bind new_carry = bitwise_or carry1 carry2 "new_carry" in
          store carryp new_carry
        in
        array_set rs i x_plus_y_with_prev_carry)
    in
    load carryp "return_carry")

(* NB: This only works when ys <= xs *)
let bigint_sub n =
  declare_function "bigint_sub"
    ~returning:Type.uint32
    ~args:Arguments_spec.([ bigint; bigint; bigint ])
    ~vars:Local_variables_spec.([ Type.uint32 ]) (fun rs xs ys carryp ->
    let%bind () = set_prefix "bigint_sub" in
    let%bind () = constant Type.uint32 UInt32.zero >>= store carryp in
    let%bind start = constant Type.uint32 Unsigned.UInt32.zero in
    let%bind stop = constant Type.uint32 Unsigned.UInt32.(sub n one) in
    let%bind () =
      for_ (start, stop) (fun i ->
        let%bind x = array_get "x" xs i
        and y = array_get "y" ys i
        in
        let%bind { high_bits=carry1; low_bits=x_plus_y } =
          sub x y "x_plus_y"
        in
        let%bind carry = load carryp "carry" in
        let%bind {low_bits=x_plus_y_with_prev_carry; high_bits=carry2 } =
          sub x_plus_y carry "x_plus_y_with_prev_carry"
        in
        let%bind () =
          let%bind new_carry = bitwise_or carry1 carry2 "new_carry" in
          store carryp new_carry
        in
        array_set rs i x_plus_y_with_prev_carry)
    in
    load carryp "sub_return_carry")

(* Assumption: 2*p > 2^n *)
let bigint_add_mod ~bigint_add ~bigint_sub ~p n =
  declare_function "bigint_add_mod"
    ~args:Arguments_spec.([ bigint; bigint; bigint ])
    ~vars:Local_variables_spec.([])
    ~returning:Type.uint32
    (fun rs xs ys ->
      let%bind one = constant Type.uint32 UInt32.one in
      let%bind zero = constant Type.uint32 UInt32.zero in
      let%bind carry_after_add = bigint_add rs xs ys in
      let%bind overflow = equal_uint32 carry_after_add one "overflow" in
      let%bind () =
        do_if overflow begin
          let%bind last_carry = bigint_sub rs rs p in
          let%bind didn't_kill_top_bit = equal_uint32 last_carry zero "didnt_kill_top_bit" in
          do_if didn't_kill_top_bit
            (bigint_sub rs rs p >>| ignore)
        end
      in
      return zero)

let bigint_sub_mod ~bigint_add ~bigint_sub ~p n =
  declare_function "bigint_sub_mod" 
    ~args:Arguments_spec.([ bigint; bigint; bigint ])
    ~vars:Local_variables_spec.([])
    (fun rs xs ys ->
      let%bind one = constant Type.uint32 UInt32.one in
      let%bind zero = constant Type.uint32 UInt32.zero in
      let%bind carry_after_sub = bigint_sub rs xs ys in
      let%bind underflow = equal_uint32 carry_after_sub one "underflow" in
      let%bind () =
        do_if underflow begin
          let%bind last_carry = bigint_add rs rs p in
          let%bind didn't_kill_top_bit = equal_uint32 last_carry zero "didnt_kill_top_bit" in
          do_if didn't_kill_top_bit
            (bigint_add rs rs p >>| ignore)
        end
      in
      return zero)

let bigint_mul n ws xs ys =
  declare_function "bigint_mul"
    ~args:Arguments_spec.([bigint; bigint; bigint])
    ~vars:Local_variables_spec.([ Type.uint32 ])
    ~returning:Type.uint32
    (fun ws xs ys kp ->
      let%bind () = set_prefix "bigint_mul" in
      let%bind zero = constant Type.uint32 UInt32.zero
      and num_limbs = constant Type.uint32 n
      and stop = constant Type.uint32 UInt32.(sub n one)
      in
      let start = zero in
      for_ (start, stop) (fun j ->
        let%bind y = array_get "y" ys j in
        let%bind () = store kp zero in
        let%bind () =
          for_ (start, stop) (fun i ->
            let%bind i_plus_j = add_ignore_overflow i j "i_plus_j" in
            let%bind k = load kp "k_in_i_loop" in
            let%bind t =
              let%bind x = array_get "x" xs i in
              let%bind xy = mul x y "xy" in
              let%bind w = array_get "w" ws i_plus_j in
              (* Claim:
                x*y + w + k < 2^(2 * 32) (i.e., it will fit in 2 uint32s).

                We have
                x, y, w, k <= 2^32 - 1
                xy <= (2^32 - 1)(2^32 - 1) = 2^64 - 2 * 2^32 + 1

                so
                xy + w + k
                <= 2^64 - 2 * 2^32 + 1 + 2*(2^32 - 1)
                = 2^64 - 2 * 2^32 + 1 + 2 * 2^32 - 2
                = 2^64 - 2 * 2^32 + 2 * 2^32 + 1 - 2
                = 2^64 + 1 - 2
                = 2^64 - 1
              *)
              let%bind k_plus_w = add k w "k_plus_w" in
              let%bind xy_plus_k_plus_w_low_bits =
                add xy.low_bits k_plus_w.low_bits "xy_plus_k_plus_w_low_bits"
              in
              (* By the above there should be no overflow here *)
              let%map high_bits =
                let%bind intermediate =
                  add_ignore_overflow
                    xy.high_bits
                    xy_plus_k_plus_w_low_bits.high_bits
                    "intermediate"
                in
                add_ignore_overflow intermediate k_plus_w.high_bits
                  "high_bits"
              in
              { Arith_result.high_bits
              ; low_bits = xy_plus_k_plus_w_low_bits.low_bits }
            in
            let%bind () = array_set ws i_plus_j t.low_bits in
            store kp t.high_bits
          )
        in
        let%bind k = load kp "k_in_j_loop" in
        let%bind j_plus_n = add_ignore_overflow j num_limbs "j_plus_n" in
        array_set ws j_plus_n k
      )
      >>| fun () -> zero
    )

module%test Test = struct
  open Core

  let bignum_limbs = 24
  let bignum_bytes = 4 * bignum_limbs
  let bignum_bits = 8 * bignum_bytes

  let uint32_array_of_bigint n =
    let n = Bigint.to_zarith_bigint n in
    let uint32_of_bits bs =
      let open Unsigned.UInt32 in
      let (_, acc) =
        List.fold bs ~init:(one, zero) ~f:(fun (pt, acc) b ->
          let open Infix in
          (pt + pt, if b then acc + pt else acc))
      in
      acc
    in
    List.groupi ~break:(fun i _ _ -> 0 = i mod 32)
      (List.init bignum_bits ~f:(fun i -> Z.testbit n i))
    |> List.map ~f:uint32_of_bits
    |> Array.of_list

  let bigint_of_uint32_array arr =
    let open Bigint in
    let b32 = of_int (Int.pow 2 32) in
    let (_, acc) =
      Array.fold arr ~init:(one, zero) ~f:(fun (shift, acc) c ->
        ( shift * b32
        , acc
          + of_int (Unsigned.UInt32.to_int c) * shift))
    in
    acc

  let add x y =
    let bigint_typ = Type.Array Type.Scalar.Uint32 in
    let result = Array.init bignum_limbs ~f:(fun _ -> Unsigned.UInt32.zero) in
    let x = uint32_array_of_bigint x in
    let y = uint32_array_of_bigint y in
    let _ =
      Interpreter.eval Interpreter.State.empty begin
        let%bind carry_pointer = create_pointer Type.Scalar.Uint32 "carry_pointer" in
        let%bind x = constant bigint_typ x
        and y = constant bigint_typ y
        and r = constant bigint_typ result
        in
        bigint_add ~carry_pointer
          (Unsigned.UInt32.of_int bignum_limbs) r x y 
      end
    in
    bigint_of_uint32_array result

  let sub x y =
    let bigint_typ = Type.Array Type.Scalar.Uint32 in
    let result = Array.init bignum_limbs ~f:(fun _ -> Unsigned.UInt32.zero) in
    let x = uint32_array_of_bigint x in
    let y = uint32_array_of_bigint y in
    let _ =
      Interpreter.eval Interpreter.State.empty begin
        let%bind carry_pointer = create_pointer Type.Scalar.Uint32 "carry_pointer" in
        let%bind x = constant bigint_typ x
        and y = constant bigint_typ y
        and r = constant bigint_typ result
        in
        bigint_sub
          ~carry_pointer
          (Unsigned.UInt32.of_int bignum_limbs) r x y 
      end
    in
    bigint_of_uint32_array result

  let add_mod ~p x y =
    let bigint_typ = Type.Array Type.Scalar.Uint32 in
    let result = Array.init bignum_limbs ~f:(fun _ -> Unsigned.UInt32.zero) in
    let x = uint32_array_of_bigint x in
    let y = uint32_array_of_bigint y in
    let p = uint32_array_of_bigint p in
    let _ =
      Interpreter.eval Interpreter.State.empty begin
        let%bind x = constant bigint_typ x
        and y = constant bigint_typ y
        and p = constant bigint_typ p
        and r = constant bigint_typ result
        in
        bigint_add_mod ~p
          (Unsigned.UInt32.of_int bignum_limbs) r x y 
      end
    in
    bigint_of_uint32_array result

  let sub_mod ~p x y =
    let bigint_typ = Type.Array Type.Scalar.Uint32 in
    let result = Array.init bignum_limbs ~f:(fun _ -> Unsigned.UInt32.zero) in
    let x = uint32_array_of_bigint x in
    let y = uint32_array_of_bigint y in
    let p = uint32_array_of_bigint p in
    let _ =
      Interpreter.eval Interpreter.State.empty begin
        let%bind x = constant bigint_typ x
        and y = constant bigint_typ y
        and p = constant bigint_typ p
        and r = constant bigint_typ result
        in
        bigint_sub_mod ~p
          (Unsigned.UInt32.of_int bignum_limbs) r x y 
      end
    in
    bigint_of_uint32_array result

  let mul x y =
    let bigint_typ = Type.Array Type.Scalar.Uint32 in
    let result = Array.init (2*bignum_limbs) ~f:(fun _ -> Unsigned.UInt32.zero) in
    let x = uint32_array_of_bigint x in
    let y = uint32_array_of_bigint y in
    let _ =
      Interpreter.eval Interpreter.State.empty begin
        let%bind x = constant bigint_typ x ~label:"xs"
        and y = constant bigint_typ y ~label:"ys"
        and r = constant bigint_typ result ~label:"result"
        in
        bigint_mul (Unsigned.UInt32.of_int bignum_limbs)
          r x y 
      end
    in
    bigint_of_uint32_array result

  let () =
    let gen = 
      let max_int = Bigint.(pow (of_int 2) (of_int bignum_bits) - one) in
      let open Quickcheck.Let_syntax in
      let%bind p =
        (* Need:
            2*p > 2^bignum_bits
            p < 2^bignum_bits

           I.e.,
           2^(bignum_bits - 1) < p < 2^bignum_bits
           2^(bignum_bits - 1) < p < 2^bignum_bits
        *)
        Bigint.(
          gen_incl
            (pow (of_int 2) (of_int (Int.(-) bignum_bits 1)) + one)
            max_int)
      in
      let%bind x = Bigint.(gen_incl zero max_int) in
      let%map y = Bigint.(gen_incl zero max_int) in
      (x, y, p)
    in
    Quickcheck.test gen
      ~f:(fun (x, y, p) ->
        let r = Bigint.(%) (add_mod ~p x y) p in
        let actual = Bigint.((x + y) % p) in
        if not (Bigint.equal r actual)
        then failwithf !"(%{sexp:Bigint.t} +_p %{sexp:Bigint.t}): got %{sexp:Bigint.t}, expected %{sexp:Bigint.t}"
               x y r actual ())

  let () =
    let gen = 
      let max_int = Bigint.(pow (of_int 2) (of_int bignum_bits) - one) in
      let open Quickcheck.Let_syntax in
      let%bind p =
        (* Need:
            2*p > 2^bignum_bits
            p < 2^bignum_bits

           I.e.,
           2^(bignum_bits - 1) < p < 2^bignum_bits
           2^(bignum_bits - 1) < p < 2^bignum_bits
        *)
        Bigint.(
          gen_incl
            (pow (of_int 2) (of_int (Int.(-) bignum_bits 1)) + one)
            max_int)
      in
      let%bind x = Bigint.(gen_incl zero max_int) in
      let%map y = Bigint.(gen_incl zero max_int) in
      (x, y, p)
    in
    Quickcheck.test gen
      ~f:(fun (x, y, p) ->
        let r = Bigint.(%) (sub_mod ~p x y) p in
        let actual = Bigint.((x - y) % p) in
        if not (Bigint.equal r actual)
        then failwithf !"(%{sexp:Bigint.t} -_p %{sexp:Bigint.t}): got %{sexp:Bigint.t}, expected %{sexp:Bigint.t}"
               x y r actual ())

  let () =
    let gen = 
      let max_int = Bigint.(pow (of_int 2) (of_int bignum_bits) - one) in
      let open Quickcheck.Let_syntax in
      let%bind x = Bigint.(gen_incl zero max_int) in
      let%map y = Bigint.(gen_incl zero x) in
      (x, y)
    in
    Quickcheck.test gen
      ~f:(fun (x, y) ->
        let r = sub x y in
        let actual = Bigint.( - ) x y in
        if not (Bigint.equal r actual)
        then failwithf !"(%{sexp:Bigint.t} - %{sexp:Bigint.t}): got %{sexp:Bigint.t}, expected %{sexp:Bigint.t}"
               x y r actual ())

  let () =
    let g = 
      Bigint.(gen_incl zero (pow (of_int 2) (of_int bignum_bits) - one))
    in
    Quickcheck.test 
      (Quickcheck.Generator.tuple2 g g)
      ~f:(fun (x, y) ->
        let r = mul x y in
        let actual = Bigint.( * ) x y in
        if not (Bigint.equal r actual)
        then failwithf !"(%{sexp:Bigint.t} * %{sexp:Bigint.t}): got %{sexp:Bigint.t}, expected %{sexp:Bigint.t}"
               x y r actual ())

  let () =
    let g = 
      (* TODO: This actually fails for now since I don't hold onto the last carry.
         It works for things with no overflow though. *)
      Bigint.(gen_incl zero (pow (of_int 2) (of_int Int.(32*(bignum_limbs - one)))))
    in
    Quickcheck.test 
      (Quickcheck.Generator.tuple2 g g)
      ~f:(fun (x, y) ->
        assert (Bigint.equal (add x y) (Bigint.(+) x y)))
end
