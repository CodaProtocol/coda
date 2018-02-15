open Core
open Snark_params
open Tick
open Let_syntax

type t =
  { upper_bound : Bignum.Bigint.t
  ; lower_bound : Bignum.Bigint.t
  ; var         : Cvar.t
  ; bits        : Boolean.var list option
  }

let pow2 n = Bignum.Bigint.(pow (of_int 2) (of_int n))

let bigint_num_bits =
  let rec go acc i =
    if Bignum.Bigint.(acc = zero)
    then i
    else go (Bignum.Bigint.shift_right acc 1) (i + 1)
  in
  fun n -> go n 0
;;

let to_bits { var; bits; upper_bound; lower_bound = _ } =
  let length = bigint_num_bits upper_bound in
  with_label "Number.to_bits" begin
    match bits with
    | Some bs -> return (List.take bs length)
    | None -> Checked.unpack var ~length
  end
;;

let of_bits bs =
  let n = List.length bs in
  assert (n < Field.size_in_bits);
  { upper_bound = Bignum.Bigint.(pow2 n - one)
  ; lower_bound = Bignum.Bigint.zero
  ; var = Checked.pack bs
  ; bits = Some bs
  }

let clamp_to_n_bits t n =
  assert (n < Field.size_in_bits);
  with_label "Number.clamp_to_n_bits" begin
    let k = pow2 n in
    if Bignum.Bigint.(t.upper_bound < k)
    then return t
    else
      let%bind bs = to_bits t in
      let bs' = List.take bs n in
      let g = Checked.pack bs' in
      let%bind fits = Checked.equal t.var g in
      let%map r =
        Checked.if_ fits
          ~then_:g
          ~else_:(Cvar.constant Field.(sub (Util.two_to_the n) one))
      in
      { upper_bound = Bignum.Bigint.(k - one)
      ; lower_bound = t.lower_bound
      ; var = r
      ; bits = None
      }
  end
;;

let (<) x y =
  let open Bignum.Bigint in
  (*
    x [ ]
    y     [ ]

    x     [ ]
    y [ ] 
  *)
  with_label "Number.(<)" begin
    if x.upper_bound < y.lower_bound
    then return Boolean.true_
    else if x.lower_bound > y.upper_bound
    then return Boolean.false_
    else
      let bit_length =
        Int.max (bigint_num_bits x.upper_bound) (bigint_num_bits y.upper_bound)
      in
      let%map { less; _ } = Util.compare ~bit_length x.var y.var in
      less
  end
;;

let to_var { var; _ } = var

let constant x =
  let tick_n = Bigint.of_field x in
  let n = Snark_params.bigint_of_tick_bigint tick_n in
  { upper_bound = n
  ; lower_bound = n
  ; var = Cvar.constant x
  ; bits =
      Some 
        (List.init (bigint_num_bits n) ~f:(fun i ->
          Boolean.var_of_value (Tick.Bigint.test_bit tick_n i)))
  }

let field_size = Snark_params.bigint_of_tick_bigint Tick_curve.field_size

let (+) x y =
  let open Bignum.Bigint in
  let upper_bound = x.upper_bound + y.upper_bound in
  if upper_bound < field_size
  then
    { upper_bound
    ; lower_bound = x.lower_bound + y.lower_bound
    ; var = Cvar.add x.var y.var
    ; bits = None
    }
  else failwith "Number.+: Potential overflow: "

let (-) x y =
  let open Bignum.Bigint in
  (* x_upper_bound >= x >= x_lower_bound >= y_upper_bound >= y >= y_lower_bound *)
  if x.lower_bound >= y.upper_bound
  then
    { upper_bound = x.upper_bound - y.lower_bound
    ; lower_bound = x.lower_bound - y.upper_bound
    ; var = Cvar.sub x.var y.var
    ; bits = None
    }
  else
    failwithf "Number.-: Potential underflow (%s < %s)"
      (to_string x.lower_bound) (to_string y.upper_bound) ()

let ( * ) x y =
  let open Bignum.Bigint in
  with_label "Number.(*)" begin
    let upper_bound = x.upper_bound * y.upper_bound in
    if upper_bound < field_size
    then
      let%map var = Checked.mul x.var y.var in
      { upper_bound
      ; lower_bound = x.lower_bound * y.lower_bound
      ; var 
      ; bits = None
      }
    else failwith "Number.+: Potential overflow"
  end

