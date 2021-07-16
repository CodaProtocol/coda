let range = (13, 19)

module Aux_data = struct
  type 'a t = 'a
end

type 'a checks = 'a * 'a * 'a * 'a * 'a * 'a

type 'a check_evals = 'a

let checks (type t) (module F : Intf.Field_intf with type t = t) ~e0 ~e1
    (endo : t Aux_data.t) =
  let open F in
  let get = Five_wires_evals.get in
  let xt = get L e0 in
  let yt = get R e0 in
  let s1 = get O e0 in
  let s2 = get Q e0 in
  let b1 = get P e0 in
  let xs = get L e1 in
  let ys = get R e1 in
  let xp = get O e1 in
  let yp = get Q e1 in
  let b2 = get P e1 in
  (* Shared values *)
  let xq = (one + (b2 * (endo - one))) * xt in
  let pow x i =
    (* Keep the code tidy, but do powers efficiently. *)
    if i = 2 then x * x else invalid_arg "pow"
  in
  ( (* binary constrain b1 *)
    b1 - (b1 * b1) (* binary constrain b2 *)
  , b2 - (b2 * b2)
  , (* (xp - (1 + (endo - 1) * b2) * xt) * s1 = yp – (2*b1-1)*yt *)
    ((xp - xq) * s1) - yp + (yt * ((of_int 2 * b1) - one))
  , (* s1^2 - s2^2 = (1 + (endo - 1) * b2) * xt - xs *)
    pow s1 2 - pow s2 2 - xq + xs
  , (* (2*xp + (1 + (endo - 1) * b2) * xt – s1^2) * (s1 + s2) = 2*yp *)
    (((of_int 2 * xp) + xq - pow s1 2) * (s1 + s2)) - (of_int 2 * yp)
  , (* (xp – xs) * s2 = ys + yp *)
    ((xp - xs) * s2) - ys - yp )

let check_evals (type t) (module F : Intf.Field_intf with type t = t) alphas
    ~e0 ~e1 aux =
  let open F in
  let check0, check1, check2, check3, check4, check5 =
    checks (module F) ~e0 ~e1 aux
  in
  (check0 * alphas range 0)
  + (check1 * alphas range 1)
  + (check2 * alphas range 2)
  + (check3 * alphas range 3)
  + (check4 * alphas range 4)
  + (check5 * alphas range 5)

let fold_check_evals ~init x ~f = f init x
