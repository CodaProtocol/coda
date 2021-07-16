let range = (29, 34)

module Aux_data = struct
  type _ t = unit
end

type 'a checks = 'a * 'a * 'a * 'a * 'a

type 'a check_evals = 'a

let checks (type t) (module F : Intf.Field_intf with type t = t) ~e0 ~e1
    (() : t Aux_data.t) =
  let open F in
  let get = Five_wires_evals.get in
  let xt = get L e0 in
  let yt = get R e0 in
  let s1 = get O e0 in
  let b = get Q e0 in
  let n1 = get P e0 in
  let xs = get L e1 in
  let ys = get R e1 in
  let xp = get O e1 in
  let yp = get Q e1 in
  let n2 = get P e1 in
  (* Shared values *)
  let ps = xp - xs in
  let pow x i =
    (* Keep the code tidy, but do powers efficiently. *)
    if i = 2 then x * x else invalid_arg "pow"
  in
  ( (* binary constrain b *)
    b - (b * b)
  , (* (xp - xt) * s1 = yp – (2b-1)*yt *)
    ((xp - xt) * s1) - yp + (yt * ((of_int 2 * b) - one))
  , (* (2*xp – s1^2 + xt) * ((xp – xs) * s1 + ys + yp) = (xp – xs) * 2*yp *)
    ((of_int 2 * xp) - pow s1 2 + xt)
    * ((ps * s1) + ys + yp - (of_int 2 * yp * ps))
  , (* (ys + yp)^2 - (xp – xs)^2 * (s1^2 – xt + xs) *)
    pow (ys + yp) 2 - (pow ps 2 * (pow s1 2 - xt + xs))
  , (* n1 - 2*n2 - b *)
    n1 - (of_int 2 * n2) - b )

let check_evals (type t) (module F : Intf.Field_intf with type t = t) alphas
    ~e0 ~e1 aux =
  let open F in
  let check0, check1, check2, check3, check4 = checks (module F) ~e0 ~e1 aux in
  (check0 * alphas range 0)
  + (check1 * alphas range 1)
  + (check2 * alphas range 2)
  + (check3 * alphas range 3)
  + (check4 * alphas range 4)

let fold_check_evals ~init x ~f = f init x
