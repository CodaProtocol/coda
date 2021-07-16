let range = (19, 24)

module Aux_data = struct
  type _ t = unit
end

type 'a checks = 'a * 'a * 'a * 'a * 'a

type 'a check_evals = 'a

let checks (type t) (module F : Intf.Field_intf with type t = t) ~e0 ~e1
    (() : t Aux_data.t) =
  let open F in
  let get = Five_wires_evals.get in
  let b0 = get Q e1 in
  let b1 = get O e1 in
  let b2 = get R e1 in
  let b3 = get L e1 in
  let b4 = get P e0 in
  let res = get P e1 in
  ( (* Unpack *)
    b0
    + (of_int 2 * b1)
    + (of_int 4 * b2)
    + (of_int 8 * b3)
    + (of_int 16 * b4)
    - res
  , (* binary constrain b3 *)
    b3 - (b3 * b3)
  , (* binary constrain b2 *)
    b2 - (b2 * b2)
  , (* binary constrain b1 *)
    b1 - (b1 * b1)
  , (* binary constrain b0 *)
    b0 - (b0 * b0) )

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
