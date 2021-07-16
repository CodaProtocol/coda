let range = (0, 5)

module Aux_data = struct
  type 'a t = 'a Five_wires_evals.t Five_wires_evals.t
end

type 'a checks = 'a * 'a * 'a * 'a * 'a

type 'a check_evals = 'a

let checks (type t) (module F : Intf.Field_intf with type t = t) ~e0 ~e1
    (mds : t Aux_data.t) =
  let open F in
  let get = Five_wires_evals.get in
  let sbox x =
    (* x^7 *)
    let x2 = x * x in
    let x4 = x2 * x2 in
    x * x2 * x4
  in
  let sboxes = Five_wires_evals.map ~f:sbox e0 in
  let lro =
    Five_wires_evals.map mds ~f:(fun mds ->
        Five_wires_evals.map2 sboxes mds ~f:( * )
        |> Five_wires_evals.reduce ~f:( + ) )
  in
  ( get L lro - get L e1
  , get R lro - get R e1
  , get O lro - get O e1
  , get Q lro - get Q e1
  , get P lro - get P e1 )

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
