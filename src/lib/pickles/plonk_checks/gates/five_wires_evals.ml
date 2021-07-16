(* TODO: Uplift this. *)
type 'eval t = {l: 'eval; r: 'eval; o: 'eval; q: 'eval; p: 'eval}

let get (x : Five_wires_cols.t) {l; r; o; q; p} =
  match x with L -> l | R -> r | O -> o | Q -> q | P -> p

let mapi {l; r; o; q; p} ~f =
  { l= f Five_wires_cols.L l
  ; r= f Five_wires_cols.R r
  ; o= f Five_wires_cols.O o
  ; q= f Five_wires_cols.Q q
  ; p= f Five_wires_cols.P p }

let map x ~f = mapi x ~f:(fun _ -> f)

let map2i x y ~f = mapi x ~f:(fun idx x -> f idx x (get idx y))

let map2 x ~f = map2i x ~f:(fun _ -> f)

let foldi {l; r; o; q; p} ~init ~f =
  let acc = init in
  let acc = f Five_wires_cols.L acc l in
  let acc = f Five_wires_cols.R acc r in
  let acc = f Five_wires_cols.O acc o in
  let acc = f Five_wires_cols.Q acc q in
  let acc = f Five_wires_cols.P acc p in
  acc

let fold x ~init ~f = foldi x ~init ~f:(fun _ -> f)

let fold2i x y ~init ~f =
  foldi x ~init ~f:(fun idx acc x -> f idx acc x (get idx y))

let fold2 x y ~init ~f = fold2i x y ~init ~f:(fun _ -> f)

let reduce {l; r; o; q; p} ~f =
  let acc = f l r in
  let acc = f acc o in
  let acc = f acc q in
  f acc p
