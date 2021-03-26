open Core_kernel
module H_list = Snarky_backendless.H_list

[%%versioned
module Stable = struct
  module V1 = struct
    type 'a t =
      { mutable b2: 'a
      ; mutable xt: 'a
      ; mutable b1: 'a
      ; xq: 'a
      ; mutable yt: 'a
      ; mutable xp: 'a
      ; l1: 'a
      ; l2: 'a
      ; mutable yp: 'a
      ; xs: 'a
      ; ys: 'a }
    [@@deriving sexp, fields, hlist]
  end
end]

let typ g =
  Snarky_backendless.Typ.of_hlistable
    [g; g; g; g; g; g; g; g; g; g; g]
    ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
    ~value_of_hlist:of_hlist

let map {b2; xt; b1; xq; yt; xp; l1; l2; yp; xs; ys} ~f =
  { b2= f b2
  ; xt= f xt
  ; b1= f b1
  ; xq= f xq
  ; yt= f yt
  ; xp= f xp
  ; l1= f l1
  ; l2= f l2
  ; yp= f yp
  ; xs= f xs
  ; ys= f ys }

let map2 t1 t2 ~f =
  { b2= f t1.b2 t2.b2
  ; xt= f t1.xt t2.xt
  ; b1= f t1.b1 t2.b1
  ; xq= f t1.xq t2.xq
  ; yt= f t1.yt t2.yt
  ; xp= f t1.xp t2.xp
  ; l1= f t1.l1 t2.l1
  ; l2= f t1.l2 t2.l2
  ; yp= f t1.yp t2.yp
  ; xs= f t1.xs t2.xs
  ; ys= f t1.ys t2.ys }
