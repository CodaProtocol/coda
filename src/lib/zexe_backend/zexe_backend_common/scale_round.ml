module Scale_round = struct
  open Core_kernel

  [%%versioned
    module Stable = struct
      module V1 = struct
        type 'a t =
          {
            xt: 'a; b: 'a; yt: 'a; 
            xp: 'a; l1: 'a; yp: 'a; 
            xs: 'a; ys: 'a
          } [@@deriving sexp, fields, hlist]
      end
    end]

  type 'a t = 'a Stable.Latest.t =
    {
      xt: 'a; b: 'a; yt: 'a; 
      xp: 'a; l1: 'a; yp: 'a; 
      xs: 'a; ys: 'a
    }
  [@@deriving sexp, fields, hlist]

  let typ g =
    Snarky_backendless.Typ.of_hlistable [g; g; g; g; g; g; g; g] ~var_to_hlist:to_hlist
      ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

  let map {xt; b; yt; xp; l1; yp; xs; ys} ~f =
    {
      xt= f xt; b= f b; yt= f yt; xp= f xp; l1= f l1; yp= f yp; xs= f xs; ys= f ys; 
    }

  let map2 t1 t2 ~f =
    {
      xt= f t1.xt t2.xt;
      b= f t1.b t2.b;
      yt= f t1.yt t2.yt;
      xp= f t1.xp t2.xp; 
      l1= f t1.l1 t2.l1;
      yp= f t1.yp t2.yp;
      xs= f t1.xs t2.xs;
      ys= f t1.ys t2.ys; 
    }
end
