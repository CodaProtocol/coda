(* Dedicated functor for the type, since instantiating it will be optimised out
   if there are no values.
*)

module Utils = struct
  module Id = struct
    type 'a t = 'a
  end

  module Gate_id (Gate : Intf.Five_wires_gate_intf) = Id

  module Constant (A : sig
    type t
  end) =
  struct
    type 'a t = A.t
  end

  module Gate_constant (A : sig
    type t
  end)
  (Gate : Intf.Five_wires_gate_intf) =
    Constant (A)

  module Gate_aux_data (Gate : Intf.Five_wires_gate_intf) = struct
    type 'a t = 'a Gate.Aux_data.t
  end

  module Gate_check_evals (Gate : Intf.Five_wires_gate_intf) = struct
    type 'a t = 'a Gate.check_evals
  end
end

module T (F : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  type 'a t
end) =
struct
  type 'a t =
    { poseidon: 'a F(Poseidon_5_wires).t
    ; ecadd: 'a F(Ecadd_5_wires).t
    ; ecdouble: 'a F(Ecdouble_5_wires).t
    ; endosclmul: 'a F(Endosclmul_5_wires).t
    ; packing: 'a F(Packing_5_wires).t
    ; varbasemul: 'a F(Varbasemul_5_wires).t
    ; varbasemulpack: 'a F(Varbasemulpack_5_wires).t }
end

module Make_functions
    (Combinator : functor
      (Gate : Intf.Five_wires_gate_intf)
      -> sig
  type 'a t
end) (A : sig
  type t
end) (Fn : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  val f : A.t Combinator(Gate).t
end) =
struct
  let fns : A.t T(Combinator).t =
    { poseidon=
        (let module M = Fn (Poseidon_5_wires) in
        M.f)
    ; ecadd=
        (let module M = Fn (Ecadd_5_wires) in
        M.f)
    ; ecdouble=
        (let module M = Fn (Ecdouble_5_wires) in
        M.f)
    ; endosclmul=
        (let module M = Fn (Endosclmul_5_wires) in
        M.f)
    ; packing=
        (let module M = Fn (Packing_5_wires) in
        M.f)
    ; varbasemul=
        (let module M = Fn (Varbasemul_5_wires) in
        M.f)
    ; varbasemulpack=
        (let module M = Fn (Varbasemulpack_5_wires) in
        M.f) }
end

module Map (F1 : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  type 'a t
end) (F2 : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  type 'a t
end) (A : sig
  type _ t
end) (B : sig
  type _ t
end) (Map : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  val f : 'f A.t F1(Gate).t -> 'f B.t F2(Gate).t
end) =
struct
  let f (type f) (t : f A.t T(F1).t) : f B.t T(F2).t =
    let module Fn (Gate : Intf.Five_wires_gate_intf) = struct
      type 'f t = 'f A.t F1(Gate).t -> 'f B.t F2(Gate).t
    end in
    let module Fns =
      Make_functions
        (Fn)
        (struct
          type t = f
        end)
        (Map)
    in
    { poseidon= Fns.fns.poseidon t.poseidon
    ; ecadd= Fns.fns.ecadd t.ecadd
    ; ecdouble= Fns.fns.ecdouble t.ecdouble
    ; endosclmul= Fns.fns.endosclmul t.endosclmul
    ; packing= Fns.fns.packing t.packing
    ; varbasemul= Fns.fns.varbasemul t.varbasemul
    ; varbasemulpack= Fns.fns.varbasemulpack t.varbasemulpack }
end

module Map2 (F1 : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  type 'a t
end) (F2 : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  type 'a t
end) (F3 : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  type 'a t
end) (A : sig
  type _ t
end) (B : sig
  type _ t
end) (C : sig
  type _ t
end) (Map2 : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  val f : 'f A.t F1(Gate).t -> 'f B.t F2(Gate).t -> 'f C.t F3(Gate).t
end) =
struct
  let f (type f) (t1 : f A.t T(F1).t) (t2 : f B.t T(F2).t) : f C.t T(F3).t =
    let module Fn (Gate : Intf.Five_wires_gate_intf) = struct
      type 'f t = 'f A.t F1(Gate).t -> 'f B.t F2(Gate).t -> 'f C.t F3(Gate).t
    end in
    let module Fns =
      Make_functions
        (Fn)
        (struct
          type t = f
        end)
        (Map2)
    in
    { poseidon= Fns.fns.poseidon t1.poseidon t2.poseidon
    ; ecadd= Fns.fns.ecadd t1.ecadd t2.ecadd
    ; ecdouble= Fns.fns.ecdouble t1.ecdouble t2.ecdouble
    ; endosclmul= Fns.fns.endosclmul t1.endosclmul t2.endosclmul
    ; packing= Fns.fns.packing t1.packing t2.packing
    ; varbasemul= Fns.fns.varbasemul t1.varbasemul t2.varbasemul
    ; varbasemulpack=
        Fns.fns.varbasemulpack t1.varbasemulpack t2.varbasemulpack }
end

module Fold (F : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  type 'a t
end) (A : sig
  type _ t
end) (Acc : sig
  type _ t
end) (Fold : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  val f : 'f Acc.t -> 'f A.t F(Gate).t -> 'f Acc.t
end) =
struct
  let f (type f) ~init:(acc : f Acc.t) (t : f A.t T(F).t) : f Acc.t =
    let module Fn (Gate : Intf.Five_wires_gate_intf) = struct
      type 'f t = 'f Acc.t -> 'f A.t F(Gate).t -> 'f Acc.t
    end in
    let module Fns =
      Make_functions
        (Fn)
        (struct
          type t = f
        end)
        (Fold)
    in
    let acc = Fns.fns.poseidon acc t.poseidon in
    let acc = Fns.fns.ecadd acc t.ecadd in
    let acc = Fns.fns.ecdouble acc t.ecdouble in
    let acc = Fns.fns.endosclmul acc t.endosclmul in
    let acc = Fns.fns.packing acc t.packing in
    let acc = Fns.fns.varbasemul acc t.varbasemul in
    let acc = Fns.fns.varbasemulpack acc t.varbasemulpack in
    acc
end

module Fold2 (F1 : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  type 'a t
end) (F2 : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  type 'a t
end) (A : sig
  type _ t
end) (B : sig
  type _ t
end) (Acc : sig
  type _ t
end) (Fold2 : functor (Gate : Intf.Five_wires_gate_intf) -> sig
  val f : 'f Acc.t -> 'f A.t F1(Gate).t -> 'f B.t F2(Gate).t -> 'f Acc.t
end) =
struct
  let f (type f) ~init:(acc : f Acc.t) (t1 : f A.t T(F1).t)
      (t2 : f B.t T(F2).t) : f Acc.t =
    let module Fn (Gate : Intf.Five_wires_gate_intf) = struct
      type 'f t =
        'f Acc.t -> 'f A.t F1(Gate).t -> 'f B.t F2(Gate).t -> 'f Acc.t
    end in
    let module Fns =
      Make_functions
        (Fn)
        (struct
          type t = f
        end)
        (Fold2)
    in
    let acc = Fns.fns.poseidon acc t1.poseidon t2.poseidon in
    let acc = Fns.fns.ecadd acc t1.ecadd t2.ecadd in
    let acc = Fns.fns.ecdouble acc t1.ecdouble t2.ecdouble in
    let acc = Fns.fns.endosclmul acc t1.endosclmul t2.endosclmul in
    let acc = Fns.fns.packing acc t1.packing t2.packing in
    let acc = Fns.fns.varbasemul acc t1.varbasemul t2.varbasemul in
    let acc = Fns.fns.varbasemulpack acc t1.varbasemulpack t2.varbasemulpack in
    acc
end
