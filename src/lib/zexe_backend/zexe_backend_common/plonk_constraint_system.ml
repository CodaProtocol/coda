include Scale_round
include Endoscale_round

module Hash_state = struct
  open Core_kernel
  module H = Digestif.SHA256

  type t = H.ctx

  let digest t = Md5.digest_string H.(to_raw_string (get t))
end

module Plonk_constraint = struct
  open Core_kernel

  module T = struct
    type ('v, 'f) t =
      | Basic of { l : 'f * 'v; r : 'f * 'v; o : 'f * 'v; m: 'f; c: 'f }
      | Poseidon of { start: 'v array; state: ('v array) array }
      | EC_add of { p1 : 'v * 'v; p2 : 'v * 'v; p3 : 'v * 'v  }
      | EC_scale of { state: ('v Scale_round.t) array  }
      | EC_endoscale of { state: ('v Endoscale_round.t) array  }
    [@@deriving sexp]

    let map (type a b f) (t : (a, f) t) ~(f : a -> b) =
      let fp (x, y) = f x, f y in
      match t with
      | Basic { l ; r; o; m; c } ->
        let p (x, y) = (x, f y) in
        Basic { l= p l; r= p r; o= p o; m; c }

      | Poseidon { start; state } ->
        Poseidon { start=Array.map ~f start; state= Array.map ~f:(fun (x) -> Array.map ~f x) state }

      | EC_add { p1; p2; p3 } ->
        EC_add { p1= fp p1; p2= fp p2; p3= fp p3 }

      | EC_scale { state } ->
        EC_scale { state= Array.map ~f:(fun (x) -> Scale_round.map ~f x) state }

      | EC_endoscale { state } ->
        EC_endoscale { state= Array.map ~f:(fun (x) -> Endoscale_round.map ~f x) state }

    let eval (type v f)
        (module F : Snarky_backendless.Field_intf.S with type t = f)
        (eval_one : v -> f)
        (t : (v, f) t) =
      match t with
      (* cl * vl + cr * vr + co * vo + m * vl*vr + c = 0 *)
      | Basic { l=(cl, vl); r=(cr, vr) ; o = (co, vo); m; c } ->
        let vl = eval_one vl in
        let vr = eval_one vr in
        let vo = eval_one vo in
        F.(equal zero (List.reduce_exn ~f:add [ mul cl vl; mul cr vl; mul co vo; mul m (mul vl vr); c ]))
      | _ -> failwith "TODO"
  end
  include Snarky_backendless.Constraint.Add_kind(T)
end 

module Position = struct
  type t = { row: int; col: int }
end

module Internal_var = Uuid

module V = struct
  open Core_kernel

  module T = struct
    type t =
      | External of int
      | Internal of Internal_var.Unstable.t
    [@@deriving compare, hash, sexp]
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let create_internal () = Internal (Uuid_unix.create ())
end

module type Constraint_matrix_intf = sig
  type field_vector

  type t

  val create : unit -> t

  val append_row : t -> Snarky_bn382.Usize_vector.t -> field_vector -> unit
end

type 'a abc = {a: 'a; b: 'a; c: 'a} [@@deriving sexp]

module Weight = struct
  open Core_kernel

  type t = int abc [@@deriving sexp]

  let ( + ) t1 (a, b, c) = {a= t1.a + a; b= t1.b + b; c= t1.c + c}

  let norm {a; b; c} = Int.(max a (max b c))
end

module Triple = struct
  type 'a t = 'a * 'a * 'a
end

type 'a t =
  { m: 'a abc
  ; equivalence_classes: Position.t list V.Table.t
  ; mutable next_row: int
  ; mutable hash: Hash_state.t
  ; mutable constraints: int
  ; mutable weight: Weight.t
  ; mutable public_input_size: int
  ; mutable auxiliary_input_size: int }

module Hash = Core.Md5

let digest (t : _ t) = Hash_state.digest t.hash

module Make (Fp : sig
    include Field.S

    val to_bigint_raw_noalloc : t -> Bigint.t
  end)
    (Mat : Constraint_matrix_intf with type field_vector := Fp.Vector.t) =
struct
  open Core

  module Hash_state = struct
    include Hash_state

    let empty = H.feed_string H.empty "r1cs_constraint_system"
  end

  type nonrec t = Mat.t t

  let create () =
    { public_input_size= 0
    ; next_row= 0
    ; equivalence_classes= V.Table.create ()
    ; hash= Hash_state.empty
    ; constraints= 0
    ; auxiliary_input_size= 0
    ; weight= {a= 0; b= 0; c= 0}
    ; m= {a= Mat.create (); b= Mat.create (); c= Mat.create ()} }

  (* TODO *)
  let to_json _ = `List []

  let get_auxiliary_input_size t = t.auxiliary_input_size

  let get_primary_input_size t = t.public_input_size

  let set_auxiliary_input_size t x = t.auxiliary_input_size <- x

  let set_primary_input_size t x = t.public_input_size <- x

  let digest = digest

  let finalize = ignore
  let canonicalize x =
    let c, terms =
      Fp.(
        Snarky_backendless.Cvar.to_constant_and_terms ~add ~mul ~zero:(of_int 0) ~equal
          ~one:(of_int 1))
        x
    in
    let terms =
      List.sort terms ~compare:(fun (_, i) (_, j) -> Int.compare i j)
    in
    let has_constant_term = Option.is_some c in
    let terms = match c with None -> terms | Some c -> (c, 0) :: terms in
    match terms with
    | [] ->
      Some ([], 0, false)
    | (c0, i0) :: terms ->
      let acc, i, ts, n =
        Sequence.of_list terms
        |> Sequence.fold ~init:(c0, i0, [], 0)
          ~f:(fun (acc, i, ts, n) (c, j) ->
              if Int.equal i j then (Fp.add acc c, i, ts, n)
              else (c, j, (acc, i) :: ts, n + 1) )
      in
      Some (List.rev ((acc, i) :: ts), n + 1, has_constant_term)

  let neg_one = Fp.(negate one)

  let add_generic_constraint sys ?m ?c ?o ?r ((ls : Fp.t), (lx: V.t)) : unit =

    let row = sys.next_row in sys.next_row <- row + 1 ;
    V.Table.add_multi sys.equivalence_classes ~key:lx ~data:{ row; col= 0 } ;
    Option.iter r ~f:(fun (_, xr) -> V.Table.add_multi sys.equivalence_classes ~key:xr ~data:{ row; col= 1 }) ;
    Option.iter o ~f:(fun (_, xo) -> V.Table.add_multi sys.equivalence_classes ~key:xo ~data:{ row; col= 2 })
  (*;
    Backend.add_generic_constraint sys.backend
    ([ sl; sr ] @ Option.to_list (Option.map ~f:fst o) @ Option.to_list m @ Option.to_list c )
  *)

  let completely_reduce sys (terms : (Fp.t * int) list) = (* just adding constrained variables without values *)
    let rec go = function
      | [] -> assert false
      | [ (s, x) ] -> (s, V.External x)
      | (ls, lx) :: t ->
        let lx = V.External lx in
        let (rs, rx) = go t in
        let s1x1_plus_s2x2 = V.create_internal () in
        add_generic_constraint sys (ls, lx) ~r:(rs, rx) ~o:(neg_one, s1x1_plus_s2x2) ;
        (Fp.one, s1x1_plus_s2x2)
    in
    go terms

  let reduce_lincom sys (x : Fp.t Snarky_backendless.Cvar.t)  =
    let constant, terms =
      Fp.(
        Snarky_backendless.Cvar.to_constant_and_terms ~add ~mul ~zero:(of_int 0) ~equal
          ~one:(of_int 1))
        x
    in
    let terms =
      List.sort terms ~compare:(fun (_, i) (_, j) -> Int.compare i j)
    in
    match constant, terms with
    | Some c, [] -> (c, `Constant)
    | None, [] -> (Fp.zero, `Constant)
    | _, (c0, i0) :: terms ->
      let terms =
        let acc, i, ts, _ =
          Sequence.of_list terms
          |> Sequence.fold ~init:(c0, i0, [], 0)
            ~f:(fun (acc, i, ts, n) (c, j) ->
                if Int.equal i j then (Fp.add acc c, i, ts, n)
                else (c, j, (acc, i) :: ts, n + 1) )
        in
        List.rev ((acc, i) :: ts)
      in
      match terms with
      | [] -> assert false
      | [(x, i)] -> (x, `Var (V.External i))
      | (ls, lx) :: tl ->
        let (rs, rx) = completely_reduce sys tl in
        let res = V.create_internal () in
        add_generic_constraint sys ?c:constant (ls, External lx) ~r:(rs, rx) ~o:(neg_one, res) ;
        (Fp.one, `Var res)
  ;;

  let add_constraint ?label:_ sys
      (constr : (Fp.t Snarky_backendless.Cvar.t, Fp.t) Snarky_backendless.Constraint.basic) =

    let red = reduce_lincom sys in
    let reduce_to_v (x : Fp.t Snarky_backendless.Cvar.t) : V.t =
      let (s, x) = red x in
      match x with
      | `Var x ->
        if Fp.equal s Fp.one then x
        else let sx = V.create_internal () in
          add_generic_constraint sys ~o:(Fp.(negate one), sx) (s, x);
          x
      | `Constant ->
        let x = V.create_internal () in
        add_generic_constraint sys ~c:(Fp.negate s) (Fp.one, x);
        x
    in
    match constr with

    | Snarky_backendless.Constraint.Boolean v ->
      let (s, x) = red v in
      ( 
        match x with
        | `Var x -> add_generic_constraint sys ~m:Fp.(negate (square s)) (s, x) ~r:(s, x)
        | `Constant -> assert Fp.(equal s (s * s))
      )

    | Snarky_backendless.Constraint.Equal (v1, v2) ->
      let (s1, x1), (s2, x2) = red v1, red v2 in
      ( 
        match x1, x2 with
        | `Var x1, `Var x2 ->
          if s1 <> s2 then add_generic_constraint sys (Fp.negate s1, x1) ~r:(s2, x2)
          (* TODO: optimize by not adding generic costraint but rather permuting the vars *)
          else add_generic_constraint sys (Fp.negate s1, x1) ~r:(s2, x2)
        | `Var x1, `Constant -> add_generic_constraint sys (Fp.negate s1, x1) ~c:s2
        | `Constant, `Var x2 -> add_generic_constraint sys (Fp.negate s2, x2) ~c:s1
        | `Constant, `Constant -> assert Fp.(equal s1 s2)
      )

    | Snarky_backendless.Constraint.Square (v1, v2) ->
      let (s1, x1), (s2, x2) = red v1, red v2 in
      ( 
        match x1, x2 with
        | `Var x1, `Var x2 -> add_generic_constraint sys ~m:Fp.(s1 * s1) ~o:(Fp.negate s2, x2) (s1, x1) ~r:(s1, x1)
        | `Var x1, `Constant -> add_generic_constraint sys ~m:Fp.(s1 * s1) ~c:s2 (s1, x1) ~r:(s1, x1)
        | `Constant, `Var x2 -> add_generic_constraint sys (Fp.negate s2, x2) ~c:(Fp.square s1)
        | `Constant, `Constant -> assert Fp.(equal (square s1) s2)
      )

    | Snarky_backendless.Constraint.R1CS (v1, v2, v3) ->
      let (s1, x1), (s2, x2), (s3, x3) = red v1, red v2, red v3 in
      ( 
        match x1, x2, x3 with
        | `Var x1, `Var x2, `Var x3 -> add_generic_constraint sys ~m:Fp.(s1 * s2) ~o:(Fp.negate s3, x3) (s1, x1) ~r:(s2, x2)
        | `Var x1, `Var x2, `Constant -> add_generic_constraint sys ~m:Fp.(s1 * s2) ~c:(Fp.negate s3) (s1, x1) ~r:(s2, x2)
        | `Var x1, `Constant, `Var x3 -> add_generic_constraint sys ~o:(Fp.negate s3, x3) (Fp.(s1 * s2), x1)
        | `Constant, `Var x2, `Var x3 -> add_generic_constraint sys ~o:(Fp.negate s3, x3) (Fp.(s1 * s2), x2)
        | `Var x1, `Constant, `Constant -> add_generic_constraint sys ~c:(Fp.negate s3) (Fp.(s1 * s2), x1)
        | `Constant, `Var x2, `Constant -> add_generic_constraint sys ~c:(Fp.negate s3) (Fp.(s1 * s2), x2)
        | `Constant, `Constant, `Var x3 -> add_generic_constraint sys ~c:Fp.(negate s1 * s2) (s3, x3)
        | `Constant, `Constant, `Constant -> assert Fp.(equal s3 Fp.(s1 * s2))
      )

    | Plonk_constraint.T (Poseidon { start; state }) ->

      let reduce_state sys (s : Fp.t Snarky_backendless.Cvar.t array array) : V.t array array =
        Array.map ~f:(Array.map ~f:reduce_to_v) s
      in

      let start = (reduce_state sys [|start|]).(0) in
      let state = reduce_state sys state in

      let add_round_state array = Array.iteri ~f:
          (
            fun i x -> V.Table.add_multi sys.equivalence_classes ~key:x ~data:{ row= sys.next_row; col= i } ;
          ) array;
        sys.next_row <- sys.next_row + 1
      in
      add_round_state start;
      Array.iter ~f:
        (
          fun state -> 
            (* Backend.add_poseidon_constraint sys.backend sys.next_row *)
            add_round_state state
        ) state;
      ()

    | Plonk_constraint.T (EC_add { p1; p2; p3 }) ->
      Array.iteri ~f:
        (
          fun i (px, py) ->
            let (x, y) = reduce_to_v px, reduce_to_v py in
            V.Table.add_multi sys.equivalence_classes ~key:y ~data:{ row= sys.next_row; col= i } ;
            V.Table.add_multi sys.equivalence_classes ~key:x ~data:{ row= sys.next_row + 1; col= i } ;
        ) [|p1; p2; p3|];
      (* Backend.add_ecadd_constraint sys.backend sys.next_row *)
      sys.next_row <- sys.next_row + 2;
      ()

    | Plonk_constraint.T (EC_scale { state }) ->
      let add_ecscale_round (round: Fp.t Snarky_backendless.Cvar.t Scale_round.t) =
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.xt) ~data:{ row= sys.next_row; col= 0 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.b) ~data:{ row= sys.next_row; col= 1 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.yt) ~data:{ row= sys.next_row; col= 2 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.xp) ~data:{ row= sys.next_row+1; col= 0 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.l1) ~data:{ row= sys.next_row+1; col= 1 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.yp) ~data:{ row= sys.next_row+1; col= 2 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.xs) ~data:{ row= sys.next_row+2; col= 0 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.xs) ~data:{ row= sys.next_row+2; col= 1 } ;
        sys.next_row <- sys.next_row + 3 ;
      in
      Array.iter ~f:
        (
          fun round ->
            (* Backend.add_ecscale_constraint sys.backend sys.next_row *)
            add_ecscale_round round
        ) state;
      ()

    | Plonk_constraint.T (EC_endoscale { state }) ->
      let add_endoscale_round (round: Fp.t Snarky_backendless.Cvar.t Endoscale_round.t) =
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.b2i1) ~data:{ row= sys.next_row; col= 0 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.xt) ~data:{ row= sys.next_row; col= 1 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.b2i) ~data:{ row= sys.next_row+1; col= 0 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.xq) ~data:{ row= sys.next_row+1; col= 1 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.yt) ~data:{ row= sys.next_row+1; col= 2 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.xp) ~data:{ row= sys.next_row+2; col= 0 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.l1) ~data:{ row= sys.next_row+2; col= 1 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.yp) ~data:{ row= sys.next_row+2; col= 2 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.xs) ~data:{ row= sys.next_row+3; col= 0 } ;
        V.Table.add_multi sys.equivalence_classes ~key:(reduce_to_v round.xs) ~data:{ row= sys.next_row+3; col= 1 } ;
        sys.next_row <- sys.next_row + 4 ;
      in
      Array.iter ~f:
        (
          fun round ->
            (* Backend.add_endocscale_constraint sys.backend sys.next_row *)
            add_endoscale_round round
        ) state;
      ()

    | constr ->
      failwithf "Unhandled constraint %s"
        Obj.(extension_name (extension_constructor constr))
        ()
end
