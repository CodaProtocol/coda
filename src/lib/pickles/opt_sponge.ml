open Core_kernel

(* This module implements snarky functions for a sponge that can *conditionally* absorb input,
   while branching minimally. Specifically, if absorbing N field elements, this sponge can absorb
   a variable subset of N field elements, while performing N + 1 invocations of the sponge's
   underlying permutation. *)

let m = 3

let capacity = 1

let rate = m - capacity

type 'f sponge_state =
  | Absorbing of
      { next_index : 'f Snarky_backendless.Boolean.t
      ; xs : ('f Snarky_backendless.Boolean.t * 'f) list
      }
  | Squeezed of int

type 'f t =
  { mutable state : 'f array
  ; params : 'f Sponge.Params.t
  ; needs_final_permute_if_empty : bool
  ; mutable sponge_state : 'f sponge_state
  }

module Make
    (Impl : Snarky_backendless.Snark_intf.Run with type prover_state = unit)
    (P : Sponge.Intf.Permutation with type Field.t = Impl.Field.t) =
struct
  open P
  open Impl

  type nonrec t = Field.t t

  let state { state; _ } = Array.copy state

  let copy { state; params; sponge_state; needs_final_permute_if_empty } =
    { state = Array.copy state
    ; params
    ; sponge_state
    ; needs_final_permute_if_empty
    }

  let initial_state = Array.init m ~f:(fun _ -> Field.zero)

  let of_sponge { Sponge.state; params; sponge_state } =
    match sponge_state with
    | Squeezed n ->
        { sponge_state = Squeezed n
        ; state = Array.copy state
        ; needs_final_permute_if_empty = true
        ; params
        }
    | Absorbed n -> (
        let abs i =
          { sponge_state = Absorbing { next_index = i; xs = [] }
          ; state = Array.copy state
          ; params
          ; needs_final_permute_if_empty = true
          }
        in
        match n with
        | 0 ->
            abs Boolean.false_
        | 1 ->
            abs Boolean.true_
        | 2 ->
            { sponge_state = Absorbing { next_index = Boolean.false_; xs = [] }
            ; state = P.block_cipher params state
            ; needs_final_permute_if_empty = false
            ; params
            }
        | _ ->
            assert false )

  let create ?(init = initial_state) params =
    { params
    ; state = Array.copy init
    ; needs_final_permute_if_empty = true
    ; sponge_state = Absorbing { next_index = Boolean.false_; xs = [] }
    }

  let () = assert (rate = 2)

  let add_in a i x =
    let i_equals_0 = Boolean.not i in
    let i_equals_1 = i in
    (*
      a.(0) <- a.(0) + i_equals_0 * x
      a.(1) <- a.(1) + i_equals_1 * x *)
    List.iteri [ i_equals_0; i_equals_1 ] ~f:(fun j i_equals_j ->
        let a_j' =
          exists Field.typ
            ~compute:
              As_prover.(
                fun () ->
                  let a_j = read Field.typ a.(j) in
                  if read Boolean.typ i_equals_j then
                    Field.Constant.(a_j + read Field.typ x)
                  else a_j)
        in
        assert_r1cs x (i_equals_j :> Field.t) Field.(a_j' - a.(j)) ;
        a.(j) <- a_j')

  let consume ~needs_final_permute_if_empty ~params ~start_pos input state =
    assert (Array.length state = m) ;
    let n = Array.length input in
    let pos = ref start_pos in
    let cond_permute permute =
      let permuted = P.block_cipher params (Array.copy state) in
      for i = 0 to m - 1 do
        state.(i) <- Field.if_ permute ~then_:permuted.(i) ~else_:state.(i)
      done
    in
    let pairs = n / 2 in
    let remaining = n - (2 * pairs) in
    for i = 0 to pairs - 1 do
      (* Semantically, we want to do this.
         match b, b' with
         | 1, 1 ->
          if p = 0
          then state := perm {state with .0 += x, .1 += y }
          else state := {perm {state with .1 += x} with .0 += y}
         | 1, 0 ->
          if p = 0
          then state := {state with .0 += x}
          else state := perm {state with .1 += x}
         | 0, 1 ->
          if p = 0
          then state := {state with .0 += y }
          else state := perm {state with .1 += y}
         | 0, 0 ->
          state
      *)
      let b, x = input.(2 * i) in
      let b', y = input.((2 * i) + 1) in
      let p = !pos in
      let p' = Boolean.( lxor ) p b in
      pos := Boolean.( lxor ) p' b' ;
      let y = Field.(y * (b' :> t)) in
      let add_in_y_after_perm =
        (* post
           add in
           (1, 1, 1)

           do not add in
           (1, 1, 0)
           (0, 1, 0)
           (0, 1, 1)

           (1, 0, 0)
           (1, 0, 1)
           (0, 0, 0)
           (0, 0, 1)
        *)
        (* Only one case where we add in y after the permutation is applied *)
        Boolean.all [ b; b'; p ]
      in
      let add_in_y_before_perm = Boolean.not add_in_y_after_perm in
      add_in state p Field.(x * (b :> t)) ;
      add_in state p' Field.(y * (add_in_y_before_perm :> t)) ;
      let permute =
        (* (b, b', p)
            true:
            (0, 1, 1)
            (1, 0, 1)
            (1, 1, 0)
            (1, 1, 1)

           false:
            (0, 0, 0)
            (0, 0, 1)
            (0, 1, 0)
            (1, 0, 0)
        *)
        (* (b && b') || (p && (b || b')) *)
        Boolean.(any [ all [ b; b' ]; all [ p; b ||| b' ] ])
      in
      cond_permute permute ;
      add_in state p' Field.(y * (add_in_y_after_perm :> t))
    done ;
    let empty_imput =
      Boolean.not (Boolean.Array.any (Array.map input ~f:fst))
    in
    let should_permute =
      match remaining with
      | 0 ->
          if needs_final_permute_if_empty then Boolean.(empty_imput ||| !pos)
          else !pos
      | 1 ->
          let b, x = input.(n - 1) in
          let p = !pos in
          pos := Boolean.( lxor ) p b ;
          add_in state p Field.(x * (b :> t)) ;
          if needs_final_permute_if_empty then Boolean.any [ p; b; empty_imput ]
          else Boolean.any [ p; b ]
      | _ ->
          assert false
    in
    cond_permute should_permute

  let absorb (t : t) x =
    match t.sponge_state with
    | Absorbing { next_index; xs } ->
        t.sponge_state <- Absorbing { next_index; xs = x :: xs }
    | Squeezed _ ->
        t.sponge_state <- Absorbing { next_index = Boolean.false_; xs = [ x ] }

  let squeeze (t : t) =
    match t.sponge_state with
    | Squeezed n ->
        if n = rate then (
          t.state <- block_cipher t.params t.state ;
          t.sponge_state <- Squeezed 1 ;
          t.state.(0) )
        else (
          t.sponge_state <- Squeezed (n + 1) ;
          t.state.(n) )
    | Absorbing { next_index; xs } ->
        consume ~needs_final_permute_if_empty:t.needs_final_permute_if_empty
          ~start_pos:next_index ~params:t.params (Array.of_list_rev xs) t.state ;
        t.sponge_state <- Squeezed 1 ;
        t.state.(0)

  let%test_module "opt_sponge" =
    ( module struct
      module S = Sponge.Make_sponge (P)

      let%test_unit "correctness" =
        let params : _ Sponge.Params.t =
          let a () =
            Array.init 3 ~f:(fun _ -> Field.(constant (Constant.random ())))
          in
          { mds = Array.init 3 ~f:(fun _ -> a ())
          ; round_constants = Array.init 40 ~f:(fun _ -> a ())
          }
        in
        let gen =
          let open Quickcheck.Generator.Let_syntax in
          let%bind n = Quickcheck.Generator.small_positive_int
          and n_pre = Quickcheck.Generator.small_positive_int in
          let%map xs = List.gen_with_length n Field.Constant.gen
          and bs = List.gen_with_length n Bool.quickcheck_generator
          and pre = List.gen_with_length n_pre Field.Constant.gen in
          (pre, List.zip_exn bs xs)
        in
        Quickcheck.test gen ~trials:10 ~f:(fun (pre, ps) ->
            let filtered =
              List.filter_map ps ~f:(fun (b, x) -> if b then Some x else None)
            in
            let init () =
              let pre =
                exists
                  (Typ.list ~length:(List.length pre) Field.typ)
                  ~compute:(fun () -> pre)
              in
              let s = S.create params in
              List.iter pre ~f:(S.absorb s) ;
              s
            in
            let filtered_res =
              let n = List.length filtered in
              Impl.Internal_Basic.Test.checked_to_unchecked
                (Typ.list ~length:n Field.typ)
                Field.typ
                (fun xs ->
                  make_checked (fun () ->
                      let s = init () in
                      List.iter xs ~f:(S.absorb s) ;
                      S.squeeze s))
                filtered
            in
            let opt_res =
              let n = List.length ps in
              Impl.Internal_Basic.Test.checked_to_unchecked
                (Typ.list ~length:n (Typ.tuple2 Boolean.typ Field.typ))
                Field.typ
                (fun xs ->
                  make_checked (fun () ->
                      let s =
                        if List.length pre = 0 then create params
                        else of_sponge (init ())
                      in
                      List.iter xs ~f:(absorb s) ;
                      squeeze s))
                ps
            in
            if not (Field.Constant.equal filtered_res opt_res) then
              failwithf
                !"hash(%{sexp:Field.Constant.t list}) = %{sexp:Field.Constant.t}\n\
                  hash(%{sexp:(bool * Field.Constant.t) list}) = \
                  %{sexp:Field.Constant.t}"
                filtered filtered_res ps opt_res ())
    end )
end
