open Core_kernel
open Pickles_types
open Import
open Poly_types
open Hlist

(* Compute the domains corresponding to wrap_main *)
module Make (A : T0) (A_value : T0) = struct
  module I = struct
    type ( 'prev_vars
         , 'prev_values
         , 'prev_num_input_proofss
         , 'prev_num_ruless )
         t =
      ( 'prev_vars * unit
      , 'prev_values * unit
      , 'prev_num_input_proofss * unit
      , 'prev_num_ruless * unit )
      Inductive_rule.T(A)(A_value).t
  end

  let prev (type xs ys ws hs) ~self ~(rules : (xs, ys, ws, hs) H4.T(I).t) =
    let module M_inner =
      H4.Map
        (H4.T
           (Tag))
           (H4.T
              (E04 (Domains)))
              (H4.Map
                 (Tag)
                 (E04 (Domains))
                 (struct
                   let f : type a b c d. (a, b, c, d) Tag.t -> Domains.t =
                    fun t ->
                     Types_map.lookup_map t ~self:self.Tag.id
                       ~default:Common.wrap_domains ~f:(function
                       | `Side_loaded _ ->
                           Common.wrap_domains
                       | `Compiled d ->
                           d.wrap_domains )
                 end))
    in
    let module M =
      H4.Map
        (I)
        (H4.Singleton
           (H4.T
              (E04 (Domains))))
              (struct
                let f : type vars values env num_input_proofss num_ruless.
                       (vars, values, num_input_proofss, num_ruless) I.t
                    -> ( vars
                       , values
                       , num_input_proofss
                       , num_ruless )
                       H4.Singleton(H4.T(E04(Domains))).t =
                 fun rule -> M_inner.f rule.prevs
              end)
    in
    let rec unwrap_prev_domains : type a b c d.
           (a, b, c, d) H4.T(H4.Singleton(H4.T(E04(Domains)))).t
        -> (a, b, c, d) H4.T(H4.T(E04(Domains))).t = function
      | [] ->
          []
      | [y] :: xs ->
          let ys = unwrap_prev_domains xs in
          y :: ys
    in
    unwrap_prev_domains (M.f rules)

  let result =
    lazy
      (let x =
         let (T (typ, conv)) = Impls.Wrap.input () in
         Domain.Pow_2_roots_of_unity
           (Int.ceil_log2 (Impls.Wrap.Data_spec.size [typ]))
       in
       {Common.wrap_domains with x})

  let f_debug full_signature num_rules rules_length ~self ~rules
      ~max_num_input_proofs =
    let num_rules = Hlist.Length.to_nat rules_length in
    let dummy_step_domains =
      Vector.init num_rules ~f:(fun _ -> Fix_domains.rough_domains)
    in
    let dummy_rules_num_input_proofs =
      Vector.init num_rules ~f:(fun _ ->
          Nat.to_int (Nat.Add.n max_num_input_proofs) )
    in
    let dummy_step_keys =
      lazy
        (Vector.init num_rules ~f:(fun _ ->
             let g = Backend.Tock.Inner_curve.(to_affine_exn one) in
             let g =
               Array.create g
                 ~len:
                   (Common.index_commitment_length
                      ~max_degree:Common.Max_degree.step
                      Fix_domains.rough_domains.h)
             in
             Verification_key.dummy_commitments g ))
    in
    let prev_domains = prev ~self ~rules in
    Timer.clock __LOC__ ;
    let _, main =
      Wrap_main.wrap_main full_signature rules_length dummy_step_keys
        dummy_rules_num_input_proofs dummy_step_domains prev_domains
        max_num_input_proofs
    in
    Timer.clock __LOC__ ;
    let t =
      Fix_domains.domains (module Impls.Wrap) (Impls.Wrap.input ()) main
    in
    Timer.clock __LOC__ ; t

  let f (type l) full_signature num_rules (rules_length : (l, _) Length.t)
      ~self ~rules ~max_num_input_proofs =
    let res = Lazy.force result in
    ( if debug then
      let res' =
        f_debug full_signature num_rules rules_length ~self ~rules
          ~max_num_input_proofs
      in
      [%test_eq: Domains.t] res res' ) ;
    res
end
