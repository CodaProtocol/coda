open Core_kernel
open Pickles_types
open Import
open Poly_types
open Hlist

(* Compute the domains corresponding to wrap_main *)
module Make (A : T0) (A_value : T0) = struct
  module I = Inductive_rule.T (A) (A_value)

  let prev (type xs ys ws hs) ~self ~(choices : (xs, ys, ws, hs) H4.T(I).t) =
    let module M_inner =
      H4.Map
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
        end)
    in
    let module M =
      H4.Map
        (I)
        (H4.T
           (E04 (Domains)))
           (struct
             let f : type vars values env widths heights.
                    (vars, values, widths, heights) I.t
                 -> (vars, values, widths, heights) H4.T(E04(Domains)).t =
              fun rule -> M_inner.f rule.prevs
           end)
    in
    M.f choices

  let result =
    lazy
      (let x =
         let (T (typ, conv)) = Impls.Wrap.input () in
         Domain.Pow_2_roots_of_unity
           (Int.ceil_log2 (Impls.Wrap.Data_spec.size [typ]))
       in
       {Common.wrap_domains with x})

  let f_debug full_signature num_choices choices_length ~self ~choices
      ~max_branching =
    let num_choices = Hlist.Length.to_nat choices_length in
    let dummy_step_domains =
      Vector.init num_choices ~f:(fun _ -> Fix_domains.rough_domains)
    in
    let dummy_step_widths =
      Vector.init num_choices ~f:(fun _ -> Nat.to_int (Nat.Add.n max_branching))
    in
    let dummy_step_keys =
      lazy
        (Vector.init num_choices ~f:(fun _ ->
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
    let prev_domains = prev ~self ~choices in
    Timer.clock __LOC__ ;
    let _, main =
      Wrap_main.wrap_main full_signature choices_length dummy_step_keys
        dummy_step_widths dummy_step_domains prev_domains max_branching
    in
    Timer.clock __LOC__ ;
    let t =
      Fix_domains.domains (module Impls.Wrap) (Impls.Wrap.input ()) main
    in
    Timer.clock __LOC__ ; t

  let f full_signature num_choices choices_length ~self ~choices ~max_branching
      =
    let res = Lazy.force result in
    ( if debug then
      let res' =
        f_debug full_signature num_choices choices_length ~self ~choices
          ~max_branching
      in
      [%test_eq: Domains.t] res res' ) ;
    res
end
