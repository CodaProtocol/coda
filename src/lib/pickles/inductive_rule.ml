open Core_kernel
open Pickles_types.Hlist

module B = struct
  type t = Impls.Step.Boolean.var
end

(* This type models an "inductive rule". It includes
   - the list of previous statements which this one assumes
   - the snarky main function
   - an unchecked version of the main function which computes the "should verify" flags that
     allow predecessor proofs to conditionally fail to verify
*)
type ( 'prev_vars
     , 'prev_values
     , 'prev_num_input_proofss
     , 'prev_num_ruless
     , 'a_var
     , 'a_value )
     t =
  { identifier: string
  ; prevs:
      ( 'prev_vars
      , 'prev_values
      , 'prev_num_input_proofss
      , 'prev_num_ruless )
      H4.T(Tag).t
  ; main: 'prev_vars H1.T(Id).t -> 'a_var -> 'prev_vars H1.T(E01(B)).t
  ; main_value:
      'prev_values H1.T(Id).t -> 'a_value -> 'prev_vars H1.T(E01(Bool)).t }

module T (Statement : T0) (Statement_value : T0) = struct
  type nonrec ( 'prev_vars
              , 'prev_values
              , 'prev_num_input_proofss
              , 'prev_num_ruless )
              t =
    ( 'prev_vars
    , 'prev_values
    , 'prev_num_input_proofss
    , 'prev_num_ruless
    , Statement.t
    , Statement_value.t )
    t
end
