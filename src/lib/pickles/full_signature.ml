open Pickles_types

type ('max_num_input_proofs, 'num_rules, 'maxes) t =
  { prev_num_input_proofss_per_slot:
      ((int, 'num_rules) Vector.t, 'max_num_input_proofs) Vector.t
  ; maxes:
      (module Hlist.Maxes.S
         with type length = 'max_num_input_proofs
          and type ns = 'maxes) }
