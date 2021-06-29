include module type of Genesis_proof.T with type t = Genesis_proof.T.t

val for_unit_tests : t Lazy.t

val compiled_inputs : Genesis_proof.Inputs.t Lazy.t

val compiled : t Lazy.t option
