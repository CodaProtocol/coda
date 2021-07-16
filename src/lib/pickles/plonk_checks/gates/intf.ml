module type Field_intf = sig
  type t

  val size_in_bits : int

  val one : t

  val of_int : int -> t

  val ( * ) : t -> t -> t

  val ( / ) : t -> t -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val negate : t -> t
end

module type Five_wires_gate_intf = sig
  val range : int * int

  module Aux_data : sig
    type 'f t
  end

  type 'f checks

  type 'f check_evals

  val checks :
       (module Field_intf with type t = 'f)
    -> e0:'f Five_wires_evals.t
    -> e1:'f Five_wires_evals.t
    -> 'f Aux_data.t
    -> 'f checks

  val check_evals :
       (module Field_intf with type t = 'f)
    -> (int * int -> int -> 'f)
    -> e0:'f Five_wires_evals.t
    -> e1:'f Five_wires_evals.t
    -> 'f Aux_data.t
    -> 'f check_evals
end
