module type Field = sig
  type t

  val zero : t

  val ( * ) : t -> t -> t

  val ( + ) : t -> t -> t
end

module type Operations = sig
  module Field : Field

  val add_block : state:Field.t array -> Field.t array -> unit

  val apply_matrix : Field.t array array -> Field.t array -> Field.t array
end

module Inputs = struct
  module type Common = sig
    module Field : Field

    val to_the_alpha : Field.t -> Field.t

    module Operations : Operations with module Field := Field
  end

  module type Rescue = sig
    include Common

    val alphath_root : Field.t -> Field.t
  end
end

module type Permutation = sig
  module Field : Field

  val add_block : state:Field.t array -> Field.t array -> unit

  val block_cipher : Field.t Params.t -> Field.t array -> Field.t array
end
