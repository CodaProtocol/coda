module type Basic = sig
  type t

  val fold : t -> init:'a -> f:('a -> bool -> 'a) -> 'a
end

module type S = sig
  include Basic

  val iter : t -> f:(bool -> unit) -> unit

  val to_bits : t -> bool list
end

module Snarkable = struct
  module type Basic = sig
    type (_, _) typ

    type (_, _) checked

    type boolean_var

    (*     module Bits : S *)

    module Packed : sig
      type var

      type value

      val typ : (var, value) typ
    end

    module Unpacked : sig
      type var

      type value

      val typ : (var, value) typ

      val var_to_bits : var -> boolean_var list

      val var_of_value : value -> var
    end
  end

  module type Lossy = sig
    include Basic

    val project_value : Unpacked.value -> Packed.value

    val unpack_value : Packed.value -> Unpacked.value

    val project_var : Unpacked.var -> Packed.var

    val choose_preimage_var : Packed.var -> (Unpacked.var, _) checked
  end

  module type Faithful = sig
    include Basic

    val pack_value : Unpacked.value -> Packed.value

    val unpack_value : Packed.value -> Unpacked.value

    val pack_var : Unpacked.var -> Packed.var

    val unpack_var : Packed.var -> (Unpacked.var, _) checked
  end

  module type Small = sig
    include Faithful

    type comparison_result

    val compare_var :
      Unpacked.var -> Unpacked.var -> (comparison_result, _) checked

    val increment_var : Unpacked.var -> (Unpacked.var, _) checked

    val increment_if_var :
      Unpacked.var -> boolean_var -> (Unpacked.var, _) checked

    val assert_equal_var : Unpacked.var -> Unpacked.var -> (unit, _) checked

    val equal_var : Unpacked.var -> Unpacked.var -> (boolean_var, _) checked

    val if_ :
         boolean_var
      -> then_:Unpacked.var
      -> else_:Unpacked.var
      -> (Unpacked.var, _) checked
  end
end
