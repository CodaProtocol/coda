module Make
    (Impl : Snarky_backendless.Snark_intf.S) (Scalar : sig
        type value

        type var
    end) (Group : sig
      open Impl

      type value

      type var

      val scale : value -> Scalar.value -> value

      module Checked : sig
        module Shifted : sig
          module type S =
            Snarky_curves.Shifted_intf
            with type ('a, 'b) checked := ('a, 'b) Checked.t
             and type boolean_var := Boolean.var
             and type curve_var := var

          type 'a m = (module S with type t = 'a)
        end

        val scale :
             (module Shifted.S with type t = 'shifted)
          -> var
          -> Scalar.var
          -> init:'shifted
          -> ('shifted, _) Checked.t

        val scale_generator :
             (module Shifted.S with type t = 'shifted)
          -> Scalar.var
          -> init:'shifted
          -> ('shifted, _) Checked.t
      end
    end) (Message : sig
      type value

      type var

      val hash_to_group :
           constraint_constants:Genesis_constants.Constraint_constants.t
        -> value
        -> Group.value

      module Checked : sig
        val hash_to_group : var -> (Group.var, _) Impl.Checked.t
      end
    end) (Output_hash : sig
      type t

      type var

      val hash :
           constraint_constants:Genesis_constants.Constraint_constants.t
        -> Message.value
        -> Group.value
        -> t

      module Checked : sig
        val hash : Message.var -> Group.var -> (var, _) Impl.Checked.t
      end
    end) : sig
  val eval :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> private_key:Scalar.value
    -> Message.value
    -> Output_hash.t

  module Checked : sig
    val eval :
         'shifted Group.Checked.Shifted.m
      -> private_key:Scalar.var
      -> Message.var
      -> (Output_hash.var, _) Impl.Checked.t

    val eval_and_check_public_key :
         'shifted Group.Checked.Shifted.m
      -> private_key:Scalar.var
      -> public_key:Group.var
      -> Message.var
      -> (Output_hash.var, _) Impl.Checked.t
  end
end = struct
  open Impl

  let eval ~constraint_constants ~private_key m =
    let h = Message.hash_to_group ~constraint_constants m in
    let u = Group.scale h private_key in
    Output_hash.hash ~constraint_constants m u

  module Checked = struct
    open Let_syntax

    let eval (type shifted)
        ((module Shifted) : shifted Group.Checked.Shifted.m) ~private_key m =
      let%bind h = Message.Checked.hash_to_group m in
      let%bind u =
        (* This use of unshift_nonzero is acceptable since if h^private_key = 0 then
           the prover had a bad private key *)
        Group.Checked.scale (module Shifted) h private_key ~init:Shifted.zero
        >>= Shifted.unshift_nonzero
      in
      Output_hash.Checked.hash m u

    let eval_and_check_public_key (type shifted)
        ((module Shifted) : shifted Group.Checked.Shifted.m) ~private_key
        ~public_key message =
      let%bind () =
        with_label __LOC__
          (let%bind public_key_shifted = Shifted.(add zero public_key) in
           Group.Checked.scale_generator
             (module Shifted)
             private_key ~init:Shifted.zero
           >>= Shifted.Assert.equal public_key_shifted)
      in
      eval (module Shifted) ~private_key message
  end
end
