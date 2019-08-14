open Core_kernel

(* a GADT type with unversioned input types; type parameters in result *)
module Stable = struct
  module V1 = struct
    module T = struct
      type ('instantiated, 'unbound) t =
        | Foo : int * 'unbound List.t -> (int, 'unbound) t
        | Bar : string * 'unbound Array.t -> (string, 'unbound) t
      [@@deriving version]
    end
  end
end
