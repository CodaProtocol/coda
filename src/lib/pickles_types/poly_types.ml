module type T0 = sig
  type t
end

module type T1 = sig
  type _ t
end

module type T2 = sig
  type (_, _) t
end

module type T3 = sig
  type (_, _, _) t
end

module type T4 = sig
  type (_, _, _, _) t
end

module type T5 = sig
  type (_, _, _, _, _) t
end
