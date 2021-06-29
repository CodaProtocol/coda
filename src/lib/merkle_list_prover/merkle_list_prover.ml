open Core_kernel

module type Inputs_intf = sig
  type value

  type proof_elem

  type context

  module M : Monad.S

  val to_proof_elem : value -> proof_elem

  val get_previous : context:context -> value -> value option M.t
end

module Make_intf (M : Monad.S) (Input : Inputs_intf with module M := M) =
struct
  module type S = sig
    val prove :
         ?length:int
      -> context:Input.context
      -> Input.value
      -> (Input.value * Input.proof_elem list) M.t
  end
end

module Make (M : Monad.S) (Input : Inputs_intf with module M := M) :
  Make_intf(M)(Input).S = struct
  open Input

  let prove ?length ~context last =
    let open M.Let_syntax in
    let rec find_path ~length value =
      if [%equal: int option] length (Some 0) then M.return (value, [])
      else
        match%bind get_previous ~context value with
        | None ->
            M.return (value, [])
        | Some parent ->
            let%map first, proofs =
              find_path ~length:(Option.map length ~f:pred) parent
            in
            (first, to_proof_elem value :: proofs)
    in
    let%map first, proofs = find_path ~length last in
    (first, List.rev proofs)
end

module Make_ident = Make (Monad.Ident)
