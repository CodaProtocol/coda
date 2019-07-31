open Core_kernel

module type Inputs_intf = sig
  type value

  type proof_elem

  type context

  type hash [@@deriving eq]

  val to_proof_elem : value -> proof_elem

  val get_previous : context:context -> value -> value option

  val hash : hash -> proof_elem -> hash
end

module Make_intf (Input : Inputs_intf) = struct
  module type S = sig
    val prove :
         ?length:int
      -> context:Input.context
      -> Input.value
      -> Input.value * Input.proof_elem list

    val verify :
         init:Input.hash
      -> Input.proof_elem list
      -> Input.hash
      -> Input.hash Non_empty_list.t option
  end
end

module Make (Input : Inputs_intf) : Make_intf(Input).S = struct
  open Input

  let prove ?length ~context last =
    let rec find_path ~length value =
      if length = Some 0 then (value, [])
      else
        Option.value_map (get_previous ~context value) ~default:(value, [])
          ~f:(fun parent ->
            let first, proofs =
              find_path ~length:(Option.map length ~f:pred) parent
            in
            (first, to_proof_elem value :: proofs) )
    in
    let first, proofs = find_path ~length last in
    (first, List.rev proofs)

  let verify ~init (merkle_list : proof_elem list) underlying_hash =
    let hashes =
      List.fold merkle_list ~init:(Non_empty_list.singleton init)
        ~f:(fun acc proof_elem ->
          Non_empty_list.cons (hash (Non_empty_list.head acc) proof_elem) acc
      )
    in
    if equal_hash underlying_hash (Non_empty_list.head hashes) then Some hashes
    else None
end
