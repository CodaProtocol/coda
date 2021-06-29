open Core_kernel

module type Inputs_intf = sig
  type proof_elem

  type hash [@@deriving equal]

  val hash : hash -> proof_elem -> hash
end

module Make_intf (Input : Inputs_intf) = struct
  module type S = sig
    val verify :
         init:Input.hash
      -> Input.proof_elem list
      -> Input.hash
      -> Input.hash Non_empty_list.t option
  end
end

module Make (Input : Inputs_intf) : Make_intf(Input).S = struct
  open Input

  let verify ~init merkle_list target_hash =
    let hashes =
      List.fold merkle_list ~init:(Non_empty_list.singleton init)
        ~f:(fun acc proof_elem ->
          Non_empty_list.cons (hash (Non_empty_list.head acc) proof_elem) acc
      )
    in
    if equal_hash target_hash (Non_empty_list.head hashes) then Some hashes
    else None
end
