[%%import
"../../config.mlh"]

open Core
open Fold_lib
open Snark_params.Tick

module Aux_hash = struct
  let length_in_bits = 256

  let length_in_bytes = length_in_bits / 8

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = string [@@deriving sexp, equal, compare, hash]

      let to_latest = Fn.id

      module Base58_check = Base58_check.Make (struct
        let description = "Aux hash"

        let version_byte =
          Base58_check.Version_bytes.staged_ledger_hash_aux_hash
      end)

      let to_yojson s = `String (Base58_check.encode s)

      let of_yojson = function
        | `String s -> (
          match Base58_check.decode s with
          | Error e ->
              Error
                (sprintf "Aux_hash.of_yojson, bad Base58Check:%s"
                   (Error.to_string_hum e))
          | Ok x ->
              Ok x )
        | _ ->
            Error "Aux_hash.of_yojson expected `String"
    end
  end]

  [%%define_locally
  Stable.Latest.(to_yojson, of_yojson)]

  let of_bytes = Fn.id

  let to_bytes = Fn.id

  let dummy : t = String.init length_in_bytes ~f:(fun _ -> '\000')
end

module Pending_coinbase_aux = struct
  let length_in_bits = 256

  let length_in_bytes = length_in_bits / 8

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = string [@@deriving sexp, equal, compare, hash]

      let to_latest = Fn.id

      module Base58_check = Base58_check.Make (struct
        let description = "Pending coinbase aux"

        let version_byte =
          Base58_check.Version_bytes.staged_ledger_hash_pending_coinbase_aux
      end)

      let to_yojson s = `String (Base58_check.encode s)

      let of_yojson = function
        | `String s -> (
          match Base58_check.decode s with
          | Ok x ->
              Ok x
          | Error e ->
              Error
                (sprintf "Pending_coinbase_aux.of_yojson, bad Base58Check:%s"
                   (Error.to_string_hum e)) )
        | _ ->
            Error "Pending_coinbase_aux.of_yojson expected `String"
    end
  end]

  [%%define_locally
  Stable.Latest.(to_yojson, of_yojson)]

  let dummy : t = String.init length_in_bytes ~f:(fun _ -> '\000')
end

module Non_snark = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { ledger_hash: Ledger_hash.Stable.V1.t
        ; aux_hash: Aux_hash.Stable.V1.t
        ; pending_coinbase_aux: Pending_coinbase_aux.Stable.V1.t }
      [@@deriving sexp, equal, compare, hash, yojson]

      let to_latest = Fn.id
    end
  end]

  type value = t [@@deriving sexp, compare, hash, yojson]

  let dummy : t Lazy.t =
    lazy
      { ledger_hash= Ledger_hash.empty_hash
      ; aux_hash= Aux_hash.dummy
      ; pending_coinbase_aux= Pending_coinbase_aux.dummy }

  let genesis ~genesis_ledger_hash : t =
    { ledger_hash= genesis_ledger_hash
    ; aux_hash= Aux_hash.dummy
    ; pending_coinbase_aux= Pending_coinbase_aux.dummy }

  type var = Boolean.var list

  let length_in_bits = 256

  let digest ({ledger_hash; aux_hash; pending_coinbase_aux} : t) =
    let h = Digestif.SHA256.init () in
    let h = Digestif.SHA256.feed_string h (Ledger_hash.to_bytes ledger_hash) in
    let h = Digestif.SHA256.feed_string h aux_hash in
    let h = Digestif.SHA256.feed_string h pending_coinbase_aux in
    Digestif.SHA256.(get h |> to_raw_string)

  let fold t = Fold.string_bits (digest t)

  let to_input t = Random_oracle.Input.bitstring (Fold.to_list (fold t))

  let ledger_hash ({ledger_hash; _} : t) = ledger_hash

  let aux_hash ({aux_hash; _} : t) = aux_hash

  let of_ledger_aux_coinbase_hash aux_hash ledger_hash pending_coinbase_aux : t
      =
    {aux_hash; ledger_hash; pending_coinbase_aux}

  let var_to_input = Random_oracle.Input.bitstring

  let var_of_t t : var =
    List.map (Fold.to_list @@ fold t) ~f:Boolean.var_of_value

  [%%if
  proof_level = "check"]

  let warn_improper_transport () = ()

  [%%else]

  let warn_improper_transport () =
    printf "WARNING: improperly transporting staged-ledger-hash\n"

  [%%endif]

  let typ : (var, value) Typ.t =
    Typ.transport (Typ.list ~length:length_in_bits Boolean.typ)
      ~there:(Fn.compose Fold.to_list fold) ~back:(fun _ ->
        (* If we put a failwith here, we lose the ability to printf-inspect
        * anything that uses staged-ledger-hashes from within Checked
        * computations. It's useful when debugging to dump the protocol state
        * and so we can just lie here instead. *)
        warn_improper_transport () ; Lazy.force dummy )
end

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('non_snark, 'pending_coinbase_hash) t =
        {non_snark: 'non_snark; pending_coinbase_hash: 'pending_coinbase_hash}
      [@@deriving sexp, equal, compare, hash, yojson, hlist]
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    (** Staged ledger hash has two parts
      1) merkle root of the pending coinbases
      2) ledger hash, aux hash, and the FIFO order of the coinbase stacks(Non snark).
      Only part 1 is required for blockchain snark computation and therefore the remaining fields of the staged ledger are grouped together as "Non_snark"
      *)
    type t =
      ( Non_snark.Stable.V1.t
      , Pending_coinbase.Hash_versioned.Stable.V1.t )
      Poly.Stable.V1.t
    [@@deriving sexp, equal, compare, hash, yojson]

    let to_latest = Fn.id
  end
end]

type ('a, 'b) t_ = ('a, 'b) Poly.t

type value = t [@@deriving sexp, equal, compare, hash]

type var = (Non_snark.var, Pending_coinbase.Hash.var) t_

include Hashable.Make (Stable.Latest)

let ledger_hash ({non_snark; _} : t) = Non_snark.ledger_hash non_snark

let aux_hash ({non_snark; _} : t) = Non_snark.aux_hash non_snark

let pending_coinbase_hash ({pending_coinbase_hash; _} : t) =
  pending_coinbase_hash

let pending_coinbase_hash_var ({pending_coinbase_hash; _} : var) =
  pending_coinbase_hash

let of_aux_ledger_and_coinbase_hash aux_hash ledger_hash pending_coinbase : t =
  { non_snark=
      Non_snark.of_ledger_aux_coinbase_hash aux_hash ledger_hash
        (Pending_coinbase.hash_extra pending_coinbase)
  ; pending_coinbase_hash= Pending_coinbase.merkle_root pending_coinbase }

let genesis ~(constraint_constants : Genesis_constants.Constraint_constants.t)
    ~genesis_ledger_hash : t =
  let pending_coinbase =
    Pending_coinbase.create ~depth:constraint_constants.pending_coinbase_depth
      ()
    |> Or_error.ok_exn
  in
  { non_snark= Non_snark.genesis ~genesis_ledger_hash
  ; pending_coinbase_hash= Pending_coinbase.merkle_root pending_coinbase }

let var_of_t ({pending_coinbase_hash; non_snark} : t) : var =
  let non_snark = Non_snark.var_of_t non_snark in
  let pending_coinbase_hash =
    Pending_coinbase.Hash.var_of_t pending_coinbase_hash
  in
  {non_snark; pending_coinbase_hash}

let to_input ({non_snark; pending_coinbase_hash} : t) =
  Random_oracle.Input.(
    append
      (Non_snark.to_input non_snark)
      (field (pending_coinbase_hash :> Field.t)))

let var_to_input ({non_snark; pending_coinbase_hash} : var) =
  Random_oracle.Input.(
    append
      (Non_snark.var_to_input non_snark)
      (field (Pending_coinbase.Hash.var_to_hash_packed pending_coinbase_hash)))

let data_spec =
  let open Data_spec in
  [Non_snark.typ; Pending_coinbase.Hash.typ]

let typ : (var, t) Typ.t =
  Typ.of_hlistable data_spec ~var_to_hlist:Poly.to_hlist
    ~var_of_hlist:Poly.of_hlist ~value_to_hlist:Poly.to_hlist
    ~value_of_hlist:Poly.of_hlist
