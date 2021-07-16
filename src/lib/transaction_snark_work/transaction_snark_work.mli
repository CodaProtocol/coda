open Core_kernel
open Currency
open Signature_lib

module Statement : sig
  type t = Transaction_snark.Statement.t One_or_two.t
  [@@deriving compare, sexp, yojson, equal]

  include Hashable.S with type t := t

  module Stable : sig
    module V2 : sig
      type t [@@deriving bin_io, compare, sexp, version, yojson, equal]

      include Hashable.S_binable with type t := t
    end

    module V1 : sig
      type t [@@deriving bin_io, compare, sexp, version, yojson, equal]

      include Hashable.S_binable with type t := t

      val to_latest : t -> V2.t
    end
  end
  with type V2.t = t

  val gen : t Quickcheck.Generator.t

  val compact_json : t -> Yojson.Safe.t

  val work_ids : t -> int One_or_two.t
end

module Info : sig
  type t =
    { statements : Statement.Stable.V2.t
    ; work_ids : int One_or_two.Stable.V1.t
    ; fee : Fee.Stable.V1.t
    ; prover : Public_key.Compressed.Stable.V1.t
    }
  [@@deriving to_yojson, sexp, compare]

  module Stable : sig
    module V2 : sig
      type t [@@deriving compare, to_yojson, version, sexp, bin_io]
    end

    module V1 : sig
      type t [@@deriving compare, to_yojson, version, sexp, bin_io]

      val to_latest : t -> V2.t
    end
  end
  with type V2.t = t
end

(* TODO: The SOK message actually should bind the SNARK to
       be in this particular bundle. The easiest way would be to
       SOK with
       H(all_statements_in_bundle || fee || public_key)
    *)

type t =
  { fee : Fee.t
  ; proofs : Ledger_proof.t One_or_two.t
  ; prover : Public_key.Compressed.t
  }
[@@deriving compare, sexp, yojson]

val fee : t -> Fee.t

val info : t -> Info.t

val statement : t -> Statement.t

module Stable : sig
  module V2 : sig
    type t [@@deriving sexp, compare, bin_io, yojson, version]
  end

  module V1 : sig
    type t [@@deriving sexp, compare, bin_io, yojson, version]

    val to_latest : t -> V2.t
  end
end
with type V2.t = t

type unchecked = t

module Checked : sig
  type nonrec t = t =
    { fee : Fee.t
    ; proofs : Ledger_proof.t One_or_two.t
    ; prover : Public_key.Compressed.t
    }
  [@@deriving sexp, compare, to_yojson]

  module Stable : module type of Stable

  val create_unsafe : unchecked -> t

  val statement : t -> Statement.t
end

val forget : Checked.t -> t
