module Failure = Verification_failure

module Dummy : module type of Dummy

module Prod : module type of Prod

include Verifier_intf.S with type ledger_proof = Ledger_proof.t
