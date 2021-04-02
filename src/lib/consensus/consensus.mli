[%%import "../../config.mlh"]

module Intf : module type of Intf

[%%if consensus_mechanism = "proof_of_stake"]

include
  module type of Proof_of_stake
  with module Exported := Proof_of_stake.Exported
   and type Data.Block_data.t = Proof_of_stake.Data.Block_data.t
   and type Data.Consensus_state.Value.Stable.V1.t =
              Proof_of_stake.Data.Consensus_state.Value.Stable.V1.t

[%%else]

[%%show consesus_mechanism]

[%%optcomp.error "invalid value for \"consensus_mechanism\""]

[%%endif]

module Proof_of_stake = Proof_of_stake
module Epoch = Epoch
module Slot = Slot
module Global_slot = Global_slot
