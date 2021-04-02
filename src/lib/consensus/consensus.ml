[%%import
"/src/config.mlh"]

module Intf = Intf

[%%if
consensus_mechanism = "proof_of_stake"]

include Proof_of_stake

[%%else]

[%%show
consesus_mechanism]

[%%optcomp.error
"invalid value for \"consensus_mechanism\""]

[%%endif]

module Proof_of_stake = Proof_of_stake
module Epoch = Epoch
module Slot = Slot
module Global_slot = Global_slot
