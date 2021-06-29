# find-swapped-balances.awk -- find swapped combined fee transfer balances in replayer logs

(match($0,"Starting")) { LAST_STARTING = $0 }
(match($0,"Applying combined")) { COMBINED_FEE_TRANSFER = $0; NEW_COMBINED = 1 }
# Print only when there is a "Claimed" Message suggesting balances are swapped
(match($0,"Claimed") && NEW_COMBINED) \
  { print "---------------"; print LAST_STARTING; print COMBINED_FEE_TRANSFER; NEW_COMBINED = 0 }
