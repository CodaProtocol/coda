# Bootstrap Controller

Bootstrap controller is the component that responsible for constructing the
root of transition frontier when node first comes up online or falls behind
the canonical chain too much that catchup.

Bootstrap controller does the following steps to initialize the root of
transition frontier:
1) Download the `snarked_ledger` using `Bootstrap.sync_ledger` function.
Incoming block gossips would be used to update the target `snarked_ledger`.
2) Download the `scan_state` and `pending_coinbases`.
3) Construct the `staged_ledger` from `snarked_ledger`, `scan_state` and
`pending_coinbases`.
4) Download relevent `local_state` by `Consensus.Hooks.sync_local_state`.
5) Reset persistent frontier and construct the new frontier. The new frontier
would only have the root breadcrumb. Transitions collected during bootstrap
phase would be used for catchup.

At any of the step, if it fails, bootstrap controller would loop back to
step 1.
