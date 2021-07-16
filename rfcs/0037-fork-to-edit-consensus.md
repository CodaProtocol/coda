## Summary

This rfc considers various scenarios that lead to editing certain consensus parameters and explains what other changes should accompany the corresponding edits.


## Motivation

There are various scenarios that motivate editing consensus parameters. For example, SNARK research is very active and new, more-efficient constructions might appear. This can help us reduce the slot duration. Another example is further fine tuning of the network can help reduce the value of `\Delta`, the network delay parameter. This will allow us to increase the active-slot coefficient `f` while still conforming to the consensus protocol constraints.


## Editing Consensus Parameters

We will consider the following main consensus paramters:
There are other parameters, such as window length `\omega`, which may also be edited which will be dealt in a future rfc.

**Slot duration**: Editing this parameter alone does not require editing the other parameters.

**Network delay \Delta**:
*Background*: When `\Delta = 0` (i.e., when messages reach all honest nodes within a period of slot duration) then there is much less restriction on the active-slot coefficient `f`. Specifically, for non-zero values of `\Delta`, `f` cannot be above 0.5. On the other hand, for `\Delta = 0`, we can set `f` close to 1.
If we are somehow able to get `\Delta=0`, then `f` and `k` need to be recomputed. Furthermore, we may or may not edit the honest stake assumption `\alpha` (i.e., while recomputing `f` and `k`, one may find more favorable values of these parameters for different `\alpha`).

**\alpha**: If we would like to edit the honest stake assumption, then `k` and `f` need to be recomputed.

**f**: Changing `f` might affect the validity range of `\alpha` and thereby the value of `k`, in which case `\alpha` and `k` need to be recomputed.

**k**: It is unlikely that we would like to change this parameter alone. Usually, this parameter is edited when other parameters need to be edited.



|  | Slot duration | Network delay `\Delta` | `\alpha` | `f` | `k` |
|:-:|:-:|:-:|:-:|:-:|:-:|
| Slot duration                  |   |   |   |   |   |
| Network delay `\Delta` |   |   |  [x] | [x]  | [x]  |
| `\alpha`                         |   |   |   | [x]  | [x]  |
| `f`                                   |   |   | [x]  |   |  [x] |
| `k`                                   |   |   |   |   |   |

Table 1. Parameters that need to be/likely to be recomputed upon editing


## Timing Constraints on Pushing the Hard Fork

For consensus security reasons, most consensus parameters cannot be edited mid-way through an epoch. Below are the detailed constraints.
- *Slot duration* can be edited mid way through an epoch.
- Edits to *`\Delta`, `\alpha`, `f`,* and *`k`*  can pushed so that they take effect only at the beginning of an epoch (the first slot of an epoch).


## Future Work

This RFC does not consider the parameters, such as window length, pertaining to shifting window mechanism within the consensus protocol.



