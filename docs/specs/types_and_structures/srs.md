# SRS<G: CommitmentCurve>

The in-memory verifier index structure contains the verifier key and is defined as follows.

| Field    | Type             | Description |
| - | -    | - |
| `g`      | `Vector<G>`      | For committing polynomials |
| `h`      | `G`              | Blinding factor |
| `endo_r` | `G::ScalarField` | Curve endomorphism coefficient r |
| `endo_q` | `G::BaseField`   | Curve endomorphism coefficient q |
