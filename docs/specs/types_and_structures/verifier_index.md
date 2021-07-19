# VerifierIndex

The in-memory verifier index structure contains the verifier key and is defined as follows.

| Field              | Type                            | Description |
| - | - | - |
| `domain_size`      | `u64`                           | Domain size |
| `max_poly_size`    | `u64`                           | Maximal size of polynomial section |
| `max_quot_size`    | `u64`                           | Maximal size of quotient polynomial according to the supported constraints |
| `srs`              | `SRSValue<'a, GAffine>`         | Polynomial commitment keys |
| `sigma_comm`       | `[PolyComm<GAffine>; 3]`        | Permutation commitments |
| `ql_comm`          | `PolyComm<GAffine>`             | Left input wire commitment |
| `qr_comm`          | `PolyComm<GAffine>`             | Right input wire commitment |
| `qo_comm`          | `PolyComm<GAffine>`             | Output selector poly commitment |
| `qm_comm`          | `PolyComm<GAffine>`             | Multiplication commitment |
| `qc_comm`          | `PolyComm<GAffine>`             | Constant wire commitment |
| `rcm_comm`         | `[PolyComm<GAffine>; 3]`        | Round constant polynomial commitments |
| `psm_comm`         | `PolyComm<GAffine>`             | Poseidon constraint selector polynomial commitment |
| `add_comm`         | `PolyComm<GAffine>`             | EC addition selector polynomial commitment |
| `mul_comm`         | `[PolyComm<GAffine>; 2]`        | EC variable base scalar multiplication selector polynomial commitments |
| `emul_comm`        | `[PolyComm<GAffine>; 3]`        | Endoscalar muplication selector polynomial commitments |
| `r`                | `ScalarField`                   | Coordinate shift for right wires |
| `o`                | `ScalarField`                   | Coordinate shift for output wires |
| `zkpm`             | `DensePolynomial<Fr<GAffine>>`  | Zero-knowledge polynomial |
| `w`                | `Fr<G>`                         | Root of unity for zero-knowledge |
| `endo`             | `Fr<G>`                         | Endoscalar coefficient |
| `fr_sponge_params` | `ArithmeticSpongeParams<Fr<G>>` | Random oracle Fr argument parameters |
| `fq_sponge_params` | `ArithmeticSpongeParams<Fq<G>>` | Random oracle Fq argument parameters |
