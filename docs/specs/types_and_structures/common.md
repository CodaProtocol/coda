# Common types and structures

Table of Contents
* [Basic types](#basic-types)
* [Composite types](#composite-types)

## Basic types

* `uN`: `N`-bit unsigned integer (where `N in [8, 16, 32, 64, 128, 256]`) in `little-endian` byte order
* `bool`: `true` or `false`
* `String` : [`std::string::String`](https://doc.rust-lang.org/std/string/struct.String.html)
* `[T; N]`: Array of type `T` containing `N` elements, indexed starting from `0`

## Composite types

### `Option<T>`

An optional value that eith either `Some(T)` or `None`, see [`std::option`](https://doc.rust-lang.org/std/option/)

### `Result<T, E>`

An enum with variants `OK(T)` for success and `Err(E)` for failure, see [`std::result`](https://doc.rust-lang.org/std/result/)

### `Vector<T>`

Variable length vector of type `T`

| Field     | Type  | Description |
| - | - | - |
| `length`  | `u64` | Number of elements `N` |
| `e0`      | `T`   | 0th element |
| `e1`      | `T`   | 1st element |
| ...       | ...   | ... |
| `eN`      | `T`   | Nth element|

### `GAffine`

`TODO`

### `PolyComm<T>`

| Field       | Type        | Description |
| - | - | - |
| `unshifted` | `Vector<T>` | |
| `shifted`   | `Option<T>` | |

### `CamlPlonkDomain<Fr>`

| Field       | Type        | Description |
| - | - | - |
| `log_size_of_group` | `ocaml::Int` | |
| `group_gen`         | `Fr`         | Generator point |

### `SRS<G: CommitmentCurve>`

The in-memory verifier index structure contains the verifier key and is defined as follows.

| Field    | Type             | Description |
| - | -    | - |
| `g`      | `Vector<G>`      | For committing polynomials |
| `h`      | `G`              | Blinding factor |
| `endo_r` | `G::ScalarField` | Curve endomorphism coefficient r |
| `endo_q` | `G::BaseField`   | Curve endomorphism coefficient q |


### `CamlPastaFpUrs`

Universal reference string over Pasta Fp curve

```rust
pub type CamlPastaFpUrs = CamlPointer<Rc<SRS<GAffine>>>;
```

### `CamlPlonkVerificationEvals<PolyComm>`

| Field | Type  | Description |
| - | - | - |
| `sigma_comm0` | `PolyComm` | Permutation commitment |
| `sigma_comm1` | `PolyComm` | Permutation commitment |
| `sigma_comm2` | `PolyComm` | Permutation commitment |
| `ql_comm`     | `PolyComm` | Left input wire commitment |
| `qr_comm`     | `PolyComm` | Right input wire commitment |
| `qo_comm`     | `PolyComm` | Output selector poly commitment |
| `qm_comm`     | `PolyComm` | Multiplication commitment |
| `qc_comm`     | `PolyComm` | Constant wire commitment |
| `rcm_comm0`   | `PolyComm` | Round constant polynomial commitment |
| `rcm_comm1`   | `PolyComm` | Round constant polynomial commitment |
| `rcm_comm2`   | `PolyComm` | Round constant polynomial commitment |
| `psm_comm`    | `PolyComm` | Poseidon constraint selector polynomial commitment |
| `add_comm`    | `PolyComm` | EC addition selector polynomial commitment |
| `mul1_comm`   | `PolyComm` | EC variable base scalar multiplication selector polynomial commitment |
| `mul2_comm`   | `PolyComm` | EC variable base scalar multiplication selector polynomial commitment |
| `emul1_comm`  | `PolyComm` | Endoscalar muplication selector polynomial commitment |
| `emul2_comm`  | `PolyComm` | Endoscalar muplication selector polynomial commitment |
| `emul3_comm`  | `PolyComm` | Endoscalar muplication selector polynomial commitment |

### `CamlPlonkVerificationShifts<Fr>`

| Field | Type | Description |
| - | - | - |
| `r`   | `Fr` | Right wires shift |
| `o`   | `Fr` | Output wires shift |

### `CamlPastaFpPlonkVerifierIndex`

Plonk verifier index for Pasta Fp

```rust
pub type CamlPastaFpPlonkVerifierIndex =
    CamlPlonkVerifierIndex<Fp, CamlPastaFpUrs, PolyComm<GAffine>>;
```

| Field           | Type                  | Description |
| - | - | - |
| `domain`        | `CamlPlonkDomain<Fp>` | |
| `max_poly_size` | `ocaml::Int` | |
| `max_quot_size` | `ocaml::Int` | |
| `urs`           | `CamlPastaFpUrs` | |
| `evals`         | `CamlPlonkVerificationEvals<PolyComm<GAffine>>` | |
| `shifts`        | `CamlPlonkVerificationShifts<Fp>` | |
