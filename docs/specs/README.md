# Types and structures

* [Common](types_and_structures/common.md)
* [Serialized key](types_and_structures/serialized_key.md)
* [Serialized SRS](types_and_structures/serialized_srs.md)
* [Block structure](types_and_structures/block.md)
* [Consensus](consensus/README.md)

# Loading the URS

**Inputs:**
* `path` to the serialized URS file
* `offset` of binary URS within file

**Outputs**
* `TODO`

**Spec**

Load with [`bin_prot`](https://github.com/janestreet/bin_prot)

# Loading the verifier keys

**Inputs**
* `path: String` to serialized verifier key file
* `URS: CamlPastaFpUrs` loaded

**Outputs**
* `TODO`

**Spec**

1. Open the serialized key `path`
2. Read and parse the `header` (see the [serialized key format](types_and_structures/serialized_key.md))
3. Compute the offset of the `body`
4. Validate the `header` against the metadata provided by O(1) Labs in the verifier key filename
    1. `header.kind.type`
    2. `header.kind.identifier`
    3. `header.constraint_system_hash`
5. Load the `key` located at `path, offset` using [`bin_prot`](https://github.com/janestreet/bin_prot)
6. Validate the `header` against the loaded key contents
    1. `constraint_constants`
    2. `constraint_system_hash`
7. Create the `VerifierIndex` structure containing the `URS` and `key`
