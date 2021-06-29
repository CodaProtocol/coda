# Base58Check

For many values in the protocol that users see, we wish to present
them in a way that is human-readable. The values may be represented in
computer code as arbitrary bit sequences. We can encode values as
strings in base 58 over some alphabet, and decode those strings back
to values in the internal representation.

## Base 58 alphabet

The base 58 alphabet we use is the same as Bitcoin uses for base 58
encodings of addresses, consisting of

  - the numerals 1 to 9
  - the ASCII uppercase letters A to Z, omitting I and O, and
  - the ASCII lowercase letters, omitting l

## Prefix and suffix

To assure that base 58 strings represent bonafide values, we use the
Base58Check algorithm, which adds a prefix and suffix to the value
before converting the result to base 58.

The prefix is a single byte, which is distinct for each type that we
encode. When decoding a Base58 string to a value of a type, the prefix
is checked against the expected byte. It's an error if the prefix
differs from what's expected.  The OCaml code raise an exception in
that case. The check yields a measure of run-time type safety.

The added suffix is a derived from double-hashing a payload using the
SHA256 algorithm. The suffix is the first four bytes of the
double-hash. The checksum is checked when decoding a Base58Check value
to assure the integrity of the encoded string. The OCaml code raises
an exception if the checksum at the end the decoded string does not
match the checksum of the decoded payload.

## Creating encoders and decoders for a type

In this library, the `Version_bytes` module contains bindings for the
types that are Base58Check-encoded. The functor `Base58_check.Make`
takes a structure containing a version byte for a type (and a textual
description of the type), and returns a structure with an encoder and
decoders. One decoder raise exceptions in the case of an errors, the
other returns an `Or_error.t`

Encoding a value to Base58Check requires that the value of a type
first be encoded as an OCaml string. The decoders returned by
`Base58_check.Make` return a string, so that string needs to be
converted to the value of the type.

While a value can be converted to and from a string in whatever way,
if the type is `Bin_prot`-serializable, there's a convenient means to
obtain Base58Check encoders and decoders. The `Codable` library
contains the functor `Make_base58_check`, which accepts a structure
with such a type, a version byte, and a textual description, and
returns an encoder and decoders. The encoder takes a value of the type
(not a string), and the decoders return a value of the type (not a
string).
