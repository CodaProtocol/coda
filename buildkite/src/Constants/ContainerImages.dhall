-- TODO: Automatically push, tag, and update images #4862

{
  toolchainBase = "codaprotocol/ci-toolchain-base:v3",
  minaToolchainStretch = "gcr.io/o1labs-192920/mina-toolchain@sha256:a9e391a9c771254b7ff87d9266d55882cea2d7ba59febb5d7b2d686a1062ccce",
  minaToolchainBuster = "gcr.io/o1labs-192920/mina-toolchain@sha256:8cb1d22ea84425c065887d31ddcc68c20d26acf717ceb7acc8425701f3b2a37c",
  elixirToolchain = "elixir:1.10-alpine",
  rustToolchain = "codaprotocol/coda:toolchain-rust-e855336d087a679f76f2dd2bbdc3fdfea9303be3",
  nodeToolchain = "node:14.13.1-stretch-slim",
  ubuntu1804 = "ubuntu:18.04"
}
