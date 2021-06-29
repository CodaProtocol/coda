# Dev Environment

To build your changes using docker, you can use the following flow:

- in one window run `make`
- in another one run `make ssh` to have a shell into the container
    + don't forget to run `./scripts/setup-ocaml.sh` to set up opam/ocaml/etc.
    + don't forget to run `eval $(opam config env)` as well
    + `make build` should work after that :)
