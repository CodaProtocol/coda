# Mina

Mina is a new cryptocurrency protocol with a lightweight, constant-sized blockchain.

- [Developer homepage](https://codaprotocol.com/code.html)
- [Roadmap](https://codaprotocol.com/docs/developers)
- [Repository Readme](README.md)

If you haven't seen it yet, [CONTRIBUTING.md](CONTRIBUTING.md) has information
about our development process and how to contribute. If you just want to build
Mina, this is the right file!

## Building Mina

Building Mina can be slightly involved. There are many C library dependencies that need
to be present in the system, as well as some OCaml-specific setup.

Currently, Mina builds/runs on Linux & macOS. MacOS may have some issues that you can track [here](https://github.com/MinaProtocol/coda/issues/962).

The short version:

1.  Start with Ubuntu 18 or run it in a [virtual machine](https://www.osboxes.org/ubuntu/)
2.  Set github repos to pull and push over ssh: `git config --global url.ssh://git@github.com/.insteadOf https://github.com/`
    - To push branches to repos in the MinaProtocol or o1-labs organisations, you must complete this step. These repositories do not accept the password authentication used by the https URLs.
3.  Pull in our submodules: `git submodule update --init`
    - This might fail with `git@github.com: Permission denied (publickey).`. If that happens it means
      you need to [set up SSH keys on your machine](https://help.github.com/en/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent).
4.  Run `git config --local --add submodule.recurse true`

### Developer Setup (MacOS)

- Invoke `make macos-setup`
  - If this is your first time using OCaml, be sure to run `eval $(opam config env)`
- Invoke `rustup default 1.45.2`
- Invoke `make build`
- Jump to [customizing your editor for autocomplete](#dev-env)

### Developer Setup (Linux)

#### Install or have Ubuntu 18

- [VM Images](https://www.osboxes.org/ubuntu/)

#### Building

Mina has a variety of opam and system dependencies.

You can see [`Dockerfile-toolchain`](/dockerfiles/Dockerfile-toolchain) for how we
install them all in the container. To get all the opam dependencies
you need, you run `opam switch import src/opam.export`.

Some of our dependencies aren't taken from `opam`, and aren't integrated
with `dune`, so you need to add them manually, by running `scripts/pin-external-packages.sh`.

There are a variety of C libraries we expect to be available in the system.
These are also listed in the dockerfiles. Unlike most of the C libraries,
which are installed using `apt` in the dockerfiles, the libraries for RocksDB are
automatically installed when building Mina via a `dune` rule in the library
ocaml-rocksdb.

#### Setup Docker CE on Ubuntu

- [Ubuntu Setup Instructions](https://docs.docker.com/install/linux/docker-ce/ubuntu/)

#### Customizing your dev environment for autocomplete/merlin

- If you use vim, add this snippet in your vimrc to use merlin. (REMEMBER to change the HOME directory to match yours)

```bash
let s:ocamlmerlin="/Users/USERNAME/.opam/4.07/share/merlin"
execute "set rtp+=".s:ocamlmerlin."/vim"
execute "set rtp+=".s:ocamlmerlin."/vimbufsync"
let g:syntastic_ocaml_checkers=['merlin']
```

- In your home directory `opam init`
- In this shell, `eval $(opam config env)`
- Now `/usr/bin/opam install merlin ocp-indent core async ppx_jane ppx_deriving` (everything we depend on, that you want autocompletes for) for doc reasons
- Make sure you have `au FileType ocaml set omnifunc=merlin#Complete` in your vimrc
- Install an auto-completer (such as YouCompleteMe) and a syntastic (such syntastic or ALE)
- If you use vscode, you might like these extensions

  - [OCaml and Reason IDE](https://marketplace.visualstudio.com/items?itemName=freebroccolo.reasonml)
  - [Dune](https://marketplace.visualstudio.com/items?itemName=maelvalais.dune)

- If you use emacs, besides the `opam` packages mentioned above, also install `tuareg`, and add the following to your .emacs file:

```lisp
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (load "tuareg-site-file")
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)))
```

Emacs has a built-in autocomplete, via `M-x completion-at-point`, or simply `M-tab`. There are other
Emacs autocompletion packages; see [Emacs from scratch](https://github.com/ocaml/merlin/wiki/emacs-from-scratch).

## Using the makefile

The makefile contains phony targets for all the common tasks that need to be done.
It also knows how to use Docker automatically. 

These are the most important `make` targets:

- `build`: build everything
- `docker`: build the container
- `container`: restart the development container (or start it if it's not yet)
- `dev`: does `docker`, `container`, and `build`
- `test`: run the tests
- `libp2p_helper`: build the libp2p helper
- `web`: build the website, including the state explorer

We use the [dune](https://github.com/ocaml/dune/) buildsystem for our OCaml code.

NOTE: all of the `test-*` targets (including `test-all`) won't run in the container.
`test` wraps them in the container.

## Steps for adding a new dependency

Rarely, you may edit one of our forked opam-pinned packages, or add a new system
dependency (like libsodium). Some of the pinned packages are git submodules,
others inhabit the git Mina repository.

If an existing pinned package is updated, either in the Mina repository or in the
the submodule's repository, it will be automatically re-pinned in CI.

If you add a new package in the Mina repository or as a submodule, you must do all of the following:

1. Update [`Dockerfile-toolchain`](/dockerfiles/Dockerfile-toolchain) as required; there are
   comments that distinguish the treatment of submodules from other packages
2. Update [`scripts/macos-setup.sh`](scripts/macos-setup.sh) with the required commands for Darwin systems
3. Bust the circle-ci Darwin cache by incrementing the version number in the cache keys as required inside [`.circleci/config.yml.jinja`](.circleci/config.yml.jinja)
4. Commit your changes
5. Rebuild the container with `make docker-toolchain`.
6. Re-render the jinja template `make update-deps`
7. Commit your changes again

Rebuilding the docker toolchain will take a long time. Running circleci for
macos once you've busted the cache will also take a long time. However, only
you have to do the waiting and all other developers will get the fast path.

The automatic re-pinning of modified packages does take some CI time, so eventually,
you'll want to rebuild the Docker toolchain to save that time.

## Common dune tasks

To run unit tests for a single library, do `dune runtest lib/$LIBNAME`.

You can use `dune exec coda` to build and run `coda`. This is especially useful
in the form of `dune exec coda -- integration-tests $SOME_TEST`.

You might see a build error like this:

```text
Error: Files src/lib/mina_base/mina_base.objs/account.cmx
       and src/lib/mina_base/mina_base.objs/token_id.cmx
       make inconsistent assumptions over implementation Crypto_params
```

You can work around it with `rm -r src/_build/default/src/$OFFENDING_PATH` and a rebuild.
Here, the offending path is `src/lib/mina_base/mina_base.objs`.

## Docker Image Family Tree

Container Stages:

- Stage 0: Initial Image [ocaml/opam2:debian-9-ocaml-4.07](https://hub.docker.com/r/ocaml/opam2/) (opam community image, ~880MB)
- Stage 1: [coda toolchain](https://github.com/MinaProtocol/coda/blob/master/dockerfiles/Dockerfile-toolchain) (built by us, stored on docker hub, ~2GB compressed)
- Stage 2: [codabuilder](https://github.com/MinaProtocol/coda/blob/master/dockerfiles/Dockerfile) (built with `make codabuilder`, used with `make build`, ~2GB compressed)

## Overriding Genesis Constants

Mina genesis constants consists of constants for the consensus algorithm, sizes for various data structures like transaction pool, scan state, ledger etc.
All the constants can be set at compile-time. A subset of the compile-time constants can be overriden when generating the genesis state using `runtime_genesis_ledger.exe`, and a subset of those can again be overridden at runtime by passing the new values to the daemon.

The constants at compile-time are set for different configurations using optional compilation. This is how integration tests/builds with multiple configurations are run.
Currently some of these constants (defined [here](src/lib/mina_compile_config/mina_compile_config.ml)) cannot be changed after building and would require creating a new build profile (\*.mlh files) for any change in the values.

<b> 1. Constants that can be overridden when generating the genesis state are:</b>

- k (consensus constant)
- delta (consensus constant)
- genesis_state_timestamp
- transaction pool max size

To override the above listed constants, pass a json file to `runtime_genesis_ledger.exe` with the format:

```json
{
  "k": 10,
  "delta": 3,
  "txpool_max_size": 3000,
  "genesis_state_timestamp": "2020-04-20 11:00:00-07:00"
}
```

The exe will then package the overriden constants along with the genesis ledger and the genesis proof for the daemon to consume.

<b> 2. Constants that can be overriden at runtime are:</b>

- genesis_state_timestamp
- transaction pool max size

To do this, pass a json file to the daemon using the flag `genesis-constants` with the format:

```json
{
  "txpool_max_size": 3000,
  "genesis_state_timestamp": "2020-04-20 11:00:00-07:00"
}
```

The daemon logs should reflect these changes. Also, `mina client status` displays some of the constants.
