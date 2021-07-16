# Coda Bazelization Project

WARNING: this document is outdated by a few weeks.  Check back in a few days.

# Table of Contents
* [Goals and Non-goals](#goals)
* [Tasks](#tasks)
  * [C/C++](#tasks_c)
  * [OCaml](#tasks_ocaml)
  * [Go](#tasks_go)
  * [Rust](#tasks_rust)
  * [Javascript](#tasks_js)
  * [Integration](#tasks_integration)
* [Status](#status)
* [Schedule](#schedule)
* [Working with Bazel](#workingwithbazel)
* [Resources](#resources)

## Goals and Non-goals <a name="goals"></a>

Mandatory Goals:

* Complete Bazel coverage of the Coda codebase:
  * OCaml code
  * C/C++ code and dependencies.
  * Golang dependencies ([go-libp2p](https://github.com/libp2p/go-libp2p))
  * Rust dependencies ([marlin](https://github.com/scipr-lab/marlin)?)
  * Reason/Javascript
* "Just works" on:
  * Linux
  * MacOS

Non-mandatory goals:

* Cross-platform builds targeting Linux, x86_64
  * In particular, MacOS -> Linux
* Dockerfile to support Linux development of Coda on Mac host

Non-goals:

* Windows support.  Maybe later.
* Cross-platform build targeting ARM.
* Production-quality `rules_ocaml` Bazel library.  The goal is
  implement just enough functionality to build Coda.  Supporting a
  production-quality general-purpose library of OCaml rules for Bazel
  is out of scope.

## Tasks <a name="tasks"></a>

General:

* Customization/Optimiztion configurations.
  * User-friendly support for settings e.g. debug/optimized build, etc.
  * Support for dependency configuration.  For example, libsnark
    supports various curves (bn128, etc.), building with or without
    procps support, etc.

### C/C++ Tasks <a name="tasks_c"></a>

#### Quick Links:
* [Developer readme](README-dev.md)
* [Compiling from source and running a node](docs/demo.md)
* [Directory structure](frontend/website/docs/developers/directory-structure.md)
* [Lifecycle of a payment](frontend/website/docs/architecture/lifecycle-payment.md)

* Take inventory of all C/C++ dependencies, both internal and external.
* Write Bazel code for all C/C++ dependencies.
  * For "native" builds: build host == execution host == target host
  * Cross-platform builds: build/exec host == Linux, MacOS; target host == Linux x86_64

[libsnark](https://github.com/scipr-lab/libsnark) gets special treatment, since it is central to Coda.
Coda vendors this library - embeds a copy of it within the Coda
codebase
([src/lib/snarky/src/camlsnark_c/libsnark-caml/libsnark](src/lib/snarky/src/camlsnark_c/libsnark-caml/libsnark)),
with some local modifications.

Several strategies for Bazelization of libsnark are available:

* Add direct (embedded) Bazel support to libsnark and all or some of its dependencies; this entails one of:
  * Convince the upstream maintainers to incorporate Bazel support; or
  * Maintain the Bazelized forks in the Coda repo; or
  * Incorporate the Bazel code into the vendored image of libsnark within Coda.
* Import libsnark as an external repo and write a local BUILD file for it.

This implies a choice:

* Retain the current vendored image and add Bazel support to it.
* Treat libsnark as an external repo, and patch/augment it for Coda purposes
  (instead of vendoring the code)

I've chosen to fork and Bazelize libsnark and all of its dependencies
(see details in [Status](#status)).  The resulting Bazel code can be
adapted to the needs of the Coda project.

### OCaml Tasks <a name="tasks_ocaml"></a>

### Go Tasks <a name="tasks_go"></a>

### Rust <a name="tasks_rust"></a>

### Reason/Javascript <a name="tasks_js"></a>

### Integration <a name="tasks_integration"></a>



## Status <a name="status"></a>

As of Fri 6/12/2020

### C/C++ Status

* Manifest of C/C++ deps: see WORKSPACE file.
* The following libs have been Bazelized:
  * boost
  * libffi
  * libgmp
  * gtest
  * libjemalloc
  * libomp (OpenMP)
  * libre2
  * libsnark and its dependencies (details below)
  * libsodium
  * libssl, libcrypto (OpenSSL)
  * zlib

Remaining: rocksdb

See WORKSPACE and BUILD.bazel for details.

#### libsnark

libsnark and its dependencies have been forked and Bazilized:

* [libsnark](https://github.com/mobileink/libsnark)
* [libfqfft](https://github.com/mobileink/libfqfft)
* [libff](https://github.com/mobileink/libff)
* [ate-pairing](https://github.com/mobileink/ate-pairing)
* [xbyak](https://github.com/mobileink/xbyak)

#### C/C++ Issues

* libpq-dev (Postgresql)
* libprocps-dev
* libsnark - several targets fail to compile, with an error that does
  not appear to be related to the build logic.  Possible causes:
  versioning issues; MacOS eccentricities; compile/link flags. Or
  maybe those failures are expected.  To be investigated.

### OCaml Status

In progress; see notes below in Schedule section.

General Bazel support for OCaml is under development at
https://github.com/mobileink/obazl.  For notes on Bazel support for
Coda, see [Building Coda with Bazel](docs/building/bazel.md).

### Go Status

First cut done.  See build targets in `src/app/libp2p_helper`

### Rust Status

First cut done, but not yet integrated into this repo.  See https://github.com/mobileink/marlin and https://github.com/mobileink/zexe

### Reason/JS Status
Nothing to report.

## Schedule <a name="schedule"></a>

Today (5/20/2020) we are in week 21 of the year 2020.

### Week 21, ending 5/24/2020 (Sunday)

#### Tasks

* Documentation: Terminology; Principles and Policies, Getting Started, etc.

#### Deliverables

* Documentation: [Building Coda with Bazel](docs/building/bazel.md) (markdown)
* C/C++:  Bazel code for all dependencies added to [Bazel fork](https://github.com/mobileink/coda)
* Go:  Bazel code to build [go-libp2p](https://github.com/libp2p/go-libp2p)
* Rust: Bazel code to build [marlin](https://github.com/scipr-lab/marlin)

### Week 22, ending 5/31/2020

#### Tasks

* Migrate stable Bazel code from dev fork into master
* Integration support: C/C++, Go, Rust, JS
* Configuration support - expose parameters for setting all dependency
  options.
* Linux dev container running on Mac.

#### Deliverables

* Git PRs for migrating Bazel code into master
* Integrated build Proof-of-Concept
  * Minimal working example involving:
    * OCaml driver
    * C/C++ dependency (e.g. libsnark, openssl, etc.)
    * go-libp2p dependency
    * marlin dependency (rust)
    * At least one Reason/JS dependency
* Bazel build_settings for configurations.
* Dockerfile - Linux image capable of building Coda on Mac

#### Actuals

* Finished first-cut versions of support for C/C++ deps, go-libp2p
  (golang), and two rust deps (marlin and zexe).

### Week 23, ending 6/7/2020

#### Tasks

* Update `rules_ocaml` - incorporate lessons learned from rules_go, rules_rust
* Complete Bazel support for interop/integrated builds within Coda
  * snarky/libsnark
  * go-libp2p
  * etc.
* Bazel support for OCaml code in Coda

#### Deliverables

* rules_ocaml files
* Bazel files supporting integrated build
* BUILD.bazel files for pure OCaml targets

#### Actuals

* All work focussed on Bazel Ocaml support - [OBazl](https://github.com/mobileink/obazl).
* Started with [rules_ocaml](https://github.com/jin/rules_ocaml), but it turned out to be very minimal.  Switched to [rules_go](https://github.com/bazelbuild/rules_go) as a model.
* Implemented basic bootstrapping functionality based on local
  toolchain - the repository rules necessary to setup the SDK etc. for
  use by buld rules.
* Started work on OCaml build rules modeled on Dune.

### Week 24, ending 6/14/2020

* Complete migration of all Bazel code into master.
* Finish documentation

#### Actuals

* Lots of experimenting with both Bazel and the OCaml tooling ecosystem.
  * Most of the last two weeks has been devoted to deciphering the
    rather mysterious OCaml build process and integrating it with
    Bazel's dependency management structure (targets and actions).
* Major achievments:
  * Partial support for PPX libraries and executables
    * Still some kinks to be worked out, e.g. a successful build does
      not guarantee correct functionality, PPX logic may not be
      initialized/registered correctly.
  * Support for "redirector" modules, i.e. "wrappers" that use module
    aliases to package files into a module.  Dune does this
    implicitly, by generating a file and renaming submodule files
    behind the scenes.  With Bazel, every step is explicit. A generic
    rule that automates the process can be added later.
  * Config support - generation of config.mlh is vastly simpler with Bazel

### Week 25, ending 6/21/2020

#### Tasks

* Finish support for building and using PPXes.
* Add support for fine-grained user-controlled config settings as
  needed, e.g. verbosity levels, optimization flags, etc.
* Interop: Ocaml + Go, Rust, C/C++
* Make substantial progress adding Bazel support to Coda
  * Coda contains on the order of 300 dune files.
  * Plan is to convert a sample of these by hand and use the knowledge
    gained to design an automated process for the rest.

#### Deliverables

* OBazl rules for generating:
  * ppx libs and pipelines
  * application libs with and without preprocessing
  * executables
  * tests
* A sample of a few dozen BUILD.bazel files, selected so as to cover
  the most common cases.

### Week 26, ending 6/28/2020

#### Tasks

* Finish OBazl rules
* Finish adding Bazel support for OCaml code
* Complete integrated build system

#### Deliverables

* One-click builds!

## Working with Bazel <a name="workingwithbazel"></a>

Print list of targets:

```
$ bazel query 'attr(visibility, "//visibility:public", //...:all)'
```

Build a target:

```
$ bazel build //:libsodium
## equivalently:
$ bazel build libsodium
```

Some but not all of the third-party libs have test targets, in order
to demonstrate a successful build. The tests are in `bzl/test`.  To
run a test:

```
$ bazel run //bzl/test/libsodium
```

## C/C++

The current build system lists a variety of external dependencies.
Some of these represent tools used by the build process (e.g. cmake,
gpatch); others are code libraries (e.g. libsodium, openssl).

The WORKSPACE file defines repos for all the deps that look to me like
code deps.  Tool deps are outside the scope of a Bazel build; users
will just have to install them as usual.

## Resources <a name="resources"></a>

* [Best practices for Bazel](https://docs.bazel.build/versions/master/best-practices.html)
* [External C++ dependency management in Bazel](https://blog.envoyproxy.io/external-c-dependency-management-in-bazel-dd37477422f5)
* [Awesome Bazel](https://awesomebazel.com/). Curated list of resources.
* [OCaml rules for Bazel](https://github.com/jin/rules_ocaml). "Very experimental".
* [Go rules for Bazel](https://github.com/bazelbuild/rules_go). Officially supported.
* [Rust rules for Bazel](https://github.com/bazelbuild/rules_rust). Officially supported?
