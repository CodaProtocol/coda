# Building with Bazel

**ToC**
* [Prerequisites](#prerequisites)
* [Getting Started](#getting-started)
* [Configuration](#configuration)
* [Development](development.md)
* [Status](status.md)

## Prerequisites

The current implementation is not hermetic - it relies on the host
development environment.  For example, it reads the environment
variables OPAMROOT and OPAM_SWITCH_PREFIX; if these are not set you
will get an error message telling you what to do.  The final version will be hermetic, meaning it will automatically install all dependencies.

For now, you must have the development environment for the current
Dune-based build system (system and opam deps) already installed.  See
[`Dockerfile-toolchain`](/dockerfiles/Dockerfile-toolchain). Once you
have opam installed, run `opam switch import src/opam.export` to
install the opam deps needed by Coda.  Exceptions: you do not need to
install Nix or the deps listed in `scripts/pin-external-packages.sh`.

To install Bazel, see [Getting started with
Bazel](https://docs.bazel.build/versions/master/getting-started.html).
Or you may prefer to use
[Baselisk](https://github.com/bazelbuild/bazelisk) ([Installing Bazel
using
Bazelisk](https://docs.bazel.build/versions/master/install-bazelisk.html)).

You can plough into the examples below, but I strongly recommend that
you read [Concepts and
Terminology](https://docs.bazel.build/versions/3.3.0/build-ref.html)
first.  You should also read [Specifying targets to
build](https://docs.bazel.build/versions/master/guide.html#specifying-targets-to-build);
this will help you understand target patterns used below, such as
`@digestif//:*`.

## Getting Started

**NOTE** Bazelization is still in progress, so this section is
intended to help you start using/exploring the use of Bazel with Coda.
Once Bazel builds are complete, a genuine Quickstart for building Coda
will replace this one.

A good way to get started is by using Bazel's [query
facility](https://docs.bazel.build/versions/master/query-how-to.html)
to explore your options.  If you look at the bottom of the WORKSPACE
file you will find a series of 'git_repository' rules in the OCaml
section, for the external repos used by Coda, like `digestif` and
`ppx_version`.  You can use the Query facility to interrogate
these. (NOTE: these repos contain Coda source code; the other repo
rules in WORKSPACE contain libraries used by Coda, so you don't need
to explore them, although you could.)

First let's list all rules (i.e. productions of 'kind' _rule_) in the
`digestif` external repo:

`$ bazel query 'kind(rule, @digestif//:*)' --output label_kind | sort`

Pick a rule, and list its deps:

`$ bazel query 'deps(@digestif//:digestif_bi)' | sort`

Then build it:

`$ bazel build @digestif//:digestif_bi`

Bazel supports a `visibility` attribute, which allows us to scope the
visibility/accessibility of targets.  By default, the visibility of
targets in a package is 'private', which means nothing outside of the
package may depend on them.  Targets that we want to expose so that
other packages/targets may depend on them are marked 'public' (or
exposed by another of the methods describe in
[Visibility](https://docs.bazel.build/versions/master/visibility.html)). Our
first query above listed _all_ rules in `@digestif`; now let's list
only the rules marked with public visibility:

`$ bazel query 'attr(visibility, "//visibility:public", @digestif//:*)' --output label_kind | sort`

WARNING: in this case, the suffixes '_batch' and '_composite' refer to
different build strategies that need not concern us at the moment.
See the [BUILD.bazel file for
digestif](https://github.com/mobileink/digestif/blob/bazel/BUILD.bazel)
if you want more information.

We can list just the binary rules - such rules produce executables:

`$ bazel query 'kind(ocaml_binary, @digestif//:*)' --output label_kind | sort`

We can also use regex syntax, which would find both `ocaml_binary` and
`ppx_binary` rules:

`$ bazel query 'kind(.*_binary, @digestif//:*)' --output label_kind | sort`

Note the use of .\* , not merely \* .  We can build such rules, as
above, but we can also run them:

`$ bazel run @digestif//:c_test`

**TESTS**

In the `@digestif` repo, tests are implemented using `ocaml_binary`
rules.  Bazel supports a special kind of rule for such tests, but it
is not yet implemented in this case.  An example may be found in the
`@ppx_version` repo.  Target `@ppx_version//:ppx_version_sh_test` uses
[sh_test](https://docs.bazel.build/versions/master/be/shell.html#sh_test)
to run a shell script that runs tests.  Use `test` to run it:

`$ bazel test @ppx_version//:ppx_version_sh_test`

The output will indicate Pass/Fail.  Anything written to stdout/stderr
will be redirected to a log file; you can find out where it is by
running `bazel info bazel-testlogs` or by passing flag `--subcommands`
to the test command and finding `test.log` in the verbose output.

## Configuration

Many Coda modules depend on `config.mlh`, which contains a set of
`%%define` declarations for preprocessing by `ppx_optcomp`.  In the
dune-based build system, this file is generated and includes a set of
additional `.mlh` files, depending on the selected configuration.  The
entire system is contained in `src/config`.

The Bazel build replaces this with a collection of build settings
whose values are used to instantiate a [config file
template](../../ocaml/config/config.mlh.tpl), producing `config.mlh`.

Build settings may be set individually at the command line (or
equivalently using `.bazelrc`).  Configurations of build settings may
be expressed as _profiles_ analogous to those used for the dune-based
system.

For example, to generate the config file `config.mlh` using the
default profile ('dev'): `$ bazel build ocaml/config`.

To use the debug profile: `$ bazel build ocaml/config --//ocaml/profile=debug`.

To see a list of supported profiles, just pass an invalid profile
value, e.g. 'profile=xxxx'. Bazel will throw an error and tell you
what the allowed values are.  Currently there are only a few.
Defining new ones is trivial; see the `coda_config` rules in
[ocaml/config/BUILD.bazel](../../ocaml/config/BUILD.bazel) for
exampls.

If you examine the output (in `.bazel/bin/ocaml/config/config.mlh`)
you will find the generated file includes a comment indicating which
profile was used to generate it, plus one line for each `%%define`
directive. It does not `%%include` any files.

Build settings are expressed as rules in BUILD.bazel files.  These
files are organized in the file system in order to support a
user-friendly namespaced hierarchy of config setting targets. To see a
list of the packages in the hierarchy:

`$ bazel query 'deps(//ocaml/config/...)' --output package`

Remember that packages are determined by BUILD.bazel files, so each
directory in the list produced by this query will contain such a file.

Let's focus on consensus settings.  List the rules defined by the
consensus package and everything under it:

`$ bazel query 'kind(rule, //ocaml/config/consensus/...)' --output label_kind | sort`

Settings defined using '\_flag' rules may be set on the command line,
those defined using '\_setting' rules may not. The latter are thus
used to fix settings for named profiles.  E.g. if you use the
'fake_hash' profile, then the `consensus_k` setting will be set to
`//ocaml/config/tiny:k1`, which is an `int_setting` that sets k
to 6. Since it is an `int_setting` flag, it cannot be overriden by the
command line.

If settings are not fixed in this way, then they can be set on the
command line.  The default 'dev' profile allows all config settings to
be controlled in this way.  For example, here is how to set
`consensus_k` to 999:

`$ bazel build ocaml/config --//ocaml/config/consensus:k=999`

(Recall that here the colon ':' means that 'k' is a target in the
package (=BUILD.bazel file) at `ocaml/config/consensus`.)

If you try to do this with a profile that fixes consensus_k, Bazel
will just ignore the argument:

`$ bazel build ocaml/config --//ocaml/profile=fake_hash --//ocaml/config/consensus:k=999`

The result will include `[%%define k 6]`, which is the value of
`//ocaml/config/consensus/tiny:k` (a _setting, not a _flag), which is
set by the `coda_config` rule for `fake_hash`, which says:

`...  consensus_k = "//ocaml/config/consensus/tiny:k", ...`

and that `consensus_k` is used to populate the template used to
generate `config.mlh`.  Notice the difference between this
`//ocaml/config/consensus/tiny:k` and the label we used above,
`//ocaml/config/consensus:k=999`.  There are two BUILD.bazel files
involved. In `...consensus/tiny/BUILD.bazel`, 'k' is a target defined
as an `int_setting` with value `6`.  In `...consensus/BUILD.bazel`,
'k' is a target defined as an `int_flag`, with a default value of
`24`.  The latter may be overridden (assigned to), the former may not.
So the `tiny` package contains `int_setting` targets suitable for
fixing setting values; the `consensus` package contains `int_flag`
targets that set default values but may be overriden.

By the way, you don't have to memorize the syntax, you can just add
lines like `build --//ocaml/config/consensus:k=999` to your
`user.bazelrc` file.  Then you don't have to type it into the command
line.


