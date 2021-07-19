# Mina and OCaml

## Summary
[summary]: #summary

This is a set of recommendations for Mina to improve the codebase in regards to its usage of OCaml.

## Motivation
[motivation]: #motivation

Security and developer efficiency.

## Detailed design
[detailed-design]: #detailed-design

### Bump OCaml version

Mina is built using an old version of OCaml: 4.07.1, released in 2018-10-04. The most recent version of OCaml is 4.12.0, released in 2021-02-24. This means that we're not benefiting from improvements to the compiler, new features, and worse: bug fixes. 

As I am writing this, it seems like both Matthew and psteckler are working on this issue, so my comment ends here and I am looking forward to the update! psteckler observes that 4.12.0 is out-of-reach, due to package constraints. Version 4.11.2 looks promising.

### Local opam switch and custom opam repository

To build or contribute to Mina, the guidance and the [provided setup scripts]( https://github.com/MinaProtocol/mina/blob/develop/scripts/setup-opam.sh) are to use a **global opam switch** and the **custom [O(1) Labs opam repository](https://github.com/o1-labs/opam-repository)**.

> An opam switch consists of an OCaml compiler as well as relevant binaries and libraries. By maintaining different switches, you can work on projects that require different versions of the OCaml compiler and OCaml dependencies.

Using a global switch means that the whole developer's system, and in turn all of their OCaml projects, are defaulting to use 4.07.1 (unless the developer remembers to use `opam switch`). In addition, opam packages are potentially shadowed by the ones offered by the O(1) Labs' custom opam repository (as `opam install` will first search there first, and only if the package is not found it will then look in the official opam repo).

There's a simple solution to this issue: use an **local opam switch**. This will create a local `_opam` folder under your clone of the Mina repo, and opam will know to use the binaries and libraries stored there instead of your global `~/.opam`.
We can also easily set the custom O(1) Labs opam repository to only exist in this local switch.

There's a work-in-progress PR for this change: https://github.com/MinaProtocol/mina/pull/8686

### Version constraints

We're currently using an `opam.export` file to ensure that we're using the right versions of needed dependencies. The `opam.export` file is used only once: when you create your opam switch. There are several issues with this file.
First, `opam.export` is a machine-generated file and it is probably not a good idea to modify it by hand. (Note that the export file can also contain pinned libraries (usually forks of libraries) but we do not use it for that.) Second, it does not play nice with commands like `opam upgrade` that will happily ignore your version constraints and mess up with your build.

There exist better solutions to constrain versions of dependencies: `package.opam` files. But I am still not sure if `opam upgrade` will play nice with them either. It seems like `opam upgrade` will only play nice if packages are pinned with `opam pin`.

In any case, I argue that we need a solution to explicitly enforce the versions of packages that are to be used when building Mina. Without that it becomes easy to mess up a dev environment, or to debug other people's issues.

### Lock file

Note that we can do better than being explicit about the versions of our dependencies: we can lock the actual code that we use. This is usually done by keeping track of a file (called a lock file) that saves the hash and version of each dependency used. This file is then tracked in the version control and helps for reproducible builds. It also protects us against supply-chain attacks where legitimate libraries are replaced by backdoored libraries (for example, if the opam repository or opam protocol is compromised).

It seems like Opam has lock files since version 2.0: https://opam.ocaml.org/doc/man/opam-lock.html and version 2.1.0 integrates it in the opam CLI.

### Internal libraries

Most of our internal libraries are stored in [src/lib](https://github.com/MinaProtocol/mina/tree/develop/src/lib) which contains quite a large amount of libraries. In addition, most of these libraries do not contain `README.md` files. It can be quite hard to navigate the code (especially as a newcomer) or set [code owners](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/about-code-owners).

I suggest that different libraries could be sorted into categories. For example, all cryptography-related libraries could live in `src/lib/crypto/`, networking code could live in `src/lib/network`, and so on. Furthermore, it would be nice to have README files in all repositories to get an idea of what is what.

For example, this is how tezos sorted their own libraries: https://github.com/tezos/tezos/tree/master/src

In there, you can see that they also store `package.opam` files in their relevant folder. We might want to follow this model to clean the `src/` folder.

Furthermore, the same could be applied to `scripts/`.

### Nonconsensus libraries

I don't have a real suggestion here, but I noticed that the way we handle nonconsensus libraries is confusing and perhaps could be cleaner. I think nonconsensus libraries are doomed to disappear though, so this might not be a big deal.

### Non-OCaml libraries

I notice that we might move to Bazel to build the project. I don't know much about Bazel, and I don't know how using Bazel will impact the way we build with dune but I'd imagine that using dune, or relying on dune as much as possible, is a good idea. It's a good idea because as users of OCaml we benefit from embracing its ecosystem: we benefit from improvements and we can also give back to the community.

I also noticed that some Rust dependencies won't build if the system's global version of rust is not set to a specific version (1.45.2). I suggest enforcing it locally (instead of globally) via this work-in-progress PR: https://github.com/MinaProtocol/mina/pull/8685

There's also support for monorepos that handle different languages in the soon-to-be-coming opam 2.1.0:

> The opam monorepo plugin lets you assemble standalone dune workspaces with your projects and all of their opam dependencies, letting you build it all from scratch using only Dune and OCaml. This satisfies the “monorepo” workflow which is commonly requested by large projects that need all of their dependencies in one place. **It is also being used by projects that need global cross-compilation for all aspects of a codebase (including C stubs in packages)**, such as the MirageOS unikernel framework.

### External libraries

We use a number of external dependencies. These external dependencies live in different places and are found in different ways by the building system. In this section I suggest consolidating how external libraries are imported.

External libraries can be found:

* In the official opam repo. This is the ideal case, as they are well-maintained and we benefit from updates and bug fixes. The usual way to obtain these dependencies is to `opam install` them, or to declare them in `package.opam` files (as discussed previously).
* In the O(1) Labs custom opam repo. This is a bit less ideal, but it makes sense as publishing a new update to the main opam repo is a slow process (a PR needs to be submitted, and then approved).
* Vendored in `src/external` as a git submodule. For example, async_kernel or ppx_optcomp. This is not ideal, as we might miss bug fixes. Git submodules are also confusing :o) I suggest upstreaming changes or publishing the fork to our custom opam repo.
* Vendored in `src/external`. For example, coda_base58, ocaml-rocksdb, ocaml-sodium. I suggest moving these to our custom opam repo as well.
* Vendored in `src/lib`. For example, snarky, marlin. As these dependencies already have their Github repository, I am not sure why we vendor them. Snarky could be published to an opam repo, while a wrapper Rust project can import marlin.

These external libraries are not always used in the same way by dune. Sometimes, dune can be used to find them with little effort, but sometimes it relies on opam. If a library is not published to an opam repository, `opam pin` is typically used. 

Pins are a manual process, which we had to add as part of [setup scripts](https://github.com/MinaProtocol/mina/blob/develop/scripts/setup-opam.sh#L64). The good thing is that pins should play nice with commands like `opam upgrade`, but pins are not enforced by dune and so can be missed.

Rust has `Cargo.toml` files that list all dependencies used by a project or a library, as well as their versions. `Cargo.toml` files can also be used to list "patches", which are the equivalent of `opam pin`. This way, dependency versions as well as pins are explicitly listed and cannot be missed (the build system uses Cargo.toml to install and manage dependencies). I don't think there's an equivalent in OCaml but it could be a good idea to think about one.

### Explicit dependencies

We use dune to build our OCaml libraries. By default, dune gives the library direct access to any transitive dependencies. So if you use library `A`, and library `A` imports library `B`, then you can use library `B` directly. This, even if you do not list library `B` as being part of your dependencies (in the `dune` file).

This is not great, as it makes it harder to understand where dependencies come from. Dune has a opt-in feature called [implicit_transitive_deps](https://dune.readthedocs.io/en/stable/dune-files.html#implicit-transitive-deps) but it has a number of issues that probably won't let us enable it in production today.

See an example: https://github.com/o1-labs/ppx_version/pull/34/files#diff-1fb9d92d4f0b89e8c93cdefc87d39436b213422961c40453cf2fef5f4a195c57R5

### Testing

There are great tools that we don't seem to make use of:

- ~~property testing with [QCheck](https://github1s.com/c-cube/qcheck/)~~. We actually use Quickcheck, not sure which library is better.
- dynamic analysis with [afl-fuzz](https://ocaml.org/manual/afl-fuzz.html)
- static analysis with [semgrep](https://github.com/returntocorp/semgrep).

### Coding Guidelines

I know we have the [Style guide](https://docs.minaprotocol.com/en/developers/style-guide), but it might be good to have additional coding guidelines to avoid bad patterns and bugs.

I am not well-educated about the common bugs that arise in OCaml code, but I've noticed, for example, that there are a lot of `open` in our codebase, which makes the code really hard to follow. I would consider this bad practice and if enough people agree it could be a good idea to write this out somewhere. I've also noticed usage of functions that can panic (sometimes indicated by the `exn` keyword in the function name) but no comments on why the panics can't happen in production.

Another example is: we use ppx_compare instead of ppx_deriving.eq, it could be a good idea to enforce that in coding guidelines.

psteckler also mentioned some "ignore" issues. Perhaps static analysis could catch these, but maybe we should also consider these bad practices?

### Lints

What if we could detect most issues we're aware of in CI? It'd be great to have something like [Clippy](https://github.com/rust-lang/rust-clippy). Perhaps, the best way to have such a thing is to start by compiling a list of lints we would like to have: https://hackmd.io/4z3hceYBR0yI7NefPpSp6g 

### Convention over configuration

It's nice to rely on known conventions when we can, even if these are not always enforced. We could even enforce these in CI.

For example, a dune library could be called the same as the folder containing it. Another example is to rely on the defaults for ocamlformat instead of configuring it, as it eases transition between codebases.

### Enforcing flows

The story about the different branches is not clear to me. It probably would be easier if the default branch is the one people should be writing code to, and if the different merging flows between branches are automated as well. Just some thoughts. At least there should be some documentation as to what branch is what.

### Issues & PRs

There's currently 1.2k issues and 126 PRs. It might be useful to automatically close old PRs and clean up issues... I'm not sure what's a good strategy here.

### Dependabot

Dependabot is currently a free service on Github that will notify you (and even create PRs) when a new version for one of your dependencies is available. I think there's even a service that tells you when there are known security vulnerabilities, prompting you to update. For example, Rust has https://github.com/RustSec/advisory-db, I am not sure if such a thing exists for OCaml but it would be good to investigate.

Note that dependabot does not support OCaml at the moment, but it could be a good investment to extend dependabot to OCaml (and it should not be too complicated, besides the fact that it is written in Ruby).

### Github discussions

Github has a new feature called "Discussion" (https://docs.github.com/en/discussions). It might be a good idea to enable it on the Mina repository.

### Move/Remove unused code

I've noticed a lot of dead or unused code. Are there tools in Ocaml to check dead code? A lot of that code is in libraries so it's not always straightforward to figure out if it's used or not. I'm trying to remove some here for example: https://github.com/o1-labs/marlin/pull/110 
