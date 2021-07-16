# Contributing to Coda

Thank you for your interest in contributing to Coda 😁. This file outlines
various parts of our process. Coda is still very young, so things might be a
little bumpy while we figure out how to smoothly run the project!

If you haven't seen it yet, the [developer README](README-dev.md) has the
basic setup you need to get up and running to build and edit Coda.

Here's the summary if you want to contribute code:

1. Learn some OCaml. The [Real World OCaml](https://dev.realworldocaml.org/toc.html) book is good. Jane Street also has [some exercises](https://github.com/janestreet/learn-ocaml-workshop).
2. Learn how we use OCaml. We have [a style guide](https://github.com/CodaProtocol/coda/blob/master/docs/style_guide.md) that goes over the important things.
3. Fork and clone the repo, then set up your development environment. See the [developer README](README-dev.md) for details.
4. Find a good first issue. The best issues to start with are those tagged [`easy`](https://github.com/CodaProtocol/coda/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc+label%3Aeasy).
5. Create a branch in your local clone and implement the solution.
6. Push the branch to your GitHub fork and create a pull request.
7. 🙌

## Bug reports

Bug reports should include, at minimal, the `coda -version` output and
a description of the error. See the [bug report
template](.github/ISSUE_TEMPLATES/bug_report.md).

All bugs need to be reproduced before they can be fixed. Anyone can try and
reproduce a bug! If you have trouble reproducing a bug, please comment with what
you tried. If it doesn't reproduce using exactly the steps in the issue report,
please write a comment describing your new steps to reproduce, or what environment
setup you had to do to reproduce.

Maintainers should label bug reports with `bug`, and any other relevant labels.

## Feature requests

We'll consider any feature requests, although the most successful feature
requests usually aren't immediately posted to the issue tracker. The most
successful feature requests start with discussion in the community! See the
[feature request template](.github/ISSUE_TEMPLATES/feature_request.md).

Maintainers should label feature requests with `feature`, and any other relevant
labels.

## Pull Requests

All pull requests go through CircleCI, which makes sure the code doesn't need to
be reformatted, builds Coda in its various configurations, and runs all the
tests.

All pull requests must get _code reviewed_. Anyone can do a code review! Check
out the [code review guide](docs/code_review.md) for what to look for. Just leave
comments on the "Files changed" view.

All pull requests must be approved by at least one member of the "core eng"
team on github.

Maintainers should assign reviewers to pull requests, and tag them with any
relevant labels.

If you are PRing from the main remote, add `ci-build-me` label when you want to run CI. If you are PRing from a fork, ask a core contributor to `!ci-build-me` when you're ready for CI to run. Note: You will need the `!ci-build-me` comment for each and every run of CI.

Once a PR has been reviewed and approved, and all CI tests have passed, tag the PR with the `ready-to-merge` label to trigger Mergify and automatically merge the code.

## Documentation

There are three main pieces of Coda documentation:

1. The [`docs`](docs/) directory, which has prose documentation of various sorts. This
   doesn't exist yet, but it should by the time this repo is made public!
2. The `README.md` files in various directories. These explain the contents of that
   directory at a high level: the purpose of the library, design constraints, anything else
   specific to that directory.
3. Inline code comments. There are very few of these, and we don't currently run ocamldoc
   so they won't be exposed even as we write them. This should change eventually.

Changes to the software should come with changes to the documentation.

## RFCs

The `rfcs` directory contains files documenting major changes to the software,
how we work together, or the protocol. To make an RFC, just copy the
`0000-template.md` to `0000-shortname.md` and fill it out! Then, open a pull
request where the title starts with `[RFC]`. The idea is that we can discuss the
RFC and come to consensus on what to do. Not all RFCs are merged, only the ones
that we agree to implement.

This process isn't final, but in general:

1. RFCs will be open for at least a week for discussion.
2. Once discussion has slowed down or there is consensus, the core eng team
   will initiate a "Final Comment Period". This will include the core eng's
   "merge disposition" (merge/don't merge). The FCP comment should be based
   on information and arguments already present in the RFC thread. At the
   end of the FCP, the core eng team decides to do another FCP, leave the FCP
   state for continued discussion, or resolve the RFC by merging/closing
   (whichever the announced disposition was).

RFCs do not have to come with an implementation, and the RFC author isn't
required to implement the PR.

RFCs generally describe the design of a feature. As the code corresponding to
the RFC changes, the RFC should be updated to reflect the current state of the
code.

This is loosely inspired by the Rust RFC process, Python PEPs, and IETF RFCs.

## Release process

Create a PR to merge master into testnet. Once that is approved by CI and checked in...

```
git checkout testnet
git tag testnet-v0.$WHATEVER_IS_NEXT
git push testnet-v0.$WHATEVER_IS_NEXT
```

Eventually, we will have a "stable" or "mainnet" branch. Once we do, the process will look like:

Create a PR to merge testnet into stable. Once that is approved by CI and checked in...

```
git checkout stable
git tag v1.$WHATEVER_IS_NEXT
git push v1.$WHATEVER_IS_NEXT
```

And then do the above steps for testnet. Don't forget to deploy!

## Issue/PR label guide

We use them, although currently in a somewhat ad-hoc way. Please see
https://github.com/CodaProtocol/coda/labels for the list of labels and their
short descriptions. Any "complicated" labels should be described here.
