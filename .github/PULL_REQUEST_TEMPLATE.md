### Welcome 👋

Thank you for contributing to Mina! Please see `CONTRIBUTING.md` if you haven't
yet. In that doc, there are more details around how to start our CI.

If you cannot complete any of the steps below, please ask for help from a core
contributor.

### Incomplete Work

We feel it's important that everyone is comfortable landing incomplete projects
so we don't keep PRs open for too long (especially on develop). To do this we
don't want to forget that something is incomplete, don't want to be blocked on
landing things, and we don't want to land anything that breaks the daemon. We
don't want to forget to test the incomplete things whenever they are completed,
and finally we want to clean up after ourselves: any temporary cruft gets
completely removed before a project is considered done.

To achieve the above, we wish to keep track of incomplete work using a draft of
the release notes. We can share this part of the current draft at anytime with
external contributors. Moreover, we will review this draft during hardforks.

To ship incomplete work, put it behind feature flags -- prefer a runtime
CLI/daemon-config-style flag if possible, and only if necessary fallthrough to
compile time flags. Note that if you put code behind a compile time flag, you
_must_ ensure that CI is building all possible code paths. Don't land something
that doesn't build in CI.

## PLEASE DELETE EVERYTHING ABOVE THIS LINE

---

Explain your changes:

*

Explain how you tested your changes:

*


Checklist:

- [ ] Modified the current draft of release notes with details on what is completed or incomplete within this project
- [ ] Document code purpose, how to use it
  - Mention expected invariants, implicit constraints
- [ ] Tests were added for the new behavior
  - Document test purpose, significance of failures
  - Test names should reflect their purpose
- [ ] All tests pass (CI will check this if you didn't)
- [ ] Serialized types are in stable-versioned modules
- [ ] Does this close issues? List them

* Closes #0000
* Closes #0000
