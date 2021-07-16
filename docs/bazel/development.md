# development

* To get more verbose output use the flags documented in `.bazelrc`.
* For convenience, use a `user.bazelrc` file (but do not place it
  under version control).  Enable the following (which you can copy
  from the `.bazelrc` file):
  * build --color=yes
  * build --subcommands    # prints build cmd with space-separate args
  * build --subcommands=pretty_print  # same, but args are newline-separated
  * build --verbose_failures
  * build --sandbox_debug
* For more details on `rc` files, see [.bazelrc, the Bazel configuration file](https://docs.bazel.build/versions/master/guide.html#bazelrc-the-bazel-configuration-file).  See also [Best Practices - .bazelrc](https://docs.bazel.build/versions/master/best-practices.html#bazelrc)
* Bazel uses a bunch of hidden files and settings. To list them run `$ bazel info`.
  * You can also pass one of the keys as parameter, e.g. `$ bazel info
    output_base`.  You can use this in shell scripts, for example, to
    make your own convenience tools.
  * External repos populate `$(bazel info output_base)/external`.  But
    note that it will only be populated when you buld a target that
    depends on the external repo.  So if you want to e.g. see what the
    `@digestif` repo looks like to Bazel, first build one of its
    targets and then do something like `$ ls $(bazel info output_base)/external/digestif`.
  * See [Output directory
    layout](https://docs.bazel.build/versions/master/output_directories.html)
    for more details.
* Bazel creates softlinks in your root repo dir pointing to some of
  its hidden directories:
  * `bazel-bin`
  * `bazel-out`
  * `bazel-testlogs`
  * `bazel-<root>` - for example, if your base WORKSPACE files is in dir foo, this will be `bazel-foo`

  If you find having these directories in the top level inconvenient,
  you can tell Bazel to put them somewhere else by passing `--symlink_prefix`.  A common practice is to put the following line in your `user.bazelrc` file, in order to put them in a hidden dot-dir:
  * build --symlink_prefix=.bazel/
* Bazel keeps a log of each command in `bazel info command_log`, so if
  you miss the output you can find it there.  But note that each
  command overwrites it - including the `info` command.  So if you run
  a command and then use `bazel info command_log` to find the log, it
  will contain the result of running `bazel info command_log`. Ha ha ha!
  So you need to save the name of the log somewhere.
* If you are working on an external repository, you can tell Bazel to
  use your local copy instead of the remote repo listed in WORKSPACE.  For example:
  * `build --override_repository=digestif=/path/to/local/digestif`

  This way you need not edit WORKSPACE to switch repos.
* Note that entries in `user.bazelrc` only apply to the subcommand,
  which is listed first. For example, `build --foo` will apply the
  `--foo` arg, but only when `bazel build` is the command.  If you
  also want it to apply for queries, you must add the analogous line:
  `query --foo`.

* If you use `--override_repository`, you need to run `baze clean
--expunge` for it to take effect(?)
