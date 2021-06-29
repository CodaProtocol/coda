# Child processes

- OCaml sources:
  - [Child\_processes](child_processes.ml): spawn and monitor child processes
    - [Child\_processes interface](child_processes.mli)
  - [Termination](termination.ml): track termination of child processes
  - [Syslimits](syslimits.ml): query operating system limits

- C sources:
  - [Syslimits C code](caml_syslimits.c): C code to support `Syslimits`

- Shell sources:
  - [Tester](tester.sh): shell script called in `Child_processes` unit tests

The Mina daemon spawns some child processes to do its work: a libp2p
helper, a prover, and a verifier. In the module `Termination`, there's
a table that maps processes to a `process_kind`, one of `Prover`,
`Verifier`, or `Libp2p_helper`, and a flag `termination_expected`,
indicating whether termination of the process is expected during the
lifetime of the daemon.  While restarts of the verifier and libp2p
helper are expected, the prover should stay running while the daemon
runs.

Periodically, the daemon checks whether these children are still
running.  If termination is not expected, there's an error log, and
the check raises an exception, which terminates the daemon itself.

In `Termination`, the function `get_signal_cause_opt`, given a signal,
returns a string describing the likely cause of a terminated process,
for some signals. The function `wait_for_process_log_errors` calls
`Process.wait` on a process in an `Async` thread, and logs errors that
may occur. Because we wish those logs to indicate where where the
waited-on process is started, it takes a module and location as
arguments.

The `Child_processes` module offers the function `start_custom`, to
start child processes with facilities for handling output and
termination in flexible ways. That function is used to start the
libp2p helper, for instances. (The prover and verifier use the
process-creation facility from a fork of the Jane Street
`Rpc_parallel` library.)

## Flags to `start_custom`

There are a number of flags to `start_custom` to control how processes
are managed.

### Output handling flags

The flags `~stdout` and `~stderr` specify how output should be directed. Both flags take a triple:

- whether to issue process output as logs at a specific level, or not
- whether to make output available via a pipe, or not
- whether empty lines should be filtered from the pipe output

The created pipes are returned in the record returned from `start_custom`.

### Termination flag

The `~termination` flag indicates how termination should be handled. The choices
for the flag allow
- raising on any termination
- raising when the process has failed
- calling a handler
- doing nothing

A termination handler receives the exit status or signal that was received by the
process.
