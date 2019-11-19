#!/bin/bash

# build run_ppx_coda, then run Python script to compare versioned types in a pull request

source ~/.profile && \
    (dune build --profile=print_versioned_types lib/ppx_coda/run_ppx_coda.exe) && \
    ./scripts/compare_pr_diff_types.py ${CIRCLE_PULL_REQUEST}
