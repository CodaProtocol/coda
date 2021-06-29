#!/bin/bash

set -euo pipefail

# If glob doesn't match anything, return empty string rather than literal pattern
shopt -s nullglob

# Print all commands executed if DEBUG mode enabled
[ -n "${DEBUG:-""}" ] && set -x

# Attempt to execute or source custom entrypoint scripts accordingly
for script in /entrypoint.d/*; do
  if [ -x "$script" ]; then
    "$script" "$@"
  else
    source "$script"
  fi
done

# Always run command under dumb-init so signals are forwarded correctly
exec /usr/bin/dumb-init "$@"