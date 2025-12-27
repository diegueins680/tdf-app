#!/usr/bin/env bash
set -euo pipefail

if [[ "$(uname -s)" == "Darwin" && -z "${MACOSX_DEPLOYMENT_TARGET:-}" ]]; then
  # Align the deployment target with the host macOS to avoid libpq linker warnings.
  macos_ver=$(sw_vers -productVersion | awk -F. '{print $1 "." $2}')
  if [[ -n "$macos_ver" ]]; then
    export MACOSX_DEPLOYMENT_TARGET="$macos_ver"
  fi
fi

set -a
source config/default.env
set +a

stack setup
stack build
stack run
