#!/usr/bin/env bash
set -euo pipefail
jar=$(realpath "${TLA2TOOLS_JAR:?set TLA2TOOLS_JAR to the repository-pinned 1.7.2 jar}")
echo "fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22  $jar" | sha256sum -c -
root=$(mktemp -d)
trap 'rm -rf "$root"' EXIT
cp formal/production/{*.cfg,*.tla} "$root/"
cd "$root"
for scenario in graph review plan read; do
  java -cp "$jar" tlc2.TLC -workers 1 -config "$scenario.cfg" Workspace.tla
  sed 's/Unsafe = FALSE/Unsafe = TRUE/' "$scenario.cfg" > unsafe.cfg
  if java -cp "$jar" tlc2.TLC -workers 1 -config unsafe.cfg Workspace.tla > negative.log; then
    echo "Negative control passed unexpectedly: $scenario" >&2
    exit 1
  fi
  grep 'Invariant .* is violated' negative.log
done
