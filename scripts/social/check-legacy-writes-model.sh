#!/usr/bin/env bash
set -euo pipefail
: "${TLA_JAR:?Set TLA_JAR to the pinned tla2tools-1.7.2.jar}"
TDF_SOCIAL_JAVA=${TDF_SOCIAL_JAVA:-java}
TDF_SOCIAL_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
TDF_SOCIAL_RESULTS=${TDF_SOCIAL_RESULTS:-$(mktemp -d)}
mkdir -p "$TDF_SOCIAL_RESULTS"
for config in LegacyWrites LegacyWritesLocks LegacyWritesPause LegacyWritesPair; do
  set +e
  "$TDF_SOCIAL_JAVA" -cp "$TLA_JAR" tlc2.TLC -workers 1 -deadlock \
    -metadir "$TDF_SOCIAL_RESULTS/states-$config" -config "$TDF_SOCIAL_ROOT/formal/social/$config.cfg" \
    "$TDF_SOCIAL_ROOT/formal/social/LegacyWrites.tla" > "$TDF_SOCIAL_RESULTS/$config.txt" 2>&1
  result=$?
  set -e
  if [ "$config" = LegacyWrites ]; then
    [ "$result" = 0 ]
    grep 'Model checking completed. No error has been found.' "$TDF_SOCIAL_RESULTS/$config.txt"
  else
    [ "$result" != 0 ]
    property=AuthorizedEffect
    grep "Invariant $property is violated" "$TDF_SOCIAL_RESULTS/$config.txt"
  fi
done
