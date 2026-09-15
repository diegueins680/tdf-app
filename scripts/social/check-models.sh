#!/usr/bin/env bash
set -euo pipefail
: "${TLA_JAR:?Set TLA_JAR to tla2tools-1.7.2.jar}"
TDF_SOCIAL_JAVA=${TDF_SOCIAL_JAVA:-java}
TDF_SOCIAL_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
TDF_SOCIAL_RESULTS=${TDF_SOCIAL_RESULTS:-$(mktemp -d)}
mkdir -p "$TDF_SOCIAL_RESULTS"
for model in Relationships Feed; do
  "$TDF_SOCIAL_JAVA" -XX:+UseParallelGC -cp "$TLA_JAR" tlc2.TLC \
    -workers 1 -metadir "$TDF_SOCIAL_RESULTS/states-$model" \
    -config "$TDF_SOCIAL_ROOT/formal/social/$model.cfg" \
    "$TDF_SOCIAL_ROOT/formal/social/$model.tla" > "$TDF_SOCIAL_RESULTS/$model.txt" 2>&1
  grep 'Model checking completed. No error has been found.' "$TDF_SOCIAL_RESULTS/$model.txt"
done
set +e
"$TDF_SOCIAL_JAVA" -cp "$TLA_JAR" tlc2.TLC -workers 1 \
  -metadir "$TDF_SOCIAL_RESULTS/states-negative" \
  -config "$TDF_SOCIAL_ROOT/formal/social/StaleCache.cfg" \
  "$TDF_SOCIAL_ROOT/formal/social/Relationships.tla" > "$TDF_SOCIAL_RESULTS/StaleCache.txt" 2>&1
result=$?
set -e
if [ "$result" -eq 0 ]; then echo 'Expected stale-cache counterexample' >&2; exit 1; fi
grep 'Invariant AuthoritativeDenial is violated' "$TDF_SOCIAL_RESULTS/StaleCache.txt"
printf 'Evidence: %s\n' "$TDF_SOCIAL_RESULTS"
