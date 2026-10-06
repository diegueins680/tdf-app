#!/bin/sh
set -eu
test "${EVENT_OPERATIONS_DISPOSABLE_HTTP_TEST:-}" = 1 || {
  echo 'Disposable HTTP test guard required' >&2; exit 1;
}
test -n "${EVENT_OPERATIONS_TEST_DSN:-}" || { echo 'Test DSN required' >&2; exit 1; }
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root/tdf-hq"
# Compile through the repository Stack environment. Cache only this harness's objects so repeated
# test runs do not re-interpret all Persistent models or overwrite the normal backend build.
mkdir -p .stack-work/event-operations-http
stack exec -- ghc -O0 -threaded -isrc -itest -outputdir .stack-work/event-operations-http \
  test/EventOperationsHttpMain.hs -o .stack-work/event-operations-http/event-operations-http-test
.stack-work/event-operations-http/event-operations-http-test
