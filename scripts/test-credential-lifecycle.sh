#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
export TDF_CREDENTIAL_LIFECYCLE_DATABASE_URL="${TDF_IDENTITY_HTTP_DATABASE_URL:?Requires the fully migrated synthetic identity database}"
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
test -x "$test_binary"
"$test_binary" --match=credential-lifecycle-postgresql --fail-on=empty
