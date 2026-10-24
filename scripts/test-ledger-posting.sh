#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
export TDF_LEDGER_POSTING_DATABASE_URL="${TDF_IDENTITY_HTTP_DATABASE_URL:?Requires the fully migrated synthetic identity database}"
node --input-type=module - "$TDF_LEDGER_POSTING_DATABASE_URL" "$repo_root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
test -x "$test_binary"
"$test_binary" --match=ledger-posting-postgresql --fail-on=empty
python3 "$repo_root/scripts/__tests__/ledger-posting-upgrade.py"
python3 "$repo_root/scripts/__tests__/ledger-posting-snapshot.py" --negative-controls
