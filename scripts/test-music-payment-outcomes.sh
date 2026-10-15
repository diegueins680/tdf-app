#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_url="${TDF_IDENTITY_HTTP_DATABASE_URL:?Requires the fully migrated synthetic identity database}"
node --input-type=module - "$test_url" "$repo_root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
PGOPTIONS='-c statement_timeout=10000 -c lock_timeout=5000' \
  psql "$test_url" -X -v ON_ERROR_STOP=1 \
    -f "$repo_root/tdf-hq/test/sql/music_payment_outcomes.sql"
