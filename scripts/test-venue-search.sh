#!/bin/sh
# EVT-VENUE-SEARCH-001: venue search SQL against PostgreSQL. Compares the statement
# the server sends with the Haskell specification on generated venue tables, the
# installed fold function with Data.Text.toCaseFold on every Unicode scalar value,
# and the list handler with the runtime flag off and on.
#
# Requires a disposable database that has received the production migration
# manifest (venue, revenue_feature_flag and 2026-11-21_venue_search_sql.sql). The
# test works on session-private copies of both tables and leaves the database's
# own rows untouched. No production connection input is accepted.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
export TDF_VENUE_SEARCH_DATABASE_URL="${TDF_VENUE_SEARCH_DATABASE_URL:-${TDF_IDENTITY_HTTP_DATABASE_URL:?Requires the fully migrated synthetic identity database}}"
node --input-type=module - "$TDF_VENUE_SEARCH_DATABASE_URL" "$repo_root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
test -x "$test_binary"
"$test_binary" --match=venue-search-postgresql --fail-on=empty --fail-on=pending
