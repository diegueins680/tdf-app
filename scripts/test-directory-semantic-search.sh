#!/bin/sh
# DIR-SEMANTIC-SEARCH-001 on PostgreSQL with pgvector: applies the
# migration twice, rolls it back and applies it again on the fully migrated
# synthetic database, then runs the Hspec group against it.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
export TDF_DIRECTORY_SEMANTIC_DATABASE_URL="${TDF_IDENTITY_HTTP_DATABASE_URL:?Requires the fully migrated synthetic identity database}"
node --input-type=module - "$TDF_DIRECTORY_SEMANTIC_DATABASE_URL" "$repo_root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
migration="$repo_root/tdf-hq/sql/2026-11-21_directory_semantic_search.sql"
rollback="$repo_root/tdf-hq/sql/2026-11-21_directory_semantic_search_rollback.sql"
for file in "$migration" "$migration" "$rollback" "$rollback" "$migration"; do
  psql -X -q -v ON_ERROR_STOP=1 "$TDF_DIRECTORY_SEMANTIC_DATABASE_URL" -f "$file" >/dev/null
done
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
test -x "$test_binary"
"$test_binary" --match=directory-semantic-search-postgresql --fail-on=empty
