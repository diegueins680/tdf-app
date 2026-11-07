#!/bin/sh
# Fresh isolated PostgreSQL only. This is also the schema used by the provider
# runtime lane; never replace it with reduced ticket/service table projections.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
url=${1:?Pass a disposable PostgreSQL URL}
node --input-type=module - "$url" "$root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
test -z "${PGHOSTADDR:-}${PGSERVICE:-}${PGSERVICEFILE:-}" || exit 1
count=$(psql "$url" -X -qAt -v ON_ERROR_STOP=1 -c "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p')")
[ "$count" = 0 ] || { echo 'Full payment schema requires an empty database' >&2; exit 1; }
psql "$url" -X -q -v ON_ERROR_STOP=1 -f "$root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
psql "$url" -X -q -v ON_ERROR_STOP=1 -f "$root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
batch=$(mktemp)
trap 'rm -f "$batch"' EXIT INT TERM
candidate=$(git -C "$root" rev-parse --verify HEAD)
SOURCE_COMMIT="$candidate" node "$root/scripts/render-production-migration-batch.mjs" > "$batch"
psql "$url" -X -q -v ON_ERROR_STOP=1 -f "$batch" >/dev/null
psql "$url" -X -q -v ON_ERROR_STOP=1 -f "$batch" >/dev/null
expected=$(node -p "require('$root/scripts/production-migrations.json').migrations.length")
actual=$(psql "$url" -X -qAt -v ON_ERROR_STOP=1 -c 'SELECT count(*) FROM tdf_schema_migration')
[ "$actual" = "$expected" ] || { echo "Expected $expected full-schema migrations; got $actual" >&2; exit 1; }
echo "Full payment schema applied and replayed: $actual migrations"
