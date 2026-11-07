#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
: "${TDF_TERMS_HISTORY_DATABASE_URL:?Requires a fully migrated disposable database}"
node --input-type=module - "$TDF_TERMS_HISTORY_DATABASE_URL" "$repo_root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
psql "$TDF_TERMS_HISTORY_DATABASE_URL" -X -v ON_ERROR_STOP=1 \
  -f "$repo_root/tdf-hq/test/integration/checkout_terms_history_identity.sql"
