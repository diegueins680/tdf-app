#!/usr/bin/env bash
# Own one isolated DB; reuse the canonical identity driver, then exercise the
# owner canonical identity/rollback suite and the registered manager PG case.
set -euo pipefail
root=$(cd -- "$(dirname -- "$0")/.." && pwd)
cd "$root"
export PGHOST="${PGHOST:-127.0.0.1}" PGPORT="${PGPORT:-5432}"
export PGUSER="${PGUSER:-postgres}" PGPASSWORD="${PGPASSWORD:-postgres}"
test_database="tdf_video_campaign_$$_test"
export TDF_IDENTITY_TEST_DATABASE_URL="postgresql://${PGHOST}:${PGPORT}/${test_database}"
node --input-type=module -e 'import assert from "node:assert/strict"; import {disposablePostgresUrl} from "./scripts/lib/disposable-postgres-url.mjs"; const port=process.env.PGPORT; assert.ok(/^[1-9][0-9]{0,4}$/.test(port) && Number(port)<=65535,"A canonical decimal test port in 1..65535 is required"); const url=disposablePostgresUrl(process.env.TDF_IDENTITY_TEST_DATABASE_URL,{ci:process.env.CI==="true",env:process.env}); assert.ok(process.env.PGHOST===url.hostname && (url.port||"5432")===port,"Raw test routing must match the validated literal hostname and port");'
createdb "$test_database"
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
bash scripts/test-identity-reconciliation.sh
export TDF_VIDEO_CAMPAIGN_MANAGER_TEST_DB="$TDF_IDENTITY_TEST_DATABASE_URL"
cd "$root/tdf-hq"
output=$(stack test ${TDF_STACK_FLAGS:-} tdf-hq:test:tdf-hq-test --jobs 1 \
  --test-arguments='--match "video campaign current directory grants" --fail-on=pending' 2>&1) || {
  printf '%s\n' "$output"; exit 1; }
printf '%s\n' "$output"
summary_output=$(printf '%s\n' "$output" | sed "s/$(printf '\033')\[[0-9;]*m//g")
printf '%s\n' "$summary_output" | grep -Eq '^[1-9][0-9]* examples?, 0 failures$' || {
  echo 'Video campaign manager PostgreSQL examples did not run' >&2; exit 1; }
