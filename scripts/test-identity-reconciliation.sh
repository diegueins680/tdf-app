#!/usr/bin/env bash
set -euo pipefail
root=$(cd -- "$(dirname -- "$0")/.." && pwd)
node "$root/scripts/generate-identity-party-reference-view.mjs" --check
: "${TDF_IDENTITY_TEST_DATABASE_URL:?Set a dedicated empty test database URL}"
psql_cmd=(psql "$TDF_IDENTITY_TEST_DATABASE_URL" -X -v ON_ERROR_STOP=1)
test "$("${psql_cmd[@]}" -Atc "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p')")" = 0
"${psql_cmd[@]}" <<'SQL'
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE TABLE party(id bigserial PRIMARY KEY, display_name text NOT NULL,legal_name text,is_org boolean NOT NULL DEFAULT false,tax_id text,primary_email text,primary_phone text,whatsapp text,instagram text,emergency_contact text,notes text,stripe_customer_id text,country_code text,country_id uuid,created_at timestamptz NOT NULL DEFAULT now());
CREATE TABLE user_credential(id bigserial PRIMARY KEY,party_id bigint REFERENCES party(id),username text UNIQUE,password_hash text,active boolean NOT NULL DEFAULT true);
CREATE TABLE booking(id bigserial PRIMARY KEY,party_id bigint REFERENCES party(id),notes text);
CREATE TABLE party_security_role(id bigserial PRIMARY KEY,party_id bigint,role_id uuid);
CREATE TABLE catalog_import_job(id bigserial PRIMARY KEY,requested_by bigint);
CREATE TABLE external_review_fixture(id bigserial PRIMARY KEY,reviewer_id bigint);
CREATE TABLE catalog_revision(id bigserial PRIMARY KEY,reviewed_by bigint,approved_by bigint);
CREATE TABLE catalog_audit_event(id bigserial PRIMARY KEY,reviewer_id bigint,approver_id bigint);
INSERT INTO party(display_name) VALUES('Operator');
SQL
"${psql_cmd[@]}" -f "$root/tdf-hq/sql/2026-09-17_identity_reconciliation.sql"
"${psql_cmd[@]}" -f "$root/tdf-hq/sql/2026-09-17_identity_reconciliation.sql"
"${psql_cmd[@]}" -f "$root/tdf-hq/sql/2026-09-18_identity_review_dependencies.sql"
"${psql_cmd[@]}" -f "$root/tdf-hq/sql/2026-09-18_identity_review_dependencies.sql"
"${psql_cmd[@]}" -f "$root/tdf-hq/test/sql/identity_reconciliation.sql"
# Real concurrent connections: both transactions must return the same ID.
work=$(mktemp -d "${TMPDIR:-/tmp}/tdf-identity-concurrency.XXXXXX")
query="BEGIN; SET LOCAL lock_timeout='10s'; SELECT identity_create_contact(1,'concurrent-request-0001','{\"display_name\":\"Concurrent contact\",\"is_org\":false}'); COMMIT;"
"${psql_cmd[@]}" -qAtc "$query" > "$work/first" &
first_pid=$!
"${psql_cmd[@]}" -qAtc "$query" > "$work/second" &
second_pid=$!
wait "$first_pid"
wait "$second_pid"
cmp "$work/first" "$work/second"
test "$("${psql_cmd[@]}" -qAtc "SELECT count(*) FROM party WHERE display_name='Concurrent contact'")" = 1
# SQL privileges: an untrusted role cannot inspect history or run a merge.
"${psql_cmd[@]}" <<'SQL'
BEGIN;
CREATE ROLE identity_untrusted_test;
DO $$ BEGIN
  IF has_table_privilege('identity_untrusted_test','identity_complementary_link','SELECT')
    OR has_function_privilege('identity_untrusted_test','identity_link_parties(uuid,uuid,text)','EXECUTE')
    OR has_table_privilege('identity_untrusted_test','identity_merge_history','SELECT')
    OR has_function_privilege('identity_untrusted_test','identity_create_contact(bigint,text,jsonb)','EXECUTE')
    OR has_function_privilege('identity_untrusted_test','identity_execute_merge(uuid,uuid,text)','EXECUTE') THEN
    RAISE EXCEPTION 'identity history or merge privileges leaked';
  END IF;
END $$;
ROLLBACK;
SQL
printf '%s\n' 'Identity reconciliation migration, replay, concurrency and access checks passed.'
