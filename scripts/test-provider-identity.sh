#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_database="tdf_provider_identity_test_$$"
export PGHOST="${PGHOST:-127.0.0.1}"
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
createdb "$test_database"
trap cleanup EXIT INT TERM
psql -X -v ON_ERROR_STOP=1 -d "$test_database" <<'SQL'
CREATE EXTENSION pgcrypto;
CREATE TABLE party(id bigserial PRIMARY KEY, primary_email text);
CREATE TABLE user_credential(id bigserial PRIMARY KEY,party_id bigint NOT NULL REFERENCES party(id),username text UNIQUE NOT NULL,password_hash text NOT NULL,active boolean NOT NULL);
INSERT INTO party(primary_email) VALUES ('shared@example.test'),('shared@example.test'),('disabled@example.test');
INSERT INTO user_credential(party_id,username,password_hash,active) SELECT id,CASE id WHEN 1 THEN 'one' WHEN 2 THEN 'two' ELSE 'disabled' END,crypt('correct-password',gen_salt('bf')),id<>3 FROM party;
SQL
migration="$repo_root/tdf-hq/sql/2026-09-18_provider_subject_identity.sql"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$migration"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$migration"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/tdf-hq/sql/2026-09-18_provider_subject_identity_rollback.sql"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$migration"
export TDF_PROVIDER_IDENTITY_TEST_DB="host=$PGHOST dbname=$test_database"
cd "$repo_root/tdf-hq"
stack test tdf-hq:test:tdf-hq-test --fast --ghc-options=-O0 --jobs 1 --test-arguments='--match provider-identity-postgresql'

if psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/tdf-hq/sql/2026-09-18_provider_subject_identity_rollback.sql" >/dev/null 2>&1; then
  echo 'Rollback incorrectly removed established provider bindings' >&2
  exit 1
fi
