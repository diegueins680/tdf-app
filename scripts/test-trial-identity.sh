#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_database="tdf_trial_identity_test_$$"
export PGHOST="${PGHOST:-127.0.0.1}"
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
createdb "$test_database"
trap cleanup EXIT INT TERM
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
node "$repo_root/scripts/render-production-migration-batch.mjs" | psql -X -v ON_ERROR_STOP=1 -d "$test_database" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/tdf-hq/sql/2026-09-18_trial_identity_requests_rollback.sql"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/tdf-hq/sql/2026-09-18_trial_identity_requests.sql"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" <<'SQL'
INSERT INTO party(id,display_name,is_org,primary_email,created_at) VALUES
 (900101,'Established synthetic account',FALSE,'trial-identity@example.test',NOW()),
 (900102,'Synthetic school administrator A',FALSE,NULL,NOW()),
 (900103,'Synthetic school administrator B',FALSE,NULL,NOW());
INSERT INTO user_credential(party_id,username,password_hash,active) VALUES (900101,'synthetic-trial-established','synthetic-non-login-hash',TRUE);
INSERT INTO subject(id,name,active) VALUES (900101,'Synthetic identity subject',TRUE);
INSERT INTO teacher_subject(teacher_id,subject_id) VALUES (900102,900101);
DO $$ BEGIN
 BEGIN
  INSERT INTO identity_trial_request(request_scope,request_key,party_id,request_payload,response_payload)
  VALUES ('invalid-scope','invalid-scope-fixture',900101,'{}','{}');
  RAISE EXCEPTION 'invalid anonymous scope accepted';
 EXCEPTION WHEN check_violation THEN NULL;
 END;
END $$;
SQL
export TDF_TRIAL_IDENTITY_TEST_DB="host=$PGHOST dbname=$test_database"
cd "$repo_root/tdf-hq"
stack test tdf-hq:test:tdf-hq-test --fast --ghc-options=-O0 --jobs 1 --test-arguments='--match trial-identity-postgresql'
if psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/tdf-hq/sql/2026-09-18_trial_identity_requests_rollback.sql" >/dev/null 2>&1; then
  echo 'Rollback incorrectly removed accepted trial receipts' >&2
  exit 1
fi
