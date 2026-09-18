#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_database="tdf_course_identity_test_$$"
export PGHOST="${PGHOST:-127.0.0.1}"
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
createdb "$test_database"
trap cleanup EXIT INT TERM
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
node "$repo_root/scripts/render-production-migration-batch.mjs" | psql -X -v ON_ERROR_STOP=1 -d "$test_database" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/tdf-hq/sql/2026-09-18_course_identity_requests_rollback.sql"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/tdf-hq/sql/2026-09-18_course_identity_requests.sql"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" <<'SQL'
INSERT INTO course(slug,title,price_cents,currency,capacity) VALUES ('identity-test-course','Synthetic identity course',0,'USD',20);
INSERT INTO party(display_name,is_org,primary_email,primary_phone,created_at) VALUES ('Established synthetic account',FALSE,'course-identity@example.test','+593990000111',NOW());
INSERT INTO user_credential(party_id,username,password_hash,active) SELECT id,'synthetic-course-established','synthetic-invalid-login-hash',TRUE FROM party WHERE display_name='Established synthetic account';
INSERT INTO course(slug,title,price_cents,currency,capacity) VALUES ('identity-paid-course','Synthetic paid identity course',1000,'USD',20);
INSERT INTO course_checkout_policy(course_id,policy_version,currency,price_minor,terms_version,terms_summary,cancellation_policy,approval_status,active,approved_at,approved_by)
 SELECT id,'identity-test-v1','USD',1000,'identity-terms-v1','Synthetic terms','Synthetic cancellation','approved',TRUE,now(),'synthetic-test' FROM course WHERE slug='identity-paid-course';

SQL
export COMMERCE_CHECKOUT_ENV=production
export TDF_COURSE_IDENTITY_TEST_DB="host=$PGHOST dbname=$test_database"
cd "$repo_root/tdf-hq"
stack test tdf-hq:test:tdf-hq-test --fast --ghc-options=-O0 --jobs 1 --test-arguments='--match course-identity-postgresql'
if psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/tdf-hq/sql/2026-09-18_course_identity_requests_rollback.sql" >/dev/null 2>&1; then
  echo 'Rollback incorrectly removed accepted course receipts' >&2
  exit 1
fi
