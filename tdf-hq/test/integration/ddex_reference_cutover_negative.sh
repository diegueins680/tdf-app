#!/usr/bin/env bash
set -euo pipefail

: "${DATABASE_URL:?Set DATABASE_URL to a disposable PostgreSQL database}"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
apply_script="${script_dir}/../../sql/2026-08-12_ddex_reference_cutover_apply.sql"
fixture_token="ddex-negative-$$"

cleanup() {
  psql "${DATABASE_URL}" -v fixture_token="${fixture_token}" -X -q <<'SQL'
ALTER TABLE ddex_document DISABLE TRIGGER ddex_document_canonical_integrity;
ALTER TABLE ddex_export DISABLE TRIGGER ddex_export_canonical_integrity;
ALTER TABLE ddex_partner_standard_version DISABLE TRIGGER ddex_partner_standard_integrity;
ALTER TABLE ddex_document DISABLE TRIGGER catalog_no_hard_delete;
ALTER TABLE ddex_export DISABLE TRIGGER catalog_no_hard_delete;
ALTER TABLE ddex_partner_standard_version DISABLE TRIGGER catalog_no_hard_delete;
ALTER TABLE ddex_partner DISABLE TRIGGER catalog_no_hard_delete;
DELETE FROM ddex_export WHERE private_uri LIKE 'private://integration/' || :'fixture_token' || '%';
DELETE FROM ddex_document WHERE private_uri LIKE 'private://integration/' || :'fixture_token' || '%';
DELETE FROM ddex_partner_standard_version membership USING ddex_partner partner
  WHERE membership.partner_id=partner.id AND partner.name LIKE :'fixture_token' || '%';
DELETE FROM ddex_partner WHERE name LIKE :'fixture_token' || '%';
ALTER TABLE ddex_document ENABLE TRIGGER ddex_document_canonical_integrity;
ALTER TABLE ddex_export ENABLE TRIGGER ddex_export_canonical_integrity;
ALTER TABLE ddex_partner_standard_version ENABLE TRIGGER ddex_partner_standard_integrity;
ALTER TABLE ddex_document ENABLE TRIGGER catalog_no_hard_delete;
ALTER TABLE ddex_export ENABLE TRIGGER catalog_no_hard_delete;
ALTER TABLE ddex_partner_standard_version ENABLE TRIGGER catalog_no_hard_delete;
ALTER TABLE ddex_partner ENABLE TRIGGER catalog_no_hard_delete;
SQL
}
trap cleanup EXIT

insert_fixture() {
  local case_name="$1"
  local version_value="$2"
  local with_export="$3"

  psql "${DATABASE_URL}" \
    -v partner_name="${fixture_token}-${case_name}" \
    -v version_value="${version_value}" \
    -v with_export="${with_export}" \
    -X -q <<'SQL'
BEGIN;
ALTER TABLE ddex_partner ADD COLUMN IF NOT EXISTS allowed_versions text[] NOT NULL DEFAULT ARRAY[]::text[];
ALTER TABLE ddex_document DISABLE TRIGGER ddex_document_canonical_integrity;
ALTER TABLE ddex_export DISABLE TRIGGER ddex_export_canonical_integrity;
WITH partner AS (
  INSERT INTO ddex_partner (name,dpid,rules_json,naming_convention,is_active,allowed_versions)
  VALUES (:'partner_name',NULL,NULL,NULL,TRUE,ARRAY[:'version_value']::text[])
  RETURNING id
), document AS (
  INSERT INTO ddex_document (
    file_name,private_uri,sha256,size_bytes,family,version,namespace,message_type,status,
    uploaded_by,created_at
  ) VALUES (
    :'partner_name' || '.xml','private://integration/' || :'partner_name' || '/document',
    md5(:'partner_name'),100,'FamilyERN',:'version_value',NULL,'NewReleaseMessage',
    'StatusReceived',1,now()
  ) RETURNING id
)
INSERT INTO ddex_export (
  release_id,partner_id,ern_version,xml_checksum,private_uri,created_at
)
SELECT 987654321,partner.id,:'version_value',md5(:'partner_name' || '-export'),
  'private://integration/' || :'partner_name' || '/export',now()
FROM partner WHERE :'with_export'='yes';
ALTER TABLE ddex_document ENABLE TRIGGER ddex_document_canonical_integrity;
ALTER TABLE ddex_export ENABLE TRIGGER ddex_export_canonical_integrity;
COMMIT;
SQL
}

assert_failed_atomically() {
  local case_name="$1"
  local version_value="$2"
  local run_code="$3"
  psql "${DATABASE_URL}" \
    -v partner_name="${fixture_token}-${case_name}" \
    -v version_value="${version_value}" \
    -v run_code="${run_code}" -X -q <<'SQL'
SELECT count(*)=1 AS document_unchanged FROM ddex_document
WHERE private_uri='private://integration/' || :'partner_name' || '/document'
  AND family='FamilyERN' AND version=:'version_value' AND standard_version_id IS NULL
\gset
SELECT count(*)=0 AS no_failed_run FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision='negative-ddex-v1' AND NOT dry_run
\gset
\if :document_unchanged
\else
  \echo 'Failed DDEX cutover changed its source document'
  \quit 1
\endif
\if :no_failed_run
\else
  \echo 'Failed DDEX cutover committed a run record'
  \quit 1
\endif
SQL
}

run_negative_case() {
  local case_name="$1"
  local version_value="$2"
  local with_export="$3"
  local safety_threshold="$4"
  local run_code="ddex-reference-negative-$$-${case_name}"
  insert_fixture "${case_name}" "${version_value}" "${with_export}"
  if psql "${DATABASE_URL}" -v run_code="${run_code}" \
    -v candidate_revision='negative-ddex-v1' -v safety_threshold="${safety_threshold}" \
    -v batch_size=2 -X -f "${apply_script}"; then
    echo "Expected ${case_name} DDEX cutover to fail" >&2
    exit 1
  fi
  assert_failed_atomically "${case_name}" "${version_value}" "${run_code}"
}

run_negative_case threshold 432 yes 0
run_negative_case unresolved 999 no 20

echo "DDEX reference cutover negative checks passed"
