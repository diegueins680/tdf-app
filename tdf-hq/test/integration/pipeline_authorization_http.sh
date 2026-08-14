#!/usr/bin/env bash
set -euo pipefail

: "${DATABASE_URL:?Set DATABASE_URL to a disposable PostgreSQL database}"
: "${API_BASE_URL:?Set API_BASE_URL to a running candidate, for example http://127.0.0.1:18127}"

allowed_token="pipeline-auth-allowed-$$"
denied_token="pipeline-auth-denied-$$"
denied_party_label="Pipeline authorization denied $$"

cleanup() {
  psql "${DATABASE_URL}" -X -q \
    -v allowed_token="${allowed_token}" \
    -v denied_token="${denied_token}" \
    -v denied_party_label="${denied_party_label}" <<'SQL'
DELETE FROM api_token WHERE token IN (:'allowed_token', :'denied_token');
ALTER TABLE party_security_role DISABLE TRIGGER catalog_no_hard_delete;
DELETE FROM party_security_role assignment
USING party fixture
WHERE assignment.party_id=fixture.id AND fixture.display_name=:'denied_party_label';
ALTER TABLE party_security_role ENABLE TRIGGER catalog_no_hard_delete;
DELETE FROM party WHERE display_name=:'denied_party_label';
SQL
}
trap cleanup EXIT

psql "${DATABASE_URL}" -X -q \
  -v allowed_token="${allowed_token}" \
  -v denied_token="${denied_token}" \
  -v denied_party_label="${denied_party_label}" <<'SQL'
WITH allowed_party AS (
  SELECT assignment.party_id
  FROM party_security_role assignment
  JOIN security_role role ON role.id=assignment.role_id AND role.active
  JOIN role_permission grant_row ON grant_row.role_id=role.id AND grant_row.active
  JOIN security_permission permission
    ON permission.id=grant_row.permission_id
    AND permission.active AND permission.code='pipeline.read'
  WHERE assignment.active
  ORDER BY assignment.party_id
  LIMIT 1
), denied_party AS (
  INSERT INTO party (display_name, is_org, created_at)
  VALUES (:'denied_party_label', FALSE, now())
  RETURNING id AS party_id
), denied_assignment AS (
  INSERT INTO party_security_role
    (party_id, role_id, approval_mode, active, created_at, version)
  SELECT denied_party.party_id, role.id, 'bootstrap', TRUE, now(), 1
  FROM denied_party
  JOIN security_role role ON role.code='artist' AND role.active
  RETURNING party_id
), inserted_allowed AS (
  INSERT INTO api_token (token, party_id, label, active)
  SELECT :'allowed_token', party_id, 'Pipeline authorization integration allowed', TRUE
  FROM allowed_party
  RETURNING id
), inserted_denied AS (
  INSERT INTO api_token (token, party_id, label, active)
  SELECT :'denied_token', party_id, 'Pipeline authorization integration denied', TRUE
  FROM denied_assignment
  RETURNING id
)
SELECT count(*) FROM inserted_allowed CROSS JOIN inserted_denied;

SELECT 1 / CASE WHEN
  EXISTS (SELECT 1 FROM api_token WHERE token=:'allowed_token' AND active)
  AND EXISTS (SELECT 1 FROM api_token WHERE token=:'denied_token' AND active)
THEN 1 ELSE 0 END AS fixtures_created;
SQL

allowed_status="$({ curl -sS -o /dev/null -w '%{http_code}' \
  -H "Authorization: Bearer ${allowed_token}" \
  "${API_BASE_URL%/}/pipelines/definitions"; } 2>/dev/null)"
denied_body="$(mktemp)"
denied_status="$({ curl -sS -o "${denied_body}" -w '%{http_code}' \
  -H "Authorization: Bearer ${denied_token}" \
  "${API_BASE_URL%/}/pipelines/definitions"; } 2>/dev/null)"

if [[ "${allowed_status}" != "200" ]]; then
  echo "Expected pipeline.read request to return 200, got ${allowed_status}" >&2
  exit 1
fi
if [[ "${denied_status}" != "403" ]]; then
  echo "Expected Scheduling-only request to return 403, got ${denied_status}" >&2
  exit 1
fi
if ! grep -q 'Missing pipeline capability: pipeline.read' "${denied_body}"; then
  echo 'Denied pipeline response did not identify the missing persisted capability' >&2
  exit 1
fi

echo 'pipeline authorization HTTP checks passed'
