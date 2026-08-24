#!/bin/sh
set -eu

TDF_AUDIT_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_AUDIT_DATABASE="tdf_studio_intern_audit_api_e2e"
TDF_AUDIT_PORT=${TDF_AUDIT_E2E_PORT:-18089}
TDF_AUDIT_BACKEND_EXE=${TDF_AUDIT_E2E_BACKEND_EXE:-}
TDF_AUDIT_PASSWORD=${TDF_AUDIT_E2E_PASSWORD:-}
TDF_AUDIT_RUNTIME_DIR=$(mktemp -d /private/tmp/tdf-studio-audit-api-e2e.XXXXXX)
TDF_AUDIT_LOG="$TDF_AUDIT_RUNTIME_DIR/backend.log"
TDF_AUDIT_BACKEND_PID=""
TDF_AUDIT_DATABASE_CREATED=0

cleanup() {
  if [ -n "$TDF_AUDIT_BACKEND_PID" ]; then
    kill "$TDF_AUDIT_BACKEND_PID" >/dev/null 2>&1 || true
    wait "$TDF_AUDIT_BACKEND_PID" >/dev/null 2>&1 || true
  fi
  if [ "$TDF_AUDIT_DATABASE_CREATED" = "1" ]; then
    dropdb --if-exists "$TDF_AUDIT_DATABASE" >/dev/null 2>&1 || true
  fi
  case "$TDF_AUDIT_RUNTIME_DIR" in
    /private/tmp/tdf-studio-audit-api-e2e.*) rm -rf -- "$TDF_AUDIT_RUNTIME_DIR" ;;
  esac
}
trap cleanup EXIT INT TERM

if [ ! -x "$TDF_AUDIT_BACKEND_EXE" ]; then
  echo "TDF_AUDIT_E2E_BACKEND_EXE must identify the compiled backend executable" >&2
  exit 1
fi
if [ "${#TDF_AUDIT_PASSWORD}" -lt 16 ]; then
  echo "TDF_AUDIT_E2E_PASSWORD must be a runtime-only value of at least 16 characters" >&2
  exit 1
fi
case "$TDF_AUDIT_PORT" in
  ''|*[!0-9]*) echo "TDF_AUDIT_E2E_PORT must be numeric" >&2; exit 1 ;;
esac
if curl -fsS "http://127.0.0.1:$TDF_AUDIT_PORT/health" >/dev/null 2>&1; then
  echo "Refusing to reuse an occupied E2E port: $TDF_AUDIT_PORT" >&2
  exit 1
fi
if psql -d postgres -Atqc "SELECT 1 FROM pg_database WHERE datname = '$TDF_AUDIT_DATABASE'" | grep -q 1; then
  echo "Refusing to replace existing database: $TDF_AUDIT_DATABASE" >&2
  exit 1
fi

createdb "$TDF_AUDIT_DATABASE"
TDF_AUDIT_DATABASE_CREATED=1

start_backend() {
  APP_ENV=test \
  DB_HOST=127.0.0.1 \
  DB_PORT=5432 \
  DB_USER="$(id -un)" \
  DB_PASS=unused-local-test-value \
  DB_NAME="$TDF_AUDIT_DATABASE" \
  APP_PORT="$TDF_AUDIT_PORT" \
  RESET_DB=false \
  RUN_MIGRATIONS=true \
  SEED_DB="$1" \
  TDF_ENABLE_SYNTHETIC_PERSONAS=1 \
  TDF_SYNTHETIC_PERSONA_FILE="$TDF_AUDIT_ROOT/test/personas/personas.json" \
  TDF_PERSONA_TEST_PASSWORD="$TDF_AUDIT_PASSWORD" \
  TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT="$TDF_AUDIT_RUNTIME_DIR/uploads" \
  HQ_ASSETS_DIR="$TDF_AUDIT_ROOT/tdf-hq/assets" \
  EVENT_DISCOVERY_ENABLED=false \
  ARTIST_ENRICHMENT_ENABLED=false \
  EVENT_LOGISTICS_RECHECK_ENABLED=false \
  "$TDF_AUDIT_BACKEND_EXE" >"$TDF_AUDIT_LOG" 2>&1 &
  TDF_AUDIT_BACKEND_PID=$!

  attempt=0
  until curl -fsS "http://127.0.0.1:$TDF_AUDIT_PORT/health" 2>/dev/null | grep -q '"status":"ok"'; do
    if ! kill -0 "$TDF_AUDIT_BACKEND_PID" >/dev/null 2>&1; then
      echo "Backend stopped before becoming healthy" >&2
      tail -80 "$TDF_AUDIT_LOG" >&2
      exit 1
    fi
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 120 ]; then
      echo "Backend did not become healthy within 120 seconds" >&2
      tail -80 "$TDF_AUDIT_LOG" >&2
      exit 1
    fi
    sleep 1
  done
}

stop_backend() {
  kill "$TDF_AUDIT_BACKEND_PID" >/dev/null 2>&1 || true
  wait "$TDF_AUDIT_BACKEND_PID" >/dev/null 2>&1 || true
  TDF_AUDIT_BACKEND_PID=""
}

start_backend true
stop_backend

psql -X -v ON_ERROR_STOP=1 -d "$TDF_AUDIT_DATABASE" \
  < "$TDF_AUDIT_ROOT/tdf-hq/sql/2026-08-21_studio_internship_audit.sql" >/dev/null

psql -X -v ON_ERROR_STOP=1 -d "$TDF_AUDIT_DATABASE" <<'SQL' >/dev/null
WITH source_party AS (
  SELECT id
  FROM party
  WHERE lower(primary_email) = 'per-11.martina@persona.test'
), new_party AS (
  INSERT INTO party(display_name,is_org,primary_email,created_at)
  VALUES ('Lucía Torres — Other Audit Intern',FALSE,'audit.other-intern@persona.test',CURRENT_TIMESTAMP)
  RETURNING id
), copied_credential AS (
  INSERT INTO user_credential(party_id,username,password_hash,active)
  SELECT new_party.id,'audit.other-intern@persona.test',credential.password_hash,TRUE
  FROM new_party
  JOIN source_party ON TRUE
  JOIN user_credential credential ON credential.party_id=source_party.id AND credential.active=TRUE
  RETURNING party_id
)
INSERT INTO party_security_role(
  id,party_id,role_id,granted_by,approved_by,approval_mode,emergency_reason,
  source_revision_id,source_policy_id,active,created_at,revoked_at,version
)
SELECT
  gen_random_uuid(),copied_credential.party_id,assignment.role_id,assignment.granted_by,
  assignment.approved_by,assignment.approval_mode,assignment.emergency_reason,
  assignment.source_revision_id,assignment.source_policy_id,TRUE,CURRENT_TIMESTAMP,NULL,1
FROM copied_credential
JOIN source_party ON TRUE
JOIN party_security_role assignment ON assignment.party_id=source_party.id AND assignment.active=TRUE
JOIN security_role role ON role.id=assignment.role_id AND role.code='intern';
SQL

start_backend false

TDF_AUDIT_E2E_API_BASE="http://127.0.0.1:$TDF_AUDIT_PORT" \
TDF_AUDIT_E2E_PASSWORD="$TDF_AUDIT_PASSWORD" \
TDF_AUDIT_E2E_OTHER_INTERN_EMAIL="audit.other-intern@persona.test" \
node "$TDF_AUDIT_ROOT/scripts/test-studio-internship-audit-api-e2e.mjs"

OUTBOX_CHECK=$(psql -X -d "$TDF_AUDIT_DATABASE" -Atqc \
  "SELECT count(*) > 0 AND bool_and(test_transport) AND bool_and(dispatched_at IS NULL) FROM intern_audit_notification_outbox")
if [ "$OUTBOX_CHECK" != "t" ]; then
  echo "Notification outbox was not isolated to undispatched test transport" >&2
  exit 1
fi

MIDPOINT_UNIQUENESS_CHECK=$(psql -X -d "$TDF_AUDIT_DATABASE" -Atqc \
  "SELECT count(*) > 0 AND count(*) = count(DISTINCT (plan_id, recipient_party_id)) FROM intern_audit_notification_outbox WHERE template_key='internship_midpoint_reached'")
if [ "$MIDPOINT_UNIQUENESS_CHECK" != "t" ]; then
  echo "Midpoint notifications were not enqueued exactly once per plan and recipient" >&2
  exit 1
fi

TEAM_NOTIFICATION_CHECK=$(psql -X -d "$TDF_AUDIT_DATABASE" -Atqc \
  "SELECT count(*) > 0 FROM notification WHERE notif_type IN ('internship_midpoint_reached','internship_assignment_blocked','internship_final_ready','internal_feedback_information_response','internal_feedback_retest_recorded') AND recipient_party_id IN (SELECT party_id FROM party_security_role assignment JOIN security_role role ON role.id=assignment.role_id WHERE assignment.active AND role.code IN ('admin','manager','studio-manager'))")
if [ "$TEAM_NOTIFICATION_CHECK" != "t" ]; then
  echo "Immediate team in-app notifications were not created for the synthetic authorized team" >&2
  exit 1
fi

echo "Studio internship audit API E2E passed with synthetic admin, assigned intern, isolated other intern, test notifications, and disposable persistence"
