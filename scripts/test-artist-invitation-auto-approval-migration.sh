#!/bin/sh
set -eu

test_container="tdf-artist-invitation-test-$$"
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
up_migration="$repo_root/tdf-hq/sql/2026-09-14_artist_invitation_auto_approval.sql"
down_migration="$repo_root/tdf-hq/sql/2026-09-14_artist_invitation_auto_approval_rollback.sql"

cleanup() {
  docker rm -f "$test_container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$test_container" \
  -e POSTGRES_PASSWORD=artist-invitation-test \
  -e POSTGRES_DB=artist_invitation_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d artist_invitation_test -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    docker logs "$test_container" >&2
    echo "PostgreSQL artist invitation migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -i "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d artist_invitation_test "$@"
}

apply_sql() {
  docker exec -i "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d artist_invitation_test < "$1" >/dev/null
}

psql_exec <<'SQL' >/dev/null
CREATE TABLE security_role (
  id uuid PRIMARY KEY,
  code text NOT NULL UNIQUE,
  active boolean NOT NULL,
  automatic_assignable boolean NOT NULL,
  emergency_administrator boolean NOT NULL
);
CREATE TABLE security_role_assignment_policy (
  id uuid PRIMARY KEY,
  code text NOT NULL UNIQUE,
  trigger_code text NOT NULL,
  role_id uuid NOT NULL REFERENCES security_role(id),
  name_es text NOT NULL,
  name_en text NOT NULL,
  description_es text,
  description_en text,
  requires_verified_email boolean NOT NULL DEFAULT FALSE,
  active boolean NOT NULL DEFAULT TRUE,
  effective_from timestamptz,
  effective_to timestamptz,
  created_by bigint,
  updated_by bigint,
  approved_by bigint,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  version bigint NOT NULL DEFAULT 1,
  UNIQUE (trigger_code, role_id)
);
INSERT INTO security_role (id, code, active, automatic_assignable, emergency_administrator)
VALUES ('331c2422-89e0-4cfa-ad65-8dc57f27d5e5', 'artist', TRUE, TRUE, FALSE);
CREATE OR REPLACE FUNCTION security_validate_assignment_policy()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.trigger_code NOT IN ('account-signup','verified-artist-claim','artist-profile-created','artist-self-service-activated') THEN
    RAISE EXCEPTION 'unknown automatic security policy trigger' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END $$;
CREATE TRIGGER security_assignment_policy_integrity
BEFORE INSERT OR UPDATE ON security_role_assignment_policy
FOR EACH ROW EXECUTE FUNCTION security_validate_assignment_policy();
SQL

apply_sql "$up_migration"
apply_sql "$up_migration"

binding=$(psql_exec -qAtc "SELECT count(*) || ':' || trigger_code || ':' || active::text || ':' || requires_verified_email::text FROM security_role_assignment_policy WHERE code='artist.invitation.artist' GROUP BY trigger_code,active,requires_verified_email;")
test "$binding" = "1:artist-invitation-redeemed:true:false"
validator_codes() {
  psql_exec -qAtc "SELECT pg_get_functiondef('security_validate_assignment_policy()'::regprocedure);"
}
case "$(validator_codes)" in
  *"'artist-self-service-activated'"*) ;;
  *) echo "Expected the up migration to keep artist-self-service-activated" >&2; exit 1 ;;
esac

if psql_exec -c "INSERT INTO security_role_assignment_policy (id,code,trigger_code,role_id,name_es,name_en) VALUES ('00000000-0000-4000-8000-000000000399','invalid.policy','unknown-trigger','331c2422-89e0-4cfa-ad65-8dc57f27d5e5','Inválida','Invalid');" >/dev/null 2>&1; then
  echo "Expected the assignment-policy trigger to reject an unknown trigger code" >&2
  exit 1
fi

apply_sql "$down_migration"
apply_sql "$down_migration"
test "$(psql_exec -qAtc "SELECT active::text FROM security_role_assignment_policy WHERE code='artist.invitation.artist';")" = "false"
case "$(validator_codes)" in
  *"'artist-invitation-redeemed'"*) echo "Expected rollback to remove artist-invitation-redeemed" >&2; exit 1 ;;
  *"'artist-self-service-activated'"*) ;;
  *) echo "Expected rollback to keep artist-self-service-activated" >&2; exit 1 ;;
esac

if psql_exec -c "UPDATE security_role_assignment_policy SET active=TRUE WHERE code='artist.invitation.artist';" >/dev/null 2>&1; then
  echo "Expected rollback validation to reject reactivation of the removed invitation trigger" >&2
  exit 1
fi

apply_sql "$up_migration"
test "$(psql_exec -qAtc "SELECT active::text FROM security_role_assignment_policy WHERE code='artist.invitation.artist';")" = "true"

echo "Artist invitation migration passed forward, policy binding, trigger rejection, rollback, idempotency, and reapply checks."
