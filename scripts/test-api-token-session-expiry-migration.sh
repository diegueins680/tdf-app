#!/usr/bin/env bash
# Session expiry migration (companion to ID-SESSION-003): idempotent, additive,
# never backfills legacy interactive tokens, and rejects malformed windows.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
db="${TDF_IDENTITY_HTTP_DATABASE_URL:?Requires isolated fully migrated test database}"
node --input-type=module - "$db" "$root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
migration="$root/tdf-hq/sql/2026-10-31_api_token_session_expiry.sql"
# Repeat application must be a no-op.
psql "$db" -X -q -v ON_ERROR_STOP=1 -f "$migration"
psql "$db" -X -q -v ON_ERROR_STOP=1 -f "$migration"
psql "$db" -X -q -v ON_ERROR_STOP=1 <<'SQL'
BEGIN;
DO $$
DECLARE
  party_key BIGINT;
  legacy_key BIGINT;
  service_key BIGINT;
  now_epoch BIGINT := floor(extract(epoch FROM clock_timestamp()))::bigint;
BEGIN
  IF (SELECT count(*) FROM information_schema.columns
      WHERE table_schema='public' AND table_name='api_token'
        AND column_name IN ('issued_at_epoch','expires_at_epoch')
        AND data_type='bigint' AND is_nullable='YES') <> 2 THEN
    RAISE EXCEPTION 'api_token validity columns missing or not nullable bigint';
  END IF;
  INSERT INTO party(display_name,is_org,created_at) VALUES ('Session expiry migration fixture',false,now()) RETURNING id INTO party_key;
  -- Legacy interactive token: the migration must leave it without a window.
  INSERT INTO api_token(token,party_id,label,active)
    VALUES ('session-expiry-legacy-fixture',party_key,'password-login:synthetic@example.test',true) RETURNING id INTO legacy_key;
  IF EXISTS (SELECT 1 FROM api_token WHERE id=legacy_key AND (issued_at_epoch IS NOT NULL OR expires_at_epoch IS NOT NULL)) THEN
    RAISE EXCEPTION 'legacy interactive tokens must not acquire a validity window';
  END IF;
  -- Service token without a window stays representable.
  INSERT INTO api_token(token,party_id,label,active)
    VALUES ('session-expiry-service-fixture',party_key,'service:synthetic',true) RETURNING id INTO service_key;
  -- Malformed windows are rejected.
  BEGIN
    UPDATE api_token SET issued_at_epoch=now_epoch WHERE id=service_key;
    RAISE EXCEPTION 'half window accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    UPDATE api_token SET issued_at_epoch=now_epoch, expires_at_epoch=now_epoch WHERE id=service_key;
    RAISE EXCEPTION 'empty window accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    UPDATE api_token SET issued_at_epoch=-1, expires_at_epoch=now_epoch WHERE id=service_key;
    RAISE EXCEPTION 'negative epoch accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  -- A well-formed window is accepted.
  UPDATE api_token SET issued_at_epoch=now_epoch, expires_at_epoch=now_epoch+3600 WHERE id=service_key;
  DELETE FROM api_token WHERE id IN (legacy_key, service_key);
  DELETE FROM party WHERE id=party_key;
END $$;
ROLLBACK;
SQL
echo 'Session expiry migration: repeat application, legacy non-backfill and validity-window constraints passed.'
