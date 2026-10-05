#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
db="${TDF_IDENTITY_HTTP_DATABASE_URL:?Requires isolated fully migrated test database}"
node --input-type=module - "$db" "$root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
psql "$db" -X -q -v ON_ERROR_STOP=1 -f "$root/tdf-hq/sql/2026-10-04_auth_recovery_expiry.sql"
psql "$db" -X -q -v ON_ERROR_STOP=1 <<'SQL'
BEGIN;
DO $$
DECLARE
  party_key BIGINT;
  credential_key BIGINT;
  token_key BIGINT;
BEGIN
  INSERT INTO party(display_name,is_org,created_at) VALUES ('Recovery migration fixture',false,now()) RETURNING id INTO party_key;
  INSERT INTO user_credential(party_id,username,password_hash,active) VALUES (party_key,'recovery-migration-fixture','synthetic-unused',true) RETURNING id INTO credential_key;
  INSERT INTO api_token(token,party_id,label,active) VALUES ('recovery-migration-fixture',party_key,'password-reset:synthetic@example.test',true) RETURNING id INTO token_key;
  IF EXISTS (SELECT 1 FROM auth_recovery_challenge WHERE api_token_id=token_key) THEN RAISE EXCEPTION 'legacy tokens must not acquire metadata'; END IF;
  BEGIN
    INSERT INTO auth_recovery_challenge VALUES(token_key,credential_key,1000,1901);
    RAISE EXCEPTION 'incorrect lifetime accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    INSERT INTO auth_recovery_challenge VALUES(token_key,credential_key,-1,899);
    RAISE EXCEPTION 'negative epoch accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    INSERT INTO auth_recovery_challenge VALUES(token_key,-1,1000,1900);
    RAISE EXCEPTION 'unknown credential accepted';
  EXCEPTION WHEN foreign_key_violation THEN NULL; END;
  INSERT INTO auth_recovery_challenge VALUES(token_key,credential_key,1000,1900);
  BEGIN
    INSERT INTO auth_recovery_challenge VALUES(token_key,credential_key,1000,1900);
    RAISE EXCEPTION 'duplicate metadata accepted';
  EXCEPTION WHEN unique_violation THEN NULL; END;
  BEGIN
    DELETE FROM user_credential WHERE id=credential_key;
    RAISE EXCEPTION 'credential with retained challenge deleted';
  EXCEPTION WHEN foreign_key_violation THEN NULL; END;
  DELETE FROM api_token WHERE id=token_key;
  IF EXISTS (SELECT 1 FROM auth_recovery_challenge WHERE api_token_id=token_key) THEN RAISE EXCEPTION 'token metadata did not cascade'; END IF;
  DELETE FROM user_credential WHERE id=credential_key;
END $$;
ROLLBACK;
SQL
echo 'Recovery migration: repeat application, legacy non-backfill, time/foreign-key/uniqueness constraints and deletion policies passed.'
