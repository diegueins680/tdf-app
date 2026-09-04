-- Preserve idempotency evidence during rollback; remove only callable write
-- paths so an older application cannot accidentally invoke the new protocol.
\set ON_ERROR_STOP on
BEGIN;

DROP FUNCTION IF EXISTS reputation_save_personal_preference(BIGINT,TEXT,INTEGER,BOOLEAN,JSONB,TEXT,TEXT);
DROP FUNCTION IF EXISTS reputation_personal_preference_response(UUID);

COMMIT;
