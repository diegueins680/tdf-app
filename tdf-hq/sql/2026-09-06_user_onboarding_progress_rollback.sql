BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

DROP TABLE IF EXISTS user_onboarding_progress;

COMMIT;
