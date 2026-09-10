-- Restore the directory rate-limit allowlist used before public Party selector
-- throttling. Refuse to discard live selector counters during rollback.

BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';

DO $rollback$
BEGIN
    IF pg_catalog.to_regclass('public.directory_rate_limit') IS NULL THEN
        RAISE EXCEPTION 'public.directory_rate_limit is required for Party selector rate-limit rollback';
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_constraint
        WHERE conrelid = 'public.directory_rate_limit'::pg_catalog.regclass
          AND conname = 'directory_rate_limit_scope_check'
          AND contype = 'c'
          AND convalidated
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%search%'
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%profile_create%'
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%classified_publish%'
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%application%'
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%invitation%'
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%contact%'
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%report%'
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%review%'
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%experience-review%'
    ) THEN
        RAISE EXCEPTION 'directory_rate_limit_scope_check is missing or invalid';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM public.directory_rate_limit
        WHERE scope IN (
            'party_selector:event_invitation',
            'party_selector:social_connection'
        )
    ) THEN
        RAISE EXCEPTION 'Party selector rate-limit rows exist; retain compatibility or restore a snapshot';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM public.directory_rate_limit
        WHERE scope NOT IN (
            'search', 'profile_create', 'classified_publish', 'application',
            'invitation', 'contact', 'report', 'review', 'experience-review'
        )
    ) THEN
        RAISE EXCEPTION 'directory_rate_limit contains an unsupported rollback scope';
    END IF;

    ALTER TABLE public.directory_rate_limit
        DROP CONSTRAINT directory_rate_limit_scope_check;
    ALTER TABLE public.directory_rate_limit
        ADD CONSTRAINT directory_rate_limit_scope_check
        CHECK (scope IN (
            'search', 'profile_create', 'classified_publish', 'application',
            'invitation', 'contact', 'report', 'review', 'experience-review'
        ));
END
$rollback$;

COMMIT;
