-- Allow the authenticated public Party selectors to use the shared durable
-- rate-limit table. The selector runtime introduced these two isolated scopes
-- after the directory table's closed scope allowlist was already deployed.

BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';

DO $repair$
BEGIN
    IF pg_catalog.to_regclass('public.directory_rate_limit') IS NULL THEN
        RAISE EXCEPTION 'public.directory_rate_limit is required for Party selector rate limits';
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_attribute AS attribute
        WHERE attribute.attrelid = 'public.directory_rate_limit'::pg_catalog.regclass
          AND attribute.attname = 'scope'
          AND attribute.attnum > 0
          AND NOT attribute.attisdropped
          AND attribute.atttypid = 'pg_catalog.text'::pg_catalog.regtype
          AND attribute.atttypmod = -1
          AND attribute.attnotnull
    ) THEN
        RAISE EXCEPTION 'public.directory_rate_limit.scope must be non-null text';
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
        WHERE scope NOT IN (
            'search', 'profile_create', 'classified_publish', 'application',
            'invitation', 'contact', 'report', 'review', 'experience-review',
            'party_selector:event_invitation', 'party_selector:social_connection'
        )
    ) THEN
        RAISE EXCEPTION 'directory_rate_limit contains an unsupported scope';
    END IF;

    ALTER TABLE public.directory_rate_limit
        DROP CONSTRAINT directory_rate_limit_scope_check;
    ALTER TABLE public.directory_rate_limit
        ADD CONSTRAINT directory_rate_limit_scope_check
        CHECK (scope IN (
            'search', 'profile_create', 'classified_publish', 'application',
            'invitation', 'contact', 'report', 'review', 'experience-review',
            'party_selector:event_invitation', 'party_selector:social_connection'
        ));
END
$repair$;

DO $validation$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_constraint
        WHERE conrelid = 'public.directory_rate_limit'::pg_catalog.regclass
          AND conname = 'directory_rate_limit_scope_check'
          AND contype = 'c'
          AND convalidated
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%party_selector:event_invitation%'
          AND pg_catalog.pg_get_constraintdef(oid) LIKE '%party_selector:social_connection%'
    ) THEN
        RAISE EXCEPTION 'Party selector rate-limit scopes were not established';
    END IF;
END
$validation$;

COMMIT;
