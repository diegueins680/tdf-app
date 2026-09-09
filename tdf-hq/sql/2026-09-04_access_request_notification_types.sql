-- Allow the notification types emitted by the feature access-request flow.
-- The feature tables and handlers were added after the original notification
-- constraint, whose four-type allowlist otherwise rolls back every request.

BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';

DO $migration$
DECLARE
    current_check TEXT;
    notif_type_attnum SMALLINT;
    legacy_check CONSTANT TEXT :=
        'notif_type = ANY (ARRAY[''reaction_received''::text, ' ||
        '''post_trending''::text, ''weekly_top''::text, ' ||
        '''artist_liked''::text])';
    legacy_varchar_check CONSTANT TEXT :=
        'notif_type = ANY (ARRAY[''reaction_received''::character varying::text, ' ||
        '''post_trending''::character varying::text, ' ||
        '''weekly_top''::character varying::text, ' ||
        '''artist_liked''::character varying::text])';
    expected_check CONSTANT TEXT :=
        'notif_type = ANY (ARRAY[''reaction_received''::text, ' ||
        '''post_trending''::text, ''weekly_top''::text, ' ||
        '''artist_liked''::text, ''access_request_submitted''::text, ' ||
        '''access_request_review''::text, ''access_request_decided''::text])';
BEGIN
    IF pg_catalog.to_regclass('public.notification') IS NULL THEN
        RAISE EXCEPTION 'public.notification is required for access-request notification types';
    END IF;

    SELECT attribute.attnum
    INTO notif_type_attnum
    FROM pg_catalog.pg_attribute AS attribute
    WHERE attribute.attrelid = 'public.notification'::pg_catalog.regclass
      AND attribute.attname = 'notif_type'
      AND attribute.attnum > 0
      AND NOT attribute.attisdropped
      AND (
          (
              attribute.atttypid = 'pg_catalog.text'::pg_catalog.regtype
              AND attribute.atttypmod = -1
          )
          OR attribute.atttypid = 'pg_catalog.varchar'::pg_catalog.regtype
      );

    IF notif_type_attnum IS NULL THEN
        RAISE EXCEPTION
            'public.notification.notif_type must be text or character varying';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM public.notification
        WHERE notif_type IS NULL
    ) THEN
        RAISE EXCEPTION
            'public.notification.notif_type contains NULL values; repair data before enforcing NOT NULL';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM pg_catalog.pg_attribute AS attribute
        WHERE attribute.attrelid = 'public.notification'::pg_catalog.regclass
          AND attribute.attname = 'notif_type'
          AND attribute.attnum > 0
          AND NOT attribute.attisdropped
          AND attribute.atttypid = 'pg_catalog.varchar'::pg_catalog.regtype
    ) THEN
        -- The pre-ledger staging baseline used unconstrained varchar. This
        -- widening conversion is lossless and preserves every producer's
        -- notification values.
        ALTER TABLE public.notification
            ALTER COLUMN notif_type TYPE TEXT
            USING notif_type::TEXT;
    END IF;

    IF EXISTS (
        SELECT 1
        FROM pg_catalog.pg_attribute AS attribute
        WHERE attribute.attrelid = 'public.notification'::pg_catalog.regclass
          AND attribute.attname = 'notif_type'
          AND attribute.attnum > 0
          AND NOT attribute.attisdropped
          AND NOT attribute.attnotnull
    ) THEN
        ALTER TABLE public.notification
            ALTER COLUMN notif_type SET NOT NULL;
    END IF;

    SELECT pg_catalog.pg_get_expr(constraint_row.conbin, constraint_row.conrelid, TRUE)
    INTO current_check
    FROM pg_catalog.pg_constraint AS constraint_row
    WHERE constraint_row.conrelid = 'public.notification'::pg_catalog.regclass
      AND constraint_row.conname = 'notification_notif_type_check'
      AND constraint_row.contype = 'c'
      AND constraint_row.conkey = ARRAY[notif_type_attnum]::SMALLINT[]
      AND constraint_row.convalidated
      AND NOT constraint_row.connoinherit;

    IF current_check IS NULL THEN
        IF EXISTS (
            SELECT 1
            FROM pg_catalog.pg_constraint AS constraint_row
            WHERE constraint_row.conrelid = 'public.notification'::pg_catalog.regclass
              AND (
                  constraint_row.conname = 'notification_notif_type_check'
                  OR (
                      constraint_row.contype = 'c'
                      AND notif_type_attnum = ANY (constraint_row.conkey)
                  )
              )
        ) THEN
            RAISE EXCEPTION
                'public.notification has a conflicting non-canonical notif_type constraint';
        END IF;
        -- Older installations intentionally had no allowlist. Do not invent
        -- one: several independently deployed notification producers have
        -- valid persisted types outside this feature's original seven values.
        RETURN;
    ELSIF current_check = expected_check THEN
        RETURN;
    ELSIF current_check NOT IN (legacy_check, legacy_varchar_check) THEN
        RAISE EXCEPTION
            'Refusing to replace unexpected notification_notif_type_check: %', current_check;
    END IF;

    ALTER TABLE public.notification
        DROP CONSTRAINT notification_notif_type_check;
    ALTER TABLE public.notification
        ADD CONSTRAINT notification_notif_type_check CHECK (
            notif_type IN (
                'reaction_received',
                'post_trending',
                'weekly_top',
                'artist_liked',
                'access_request_submitted',
                'access_request_review',
                'access_request_decided'
            )
        ) NOT VALID;
    ALTER TABLE public.notification
        VALIDATE CONSTRAINT notification_notif_type_check;
END
$migration$;

DO $validation$
DECLARE
    actual_check TEXT;
    notif_type_attnum SMALLINT;
    expected_check CONSTANT TEXT :=
        'notif_type = ANY (ARRAY[''reaction_received''::text, ' ||
        '''post_trending''::text, ''weekly_top''::text, ' ||
        '''artist_liked''::text, ''access_request_submitted''::text, ' ||
        '''access_request_review''::text, ''access_request_decided''::text])';
BEGIN
    SELECT attribute.attnum
    INTO notif_type_attnum
    FROM pg_catalog.pg_attribute AS attribute
    WHERE attribute.attrelid = 'public.notification'::pg_catalog.regclass
      AND attribute.attname = 'notif_type'
      AND attribute.attnum > 0
      AND NOT attribute.attisdropped;

    SELECT pg_catalog.pg_get_expr(constraint_row.conbin, constraint_row.conrelid, TRUE)
    INTO actual_check
    FROM pg_catalog.pg_constraint AS constraint_row
    WHERE constraint_row.conrelid = 'public.notification'::pg_catalog.regclass
      AND constraint_row.conname = 'notification_notif_type_check'
      AND constraint_row.contype = 'c'
      AND constraint_row.conkey = ARRAY[notif_type_attnum]::SMALLINT[]
      AND constraint_row.convalidated
      AND NOT constraint_row.connoinherit;

    IF actual_check IS NULL AND EXISTS (
        SELECT 1
        FROM pg_catalog.pg_constraint AS constraint_row
        WHERE constraint_row.conrelid = 'public.notification'::pg_catalog.regclass
          AND (
              constraint_row.conname = 'notification_notif_type_check'
              OR (
                  constraint_row.contype = 'c'
                  AND notif_type_attnum = ANY (constraint_row.conkey)
              )
          )
    ) THEN
        RAISE EXCEPTION
            'Access-request notification constraint is malformed or non-canonical';
    ELSIF actual_check IS NOT NULL AND actual_check IS DISTINCT FROM expected_check THEN
        RAISE EXCEPTION 'Access-request notification constraint is invalid: %', actual_check;
    END IF;
END
$validation$;

COMMIT;
