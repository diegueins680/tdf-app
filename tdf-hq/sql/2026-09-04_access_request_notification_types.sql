-- Allow the notification types emitted by the feature access-request flow.
-- The feature tables and handlers were added after the original notification
-- constraint, whose four-type allowlist otherwise rolls back every request.

BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';

DO $migration$
DECLARE
    current_check TEXT;
    legacy_check CONSTANT TEXT :=
        'notif_type = ANY (ARRAY[''reaction_received''::text, ' ||
        '''post_trending''::text, ''weekly_top''::text, ' ||
        '''artist_liked''::text])';
    expected_check CONSTANT TEXT :=
        'notif_type = ANY (ARRAY[''reaction_received''::text, ' ||
        '''post_trending''::text, ''weekly_top''::text, ' ||
        '''artist_liked''::text, ''access_request_submitted''::text, ' ||
        '''access_request_review''::text, ''access_request_decided''::text])';
BEGIN
    IF pg_catalog.to_regclass('public.notification') IS NULL THEN
        RAISE EXCEPTION 'public.notification is required for access-request notification types';
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_attribute AS attribute
        WHERE attribute.attrelid = 'public.notification'::pg_catalog.regclass
          AND attribute.attname = 'notif_type'
          AND attribute.attnum > 0
          AND NOT attribute.attisdropped
          AND attribute.atttypid = 'pg_catalog.text'::pg_catalog.regtype
          AND attribute.atttypmod = -1
          AND attribute.attnotnull
    ) THEN
        RAISE EXCEPTION 'public.notification.notif_type must be non-null text';
    END IF;

    SELECT pg_catalog.pg_get_expr(constraint_row.conbin, constraint_row.conrelid, TRUE)
    INTO current_check
    FROM pg_catalog.pg_constraint AS constraint_row
    WHERE constraint_row.conrelid = 'public.notification'::pg_catalog.regclass
      AND constraint_row.conname = 'notification_notif_type_check'
      AND constraint_row.contype = 'c'
      AND constraint_row.conkey = ARRAY[3]::SMALLINT[]
      AND constraint_row.convalidated
      AND NOT constraint_row.connoinherit;

    IF current_check IS NULL THEN
        RAISE EXCEPTION 'public.notification has no valid canonical notif_type constraint';
    ELSIF current_check = expected_check THEN
        RETURN;
    ELSIF current_check <> legacy_check THEN
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
    expected_check CONSTANT TEXT :=
        'notif_type = ANY (ARRAY[''reaction_received''::text, ' ||
        '''post_trending''::text, ''weekly_top''::text, ' ||
        '''artist_liked''::text, ''access_request_submitted''::text, ' ||
        '''access_request_review''::text, ''access_request_decided''::text])';
BEGIN
    SELECT pg_catalog.pg_get_expr(constraint_row.conbin, constraint_row.conrelid, TRUE)
    INTO actual_check
    FROM pg_catalog.pg_constraint AS constraint_row
    WHERE constraint_row.conrelid = 'public.notification'::pg_catalog.regclass
      AND constraint_row.conname = 'notification_notif_type_check'
      AND constraint_row.contype = 'c'
      AND constraint_row.conkey = ARRAY[3]::SMALLINT[]
      AND constraint_row.convalidated
      AND NOT constraint_row.connoinherit;

    IF actual_check IS DISTINCT FROM expected_check THEN
        RAISE EXCEPTION 'Access-request notification constraint is invalid: %', actual_check;
    END IF;
END
$validation$;

COMMIT;
