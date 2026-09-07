-- Restore the original four-type notification allowlist only when doing so
-- cannot orphan notifications already emitted by the access-request flow.

BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';

DO $rollback$
DECLARE
    current_check TEXT;
    legacy_check CONSTANT TEXT :=
        'notif_type = ANY (ARRAY[''reaction_received''::text, ' ||
        '''post_trending''::text, ''weekly_top''::text, ' ||
        '''artist_liked''::text])';
    expanded_check CONSTANT TEXT :=
        'notif_type = ANY (ARRAY[''reaction_received''::text, ' ||
        '''post_trending''::text, ''weekly_top''::text, ' ||
        '''artist_liked''::text, ''access_request_submitted''::text, ' ||
        '''access_request_review''::text, ''access_request_decided''::text])';
BEGIN
    IF pg_catalog.to_regclass('public.notification') IS NULL THEN
        RAISE EXCEPTION 'public.notification is required for notification-type rollback';
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
    ELSIF current_check = legacy_check THEN
        RETURN;
    ELSIF current_check <> expanded_check THEN
        RAISE EXCEPTION
            'Refusing to replace unexpected notification_notif_type_check: %', current_check;
    END IF;

    IF EXISTS (
        SELECT 1
        FROM public.notification
        WHERE notif_type IN (
            'access_request_submitted',
            'access_request_review',
            'access_request_decided'
        )
    ) THEN
        RAISE EXCEPTION
            'Access-request notifications exist; preserve them or restore a database snapshot';
    END IF;

    ALTER TABLE public.notification
        DROP CONSTRAINT notification_notif_type_check;
    ALTER TABLE public.notification
        ADD CONSTRAINT notification_notif_type_check CHECK (
            notif_type IN (
                'reaction_received',
                'post_trending',
                'weekly_top',
                'artist_liked'
            )
        ) NOT VALID;
    ALTER TABLE public.notification
        VALIDATE CONSTRAINT notification_notif_type_check;
END
$rollback$;

COMMIT;
