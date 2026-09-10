-- Repair notification schema drift for environments that applied the original
-- access-request notification migration before its NOT NULL precondition was
-- enforced. Existing rows are preserved; an explicit failure prevents hiding
-- unknown notification semantics behind a synthetic type.

BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';

DO $repair$
BEGIN
    IF pg_catalog.to_regclass('public.notification') IS NULL THEN
        RAISE EXCEPTION 'public.notification is required for notification nullability repair';
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
    ) THEN
        RAISE EXCEPTION 'public.notification.notif_type must be text';
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
          AND NOT attribute.attnotnull
    ) THEN
        ALTER TABLE public.notification
            ALTER COLUMN notif_type SET NOT NULL;
    END IF;
END
$repair$;

DO $validation$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_attribute AS attribute
        WHERE attribute.attrelid = 'public.notification'::pg_catalog.regclass
          AND attribute.attname = 'notif_type'
          AND attribute.attnum > 0
          AND NOT attribute.attisdropped
          AND attribute.attnotnull
          AND attribute.atttypid = 'pg_catalog.text'::pg_catalog.regtype
          AND attribute.atttypmod = -1
    ) THEN
        RAISE EXCEPTION 'notification nullability repair did not establish non-null text notif_type';
    END IF;
END
$validation$;

COMMIT;
