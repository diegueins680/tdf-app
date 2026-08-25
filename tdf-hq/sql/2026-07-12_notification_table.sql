-- Migration: Restore the standalone in-app notification table
-- Date: 2026-07-12
--
-- This extracts only the notification schema from the broader 2026-05-25
-- engagement migration. It deliberately fails on partial or altered objects.

BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';
SET LOCAL idle_in_transaction_session_timeout = '30s';

DO $migration$
DECLARE
    object_count INTEGER;
    correctly_typed_count INTEGER;
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_class AS c
        JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace
        JOIN pg_catalog.pg_attribute AS a ON a.attrelid = c.oid
        WHERE n.nspname = 'public'
          AND c.relname = 'party'
          AND c.relkind IN ('r', 'p')
          AND a.attname = 'id'
          AND a.attnum > 0
          AND NOT a.attisdropped
          AND a.atttypid = 'pg_catalog.int8'::pg_catalog.regtype
          AND a.atttypmod = -1
    ) THEN
        RAISE EXCEPTION
            'Cannot create notification: public.party.id is missing or is not bigint';
    END IF;

    SELECT
        COUNT(*),
        COUNT(*) FILTER (
            WHERE (c.relname = 'notification' AND c.relkind = 'r')
               OR (c.relname = 'notification_id_seq' AND c.relkind = 'S')
               OR (c.relname IN ('notification_pkey', 'idx_notification_recipient')
                   AND c.relkind = 'i')
        )
    INTO object_count, correctly_typed_count
    FROM pg_catalog.pg_class AS c
    JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace
    WHERE n.nspname = 'public'
      AND c.relname IN (
          'notification',
          'notification_id_seq',
          'notification_pkey',
          'idx_notification_recipient'
      );

    IF object_count NOT IN (0, 4) OR correctly_typed_count <> object_count THEN
        RAISE EXCEPTION
            'Refusing notification migration: table, sequence, or indexes are partially present or have the wrong relation type';
    END IF;

    IF object_count = 0 THEN
        CREATE SEQUENCE public.notification_id_seq
            AS BIGINT
            START WITH 1
            INCREMENT BY 1
            NO MINVALUE
            NO MAXVALUE
            CACHE 1;

        CREATE TABLE public.notification (
            id BIGINT NOT NULL
                DEFAULT nextval('public.notification_id_seq'::pg_catalog.regclass),
            recipient_party_id BIGINT NOT NULL,
            notif_type TEXT NOT NULL,
            title TEXT NOT NULL,
            body TEXT NOT NULL,
            target_type TEXT,
            target_id BIGINT,
            is_read BOOLEAN NOT NULL DEFAULT FALSE,
            created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
            CONSTRAINT notification_pkey PRIMARY KEY (id),
            CONSTRAINT notification_recipient_party_id_fkey
                FOREIGN KEY (recipient_party_id) REFERENCES public.party(id),
            CONSTRAINT notification_notif_type_check CHECK (
                notif_type IN (
                    'reaction_received',
                    'post_trending',
                    'weekly_top',
                    'artist_liked'
                )
            )
        );

        ALTER SEQUENCE public.notification_id_seq
            OWNED BY public.notification.id;

        CREATE INDEX idx_notification_recipient
            ON public.notification (recipient_party_id, is_read, created_at DESC);
    END IF;
END
$migration$;

DO $validation$
DECLARE
    notification_oid OID := 'public.notification'::pg_catalog.regclass;
    notification_sequence_oid OID := 'public.notification_id_seq'::pg_catalog.regclass;
    notification_index_oid OID := 'public.idx_notification_recipient'::pg_catalog.regclass;
    notification_pkey_oid OID := 'public.notification_pkey'::pg_catalog.regclass;
    party_oid OID := 'public.party'::pg_catalog.regclass;
    party_id_attnum SMALLINT;
    actual_default TEXT;
    actual_check TEXT;
BEGIN
    SELECT a.attnum
    INTO STRICT party_id_attnum
    FROM pg_catalog.pg_attribute AS a
    WHERE a.attrelid = party_oid
      AND a.attname = 'id'
      AND a.attnum > 0
      AND NOT a.attisdropped;

    IF (
        SELECT COUNT(*)
        FROM pg_catalog.pg_attribute AS a
        WHERE a.attrelid = notification_oid
          AND a.attnum > 0
          AND NOT a.attisdropped
    ) <> 9 OR EXISTS (
        SELECT 1
        FROM (
            VALUES
                (1, 'id',                 'pg_catalog.int8'::pg_catalog.regtype,       -1, TRUE,  FALSE),
                (2, 'recipient_party_id', 'pg_catalog.int8'::pg_catalog.regtype,       -1, TRUE,  FALSE),
                (3, 'notif_type',         'pg_catalog.text'::pg_catalog.regtype,       -1, TRUE,  FALSE),
                (4, 'title',              'pg_catalog.text'::pg_catalog.regtype,       -1, TRUE,  FALSE),
                (5, 'body',               'pg_catalog.text'::pg_catalog.regtype,       -1, TRUE,  FALSE),
                (6, 'target_type',        'pg_catalog.text'::pg_catalog.regtype,       -1, FALSE, FALSE),
                (7, 'target_id',          'pg_catalog.int8'::pg_catalog.regtype,       -1, FALSE, FALSE),
                (8, 'is_read',            'pg_catalog.bool'::pg_catalog.regtype,       -1, TRUE,  FALSE),
                (9, 'created_at',          'pg_catalog.timestamptz'::pg_catalog.regtype, -1, TRUE, FALSE)
        ) AS expected(attnum, attname, atttypid, atttypmod, attnotnull, attisdropped)
        LEFT JOIN pg_catalog.pg_attribute AS actual
          ON actual.attrelid = notification_oid
         AND actual.attnum = expected.attnum
        WHERE actual.attname IS DISTINCT FROM expected.attname
           OR actual.atttypid IS DISTINCT FROM expected.atttypid
           OR actual.atttypmod IS DISTINCT FROM expected.atttypmod
           OR actual.attnotnull IS DISTINCT FROM expected.attnotnull
           OR actual.attisdropped IS DISTINCT FROM expected.attisdropped
           OR actual.attidentity <> ''
           OR actual.attgenerated <> ''
    ) THEN
        RAISE EXCEPTION 'Existing public.notification columns do not match the expected schema';
    END IF;

    IF (
        SELECT COUNT(*)
        FROM pg_catalog.pg_attrdef AS d
        WHERE d.adrelid = notification_oid
    ) <> 3 THEN
        RAISE EXCEPTION 'Existing public.notification has unexpected column defaults';
    END IF;

    SELECT pg_catalog.pg_get_expr(d.adbin, d.adrelid)
    INTO actual_default
    FROM pg_catalog.pg_attrdef AS d
    WHERE d.adrelid = notification_oid
      AND d.adnum = 1;

    IF actual_default IS NULL OR actual_default NOT IN (
        'nextval(''notification_id_seq''::regclass)',
        'nextval(''public.notification_id_seq''::regclass)'
    ) THEN
        RAISE EXCEPTION 'Existing public.notification.id has an unexpected default: %', actual_default;
    END IF;

    SELECT pg_catalog.pg_get_expr(d.adbin, d.adrelid)
    INTO actual_default
    FROM pg_catalog.pg_attrdef AS d
    WHERE d.adrelid = notification_oid
      AND d.adnum = 8;

    IF actual_default IS DISTINCT FROM 'false' THEN
        RAISE EXCEPTION 'Existing public.notification.is_read has an unexpected default: %', actual_default;
    END IF;

    SELECT pg_catalog.pg_get_expr(d.adbin, d.adrelid)
    INTO actual_default
    FROM pg_catalog.pg_attrdef AS d
    WHERE d.adrelid = notification_oid
      AND d.adnum = 9;

    IF actual_default IS DISTINCT FROM 'now()' THEN
        RAISE EXCEPTION 'Existing public.notification.created_at has an unexpected default: %', actual_default;
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_sequence AS s
        WHERE s.seqrelid = notification_sequence_oid
          AND s.seqtypid = 'pg_catalog.int8'::pg_catalog.regtype
          AND s.seqstart = 1
          AND s.seqincrement = 1
          AND s.seqmin = 1
          AND s.seqmax = 9223372036854775807
          AND s.seqcache = 1
          AND NOT s.seqcycle
    ) OR pg_catalog.to_regclass(
        pg_catalog.pg_get_serial_sequence('public.notification', 'id')
    ) IS DISTINCT FROM notification_sequence_oid THEN
        RAISE EXCEPTION 'Existing public.notification_id_seq does not match the expected bigint sequence';
    END IF;

    IF (
        SELECT COUNT(*)
        FROM pg_catalog.pg_constraint AS c
        WHERE c.conrelid = notification_oid
    ) <> 3 THEN
        RAISE EXCEPTION 'Existing public.notification has unexpected constraints';
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_constraint AS c
        WHERE c.conrelid = notification_oid
          AND c.conname = 'notification_pkey'
          AND c.contype = 'p'
          AND c.conkey = ARRAY[1]::SMALLINT[]
          AND c.conindid = notification_pkey_oid
          AND NOT c.condeferrable
          AND NOT c.condeferred
          AND c.convalidated
    ) THEN
        RAISE EXCEPTION 'Existing public.notification primary key does not match the expected constraint';
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_constraint AS c
        WHERE c.conrelid = notification_oid
          AND c.conname = 'notification_recipient_party_id_fkey'
          AND c.contype = 'f'
          AND c.confrelid = party_oid
          AND c.conkey = ARRAY[2]::SMALLINT[]
          AND c.confkey = ARRAY[party_id_attnum]::SMALLINT[]
          AND c.confupdtype = 'a'
          AND c.confdeltype = 'a'
          AND c.confmatchtype = 's'
          AND NOT c.condeferrable
          AND NOT c.condeferred
          AND c.convalidated
    ) THEN
        RAISE EXCEPTION 'Existing public.notification recipient foreign key does not match the expected constraint';
    END IF;

    SELECT pg_catalog.pg_get_expr(c.conbin, c.conrelid, TRUE)
    INTO actual_check
    FROM pg_catalog.pg_constraint AS c
    WHERE c.conrelid = notification_oid
      AND c.conname = 'notification_notif_type_check'
      AND c.contype = 'c'
      AND c.conkey = ARRAY[3]::SMALLINT[]
      AND c.convalidated
      AND NOT c.connoinherit;

    IF actual_check IS DISTINCT FROM
        'notif_type = ANY (ARRAY[''reaction_received''::text, ''post_trending''::text, ''weekly_top''::text, ''artist_liked''::text])'
    THEN
        RAISE EXCEPTION 'Existing public.notification notif_type check is unexpected: %', actual_check;
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_index AS i
        JOIN pg_catalog.pg_class AS index_class ON index_class.oid = i.indexrelid
        JOIN pg_catalog.pg_am AS access_method ON access_method.oid = index_class.relam
        WHERE i.indexrelid = notification_index_oid
          AND i.indrelid = notification_oid
          AND access_method.amname = 'btree'
          AND NOT i.indisunique
          AND NOT i.indisprimary
          AND i.indisvalid
          AND i.indisready
          AND i.indislive
          AND i.indnkeyatts = 3
          AND i.indnatts = 3
          AND i.indkey::TEXT = '2 8 9'
          AND i.indoption::TEXT = '0 0 3'
          AND i.indexprs IS NULL
          AND i.indpred IS NULL
    ) THEN
        RAISE EXCEPTION 'Existing public.idx_notification_recipient does not match the expected index';
    END IF;
END
$validation$;

COMMIT;

-- Verification (run in any SQL client; every result should be true):
-- SELECT
--     pg_catalog.to_regclass('public.notification') IS NOT NULL AS table_present,
--     pg_catalog.to_regclass('public.notification_id_seq') IS NOT NULL AS sequence_present,
--     pg_catalog.to_regclass('public.notification_pkey') IS NOT NULL AS primary_key_present,
--     pg_catalog.to_regclass('public.idx_notification_recipient') IS NOT NULL AS index_present,
--     (SELECT COUNT(*) = 9 FROM information_schema.columns
--       WHERE table_schema = 'public' AND table_name = 'notification') AS nine_columns;
