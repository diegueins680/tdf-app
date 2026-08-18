BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';

DO $preflight$
BEGIN
    IF to_regclass('public.ddex_partner') IS NULL THEN
        RAISE EXCEPTION 'ddex_partner is required before rolling back legacy compatibility metadata';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM ddex_partner
        WHERE cardinality(allowed_versions) <> 0
    ) THEN
        RAISE EXCEPTION
            'ddex_partner.allowed_versions contains post-cutover legacy values; rollback requires manual review';
    END IF;
END
$preflight$;

ALTER TABLE ddex_partner
    ALTER COLUMN allowed_versions SET DEFAULT ARRAY['4.3.2']::text[];

COMMIT;
