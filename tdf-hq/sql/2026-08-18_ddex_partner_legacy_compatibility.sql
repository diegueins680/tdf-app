BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';

DO $preflight$
BEGIN
    IF to_regclass('public.ddex_partner') IS NULL THEN
        RAISE EXCEPTION 'ddex_partner is required before applying legacy compatibility metadata';
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = 'ddex_partner'
          AND column_name = 'allowed_versions'
          AND data_type = 'ARRAY'
          AND udt_name = '_text'
          AND is_nullable = 'NO'
    ) THEN
        RAISE EXCEPTION 'ddex_partner.allowed_versions must be a non-null text array';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM ddex_partner
        WHERE cardinality(allowed_versions) <> 0
    ) THEN
        RAISE EXCEPTION
            'ddex_partner.allowed_versions contains legacy values; resolve the governed DDEX cutover before changing its default';
    END IF;
END
$preflight$;

-- The column is retained exclusively as reversible cutover evidence. New
-- policy is written through ddex_partner_standard_version, so omitted legacy
-- values must default to an empty array.
ALTER TABLE ddex_partner
    ALTER COLUMN allowed_versions SET DEFAULT ARRAY[]::text[];

COMMIT;
