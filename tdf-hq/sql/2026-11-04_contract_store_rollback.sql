-- Rollback of 2026-11-04_contract_store. Refuses while contracts exist, because
-- dropping the tables would discard them; export them first.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$
BEGIN
  IF to_regclass('public.contract') IS NOT NULL AND EXISTS (SELECT 1 FROM contract) THEN
    RAISE EXCEPTION 'Refusing to drop contract: % stored contracts would be lost', (SELECT count(*) FROM contract);
  END IF;
END $$;
DROP TABLE IF EXISTS contract_delivery;
DROP TABLE IF EXISTS contract;
COMMIT;
