-- A balanced immutable entry set still loses its replay/reconciliation anchor
-- if a posted transaction's source or audit header can be rewritten. Requires
-- the registered posting, write-fence and namespace predecessors. Existing
-- status/reversal transitions remain governed by the posting validator.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE public.commerce_ledger_transaction IN SHARE ROW EXCLUSIVE MODE;

CREATE OR REPLACE FUNCTION public.commerce_protect_posted_ledger_header()
RETURNS trigger LANGUAGE plpgsql
SECURITY INVOKER
SET search_path = pg_catalog
AS $$
BEGIN
  IF OLD.status IN ('posted', 'reversed')
     AND (to_jsonb(NEW) - 'status' - 'reversal_of')
         IS DISTINCT FROM (to_jsonb(OLD) - 'status' - 'reversal_of') THEN
    RAISE EXCEPTION 'Posted ledger header is immutable; create a compensating transaction'
      USING ERRCODE = 'check_violation';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_ledger_header_immutable
  ON public.commerce_ledger_transaction;
CREATE TRIGGER trg_commerce_ledger_header_immutable
  BEFORE UPDATE ON public.commerce_ledger_transaction
  FOR EACH ROW EXECUTE FUNCTION public.commerce_protect_posted_ledger_header();

COMMIT;
-- Keep this guard through application rollback. This migration neither audits
-- nor rewrites historical source attribution, and does not grant DDL privileges.
