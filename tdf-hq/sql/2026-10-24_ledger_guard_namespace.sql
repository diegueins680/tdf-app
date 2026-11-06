-- Pin relation lookup inside ledger triggers. A non-owner with TEMP privilege
-- can otherwise shadow the real entries/parent with session-local tables.
-- Keep the historical function bodies and their physical-write fences intact.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE public.commerce_ledger_transaction, public.commerce_ledger_entry
  IN SHARE ROW EXCLUSIVE MODE;

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM public.commerce_ledger_transaction txn
    WHERE txn.status IN ('posted', 'reversed')
      AND (NOT EXISTS (SELECT 1 FROM public.commerce_ledger_entry WHERE transaction_id = txn.id)
           OR EXISTS (SELECT 1 FROM public.commerce_ledger_entry WHERE transaction_id = txn.id
                      GROUP BY currency HAVING SUM(amount_minor::numeric) <> 0))
  ) THEN
    RAISE EXCEPTION 'Existing posted ledger is empty or unbalanced; reconcile before namespace migration'
      USING ERRCODE = 'check_violation';
  END IF;
  IF EXISTS (
    SELECT 1 FROM public.commerce_ledger_transaction txn
    LEFT JOIN public.commerce_ledger_transaction reverser ON reverser.id = txn.reversal_of
    WHERE txn.status = 'reversed'
      AND (txn.reversal_of = txn.id OR reverser.status IS DISTINCT FROM 'posted')
  ) THEN
    RAISE EXCEPTION 'Existing reversed ledger requires a distinct posted reverser; reconcile before namespace migration'
      USING ERRCODE = 'check_violation';
  END IF;
END $$;

-- pg_temp must be explicit and last: omitting it would implicitly search
-- temporary relations first. public must remain a trusted, non-writable schema
-- for the runtime role. This does not elevate privileges (SECURITY INVOKER).
ALTER FUNCTION public.commerce_validate_ledger_posting()
  SET search_path = pg_catalog, public, pg_temp;
ALTER FUNCTION public.commerce_protect_posted_ledger()
  SET search_path = pg_catalog, public, pg_temp;

COMMIT;

-- Retain this integrity boundary through application rollback.
