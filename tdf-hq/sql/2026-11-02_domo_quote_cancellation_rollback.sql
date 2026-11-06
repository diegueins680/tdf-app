-- Rollback of 2026-11-02_domo_quote_cancellation. Refuses when cancellation
-- records exist: they are financial history.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$
BEGIN
  IF to_regclass('public.domo_quote_cancellation') IS NOT NULL
     AND EXISTS (SELECT 1 FROM public.domo_quote_cancellation) THEN
    RAISE EXCEPTION 'Domo cancellation records exist; rollback refused';
  END IF;
END $$;
DROP VIEW IF EXISTS public.domo_quote_cancellation_open_refund;
DROP TRIGGER IF EXISTS trg_domo_quote_cancellation_required ON public.domo_event_quote_runtime;
DROP FUNCTION IF EXISTS public.domo_quote_cancellation_required();
DROP TRIGGER IF EXISTS trg_domo_quote_cancellation_validate ON public.domo_quote_cancellation;
DROP FUNCTION IF EXISTS public.domo_quote_cancellation_validate();
DROP TABLE IF EXISTS public.domo_quote_cancellation;
COMMIT;
