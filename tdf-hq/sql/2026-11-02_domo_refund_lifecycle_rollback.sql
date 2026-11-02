-- Refuse removal of evidence or weakening the outstanding-obligation view.
BEGIN;
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM public.domo_manual_refund_evidence)
    OR EXISTS(SELECT 1 FROM public.domo_quote_cancellation WHERE refund_due_minor>0) THEN
   RAISE EXCEPTION 'Domo refund lifecycle rollback refuses financial history';
 END IF;
END $$;
DROP TRIGGER IF EXISTS trg_domo_refund_close_reconciliation ON public.commerce_refund;
DROP FUNCTION IF EXISTS public.domo_refund_close_reconciliation();
DROP TABLE IF EXISTS public.domo_manual_refund_evidence;
DROP FUNCTION IF EXISTS public.domo_manual_refund_validate();
-- Keep the stronger open-obligation view and operator-owned flags intact.
COMMIT;
