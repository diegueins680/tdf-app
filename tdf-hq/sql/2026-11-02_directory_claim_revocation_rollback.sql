-- Rollback of 2026-11-02_directory_claim_revocation. Refuses while revoked
-- claims exist: they are review history.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM public.directory_claim WHERE status = 'revoked') THEN
    RAISE EXCEPTION 'Revoked directory claims exist; rollback refused';
  END IF;
END $$;
DROP TRIGGER IF EXISTS trg_directory_claim_revocation_withdraws_grants ON public.directory_claim;
DROP FUNCTION IF EXISTS public.directory_claim_revocation_withdraws_grants();
ALTER TABLE public.directory_claim DROP CONSTRAINT IF EXISTS directory_claim_revocation_evidence;
ALTER TABLE public.directory_claim DROP CONSTRAINT IF EXISTS directory_claim_status_check;
ALTER TABLE public.directory_claim ADD CONSTRAINT directory_claim_status_check
  CHECK (status IN ('draft','submitted','under_review','more_evidence_requested','approved','rejected','withdrawn'));
COMMIT;
