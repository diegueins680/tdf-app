-- AUTHORITY-097 (audit Q3): an approved directory claim can be revoked by a
-- distinct administrator with a stated reason. Revocation is terminal and
-- withdraws every manager grant the claim created; the database refuses a
-- revoked claim that still carries an active grant.
BEGIN;
SET LOCAL lock_timeout = '10s';

ALTER TABLE public.directory_claim DROP CONSTRAINT IF EXISTS directory_claim_status_check;
ALTER TABLE public.directory_claim ADD CONSTRAINT directory_claim_status_check
  CHECK (status IN ('draft','submitted','under_review','more_evidence_requested','approved','rejected','withdrawn','revoked'));

ALTER TABLE public.directory_claim DROP CONSTRAINT IF EXISTS directory_claim_revocation_evidence;
ALTER TABLE public.directory_claim ADD CONSTRAINT directory_claim_revocation_evidence
  CHECK (status <> 'revoked'
         OR (reviewer_party_id IS NOT NULL AND reviewed_at IS NOT NULL
             AND NULLIF(btrim(reviewer_notes), '') IS NOT NULL));

CREATE OR REPLACE FUNCTION public.directory_claim_revocation_withdraws_grants()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.status = 'revoked' AND (TG_OP = 'INSERT' OR OLD.status IS DISTINCT FROM 'revoked')
     AND EXISTS (SELECT 1 FROM public.directory_profile_manager m
                 WHERE m.source_claim_id = NEW.id AND m.active) THEN
    RAISE EXCEPTION 'A revoked directory claim cannot keep an active manager grant (AUTHORITY-097)'
      USING ERRCODE = 'check_violation';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_directory_claim_revocation_withdraws_grants ON public.directory_claim;
CREATE TRIGGER trg_directory_claim_revocation_withdraws_grants
  AFTER INSERT OR UPDATE OF status ON public.directory_claim
  FOR EACH ROW EXECUTE FUNCTION public.directory_claim_revocation_withdraws_grants();

COMMIT;
