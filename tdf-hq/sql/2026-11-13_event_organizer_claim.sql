-- Reviewed claims on events that have no organizer. Additive: creates no
-- events, accounts or claims and assigns no organizer.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

CREATE TABLE IF NOT EXISTS public.event_organizer_claim (
  id BIGSERIAL PRIMARY KEY,
  event_id BIGINT NOT NULL REFERENCES public.social_event(id) ON DELETE CASCADE,
  claimant_party_id TEXT NOT NULL,
  evidence TEXT NOT NULL,
  status TEXT NOT NULL,
  submitted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  reviewer_party_id TEXT,
  reviewer_notes TEXT,
  reviewed_at TIMESTAMPTZ,
  CONSTRAINT event_organizer_claim_claimant_check CHECK (claimant_party_id ~ '^[1-9][0-9]*$'),
  CONSTRAINT event_organizer_claim_status_check
    CHECK (status IN ('submitted', 'approved', 'rejected', 'superseded')),
  CONSTRAINT event_organizer_claim_evidence_check
    CHECK (char_length(btrim(evidence)) BETWEEN 20 AND 2000),
  CONSTRAINT event_organizer_claim_open_check
    CHECK ((status = 'submitted') = (reviewed_at IS NULL)),
  -- A decision is always recorded with a reviewer other than the claimant.
  CONSTRAINT event_organizer_claim_reviewer_check
    CHECK (status NOT IN ('approved', 'rejected')
      OR (reviewer_party_id IS NOT NULL AND reviewer_party_id <> claimant_party_id))
);

-- One open claim per account and event.
CREATE UNIQUE INDEX IF NOT EXISTS event_organizer_claim_open_idx
  ON public.event_organizer_claim (event_id, claimant_party_id)
  WHERE status = 'submitted';

CREATE INDEX IF NOT EXISTS event_organizer_claim_queue_idx
  ON public.event_organizer_claim (status, submitted_at);

COMMIT;
