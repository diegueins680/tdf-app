-- Musician (participant) ticket tiers: paid tickets from a tier with this flag
-- list the buyer among the event's artists. Additive; every existing tier
-- keeps its current behaviour (false).
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

ALTER TABLE public.event_ticket_tier
  ADD COLUMN IF NOT EXISTS registers_participant BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
