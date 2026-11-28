-- Rollback for 2026-11-24_venue_coordinate_constraint.sql (EVT-VENUE-SEARCH-001).
-- Without the constraint a binary that requires it keeps the in-memory venue
-- search, so the order of these steps does not matter to readers.

BEGIN;

SET LOCAL lock_timeout = '10s';

ALTER TABLE public.venue DROP CONSTRAINT IF EXISTS venue_coordinates_in_range;
CREATE INDEX IF NOT EXISTS venue_search_out_of_range_idx
  ON public.venue (id)
  WHERE NOT (latitude BETWEEN -90 AND 90 AND longitude BETWEEN -180 AND 180);

COMMIT;
