-- EVT-VENUE-SEARCH-001 (formal/system/venue-search.md): stored venue
-- coordinates are in range.
--
-- Every writer already refuses a latitude outside -90..90, a longitude outside
-- -180..180 and any value that is not finite (the venue create and update
-- handler, event research materialisation and event discovery). This makes the
-- database refuse them too, so the SQL venue search no longer needs a branch
-- for such rows. NULL stays allowed. In PostgreSQL NaN sorts above every
-- number, so NaN and both infinities fail the BETWEEN tests.
--
-- Refuses, without changing anything, if any stored row is out of range; on
-- 2026-11-23 production held 45 venues and none was. Only after the
-- constraint exists does the partial index for out-of-range rows have nothing
-- to serve, so it is dropped.

BEGIN;

SET LOCAL lock_timeout = '10s';

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM public.venue
    WHERE NOT (latitude BETWEEN -90 AND 90) OR NOT (longitude BETWEEN -180 AND 180)
  ) THEN
    RAISE EXCEPTION 'venue holds out-of-range or non-finite coordinates; venue coordinate constraint refused';
  END IF;

  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname = 'venue_coordinates_in_range' AND conrelid = 'public.venue'::regclass
  ) THEN
    ALTER TABLE public.venue
      ADD CONSTRAINT venue_coordinates_in_range
      CHECK ((latitude IS NULL OR latitude BETWEEN -90 AND 90)
         AND (longitude IS NULL OR longitude BETWEEN -180 AND 180));
  END IF;
END
$$;

DROP INDEX IF EXISTS public.venue_search_out_of_range_idx;

COMMIT;
