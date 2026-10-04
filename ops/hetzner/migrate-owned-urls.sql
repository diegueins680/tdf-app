\set ON_ERROR_STOP on
BEGIN;
-- Run only on the final restored target after fencing Fly writers. This is
-- deployment data maintenance, not a change to the production schema manifest.
-- Keep audit/cutover-source snapshots unchanged. Repeat execution is a no-op.
SELECT pg_advisory_xact_lock(hashtextextended('tdf-owned-url-host-migration', 0));

UPDATE asset
SET photo_url = regexp_replace(photo_url,
  '^https://tdf-hq[.]fly[.]dev/assets/serve/',
  'https://api.tdfrecords.net/assets/serve/')
WHERE photo_url LIKE 'https://tdf-hq.fly.dev/assets/serve/%';

UPDATE social_event
SET metadata = jsonb_set(metadata::jsonb, '{imageUrl}',
  to_jsonb(regexp_replace(metadata::jsonb ->> 'imageUrl',
    '^https://tdf-hq[.]fly[.]dev/assets/serve/',
    'https://api.tdfrecords.net/assets/serve/')))::text
WHERE metadata::jsonb ->> 'imageUrl' LIKE 'https://tdf-hq.fly.dev/assets/serve/%';

-- directory_public_rsvp_event is a view of the canonical event metadata.
-- Verify its derived images instead of trying to update the projection.
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM directory_public_rsvp_event
      WHERE image_url LIKE 'https://tdf-hq.fly.dev/assets/serve/%') THEN
    RAISE EXCEPTION 'Public RSVP images still reference the retired asset host';
  END IF;
END $$;

UPDATE radio_stream
SET stream_url = regexp_replace(stream_url,
  '^https://tdf-hq[.]fly[.]dev/live/',
  'https://api.tdfrecords.net/live/')
WHERE stream_url LIKE 'https://tdf-hq.fly.dev/live/%';
COMMIT;
