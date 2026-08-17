-- Auditable, non-publishing assessment of legacy directory candidates.
-- It writes only run/mapping audit rows and never creates a public profile.
BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '5min';

INSERT INTO directory_backfill_run (
  id,run_code,source_revision,dry_run,status,report,correlation_id
) VALUES (
  directory_stable_uuid('directory-backfill','legacy-v1:dry-run'),
  'music-directory-legacy-v1','2026-08-14',TRUE,'running','{}'::jsonb,
  'music-directory-legacy-v1-dry-run'
)
ON CONFLICT (run_code,source_revision,dry_run) DO UPDATE SET
  status='running', completed_at=NULL, report='{}'::jsonb;

WITH run AS (
  SELECT id FROM directory_backfill_run
  WHERE run_code='music-directory-legacy-v1' AND source_revision='2026-08-14' AND dry_run
), candidates AS (
  SELECT 'artist_profile'::text source_table, artist.id::text source_id,
    artist.slug source_slug, 'planned'::text disposition,
    CASE WHEN credential.party_id IS NULL THEN 'profile-without-login' ELSE 'explicit-self-manager' END reason_code,
    jsonb_build_object('partyId',artist.artist_party_id,'willPublish',artist.slug IS NOT NULL AND trim(artist.slug)<>'') evidence
  FROM artist_profile artist
  LEFT JOIN LATERAL (SELECT party_id FROM user_credential WHERE party_id=artist.artist_party_id AND active LIMIT 1) credential ON TRUE
  UNION ALL
  SELECT 'band',band.id::text,NULL,'planned',
    CASE WHEN credential.party_id IS NULL THEN 'draft-without-login' ELSE 'draft-explicit-self-manager' END,
    jsonb_build_object('partyId',band.party_id,'willPublish',FALSE)
  FROM band
  LEFT JOIN LATERAL (SELECT party_id FROM user_credential WHERE party_id=band.party_id AND active LIMIT 1) credential ON TRUE
  UNION ALL
  SELECT 'social_artist_profile',social.id::text,NULL,
    CASE WHEN social.party_id ~ '^[0-9]+$' AND artist.artist_party_id IS NOT NULL THEN 'planned' ELSE 'ambiguous' END,
    CASE
      WHEN social.party_id IS NULL THEN 'missing-party-reference'
      WHEN social.party_id !~ '^[0-9]+$' THEN 'non-numeric-party-reference'
      WHEN party.id IS NULL THEN 'party-not-found'
      WHEN artist.artist_party_id IS NULL THEN 'no-canonical-artist-profile'
      ELSE 'link-to-canonical-artist'
    END,
    jsonb_build_object('hasPartyReference',social.party_id IS NOT NULL)
  FROM social_artist_profile social
  LEFT JOIN party ON social.party_id ~ '^[0-9]+$' AND party.id=social.party_id::bigint
  LEFT JOIN artist_profile artist ON artist.artist_party_id=party.id
  UNION ALL
  SELECT 'venue',venue.id::text,NULL,'skipped','safe-public-event-projection-only',
    jsonb_build_object('hasPublishedEvent',EXISTS (SELECT 1 FROM directory_public_event event WHERE event.venue_id=venue.id))
  FROM venue
  UNION ALL
  SELECT 'social_event',event.id::text,NULL,'skipped',
    CASE WHEN public.id IS NULL THEN 'not-public-listable' ELSE 'safe-public-event-projection' END,
    jsonb_build_object('publicListable',public.id IS NOT NULL)
  FROM social_event event LEFT JOIN directory_public_event public ON public.id=event.id
)
INSERT INTO directory_backfill_mapping (
  id,backfill_run_id,source_table,source_id,source_slug,disposition,reason_code,evidence
)
SELECT directory_stable_uuid('directory-backfill-dry-mapping',candidate.source_table||':'||candidate.source_id),
  run.id,candidate.source_table,candidate.source_id,candidate.source_slug,
  candidate.disposition,candidate.reason_code,candidate.evidence
FROM candidates candidate CROSS JOIN run
ON CONFLICT (backfill_run_id,source_table,source_id) DO UPDATE SET
  source_slug=EXCLUDED.source_slug, disposition=EXCLUDED.disposition,
  reason_code=EXCLUDED.reason_code, evidence=EXCLUDED.evidence;

UPDATE directory_backfill_run run SET
  status='completed',
  scanned_rows=counts.scanned,
  mapped_rows=counts.planned,
  created_rows=0,
  ambiguous_rows=counts.ambiguous,
  rejected_rows=counts.rejected,
  report=jsonb_build_object(
    'planned',counts.planned,'ambiguous',counts.ambiguous,'skipped',counts.skipped,
    'rejected',counts.rejected,'publishesRows',FALSE
  ),
  completed_at=now()
FROM (
  SELECT backfill_run_id,count(*) scanned,
    count(*) FILTER (WHERE disposition='planned') planned,
    count(*) FILTER (WHERE disposition='ambiguous') ambiguous,
    count(*) FILTER (WHERE disposition='skipped') skipped,
    count(*) FILTER (WHERE disposition='rejected') rejected
  FROM directory_backfill_mapping
  WHERE backfill_run_id=directory_stable_uuid('directory-backfill','legacy-v1:dry-run')
  GROUP BY backfill_run_id
) counts
WHERE run.id=counts.backfill_run_id;

COMMIT;

SELECT run_code,source_revision,dry_run,status,scanned_rows,mapped_rows,
  created_rows,ambiguous_rows,rejected_rows,report
FROM directory_backfill_run
WHERE id=directory_stable_uuid('directory-backfill','legacy-v1:dry-run');
