-- Deterministic legacy reconciliation. Run the dry-run first and review every
-- ambiguous mapping. Existing legacy rows remain untouched.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

INSERT INTO directory_backfill_run (
  id,run_code,source_revision,dry_run,status,report,correlation_id
) VALUES (
  directory_stable_uuid('directory-backfill','legacy-v1:apply'),
  'music-directory-legacy-v1','2026-08-14',FALSE,'running','{}'::jsonb,
  'music-directory-legacy-v1-apply'
)
ON CONFLICT (run_code,source_revision,dry_run) DO UPDATE SET
  status='running', completed_at=NULL;

-- Canonical artist profiles preserve compatible slugs and existing public
-- fields. Duplicate/invalid slugs receive a stable fallback without changing
-- the legacy URL or row.
WITH ranked AS (
  SELECT artist.*,
    lower(trim(artist.slug)) ~ '^[a-z0-9][a-z0-9-]{1,119}$' slug_valid,
    row_number() OVER (PARTITION BY lower(trim(artist.slug)) ORDER BY artist.id) slug_rank
  FROM artist_profile artist
), prepared AS (
  SELECT ranked.*,
    CASE WHEN slug_valid AND slug_rank=1 THEN lower(trim(slug)) ELSE 'artist-'||artist_party_id::text END target_slug
  FROM ranked
)
INSERT INTO directory_profile (
  id,subject_party_id,profile_kind,public_name,slug,bio,portfolio,links,
  profile_status,visibility,moderation_status,completeness_score,
  created_at,updated_at,published_at
)
SELECT directory_stable_uuid('directory-profile:artist',artist.id::text),
  artist.artist_party_id,'artist',party.display_name,artist.target_slug,artist.bio,
  CASE WHEN artist.hero_image_url IS NULL THEN '[]'::jsonb
       ELSE jsonb_build_array(jsonb_build_object('kind','image','url',artist.hero_image_url,'source','artist-profile')) END,
  coalesce((SELECT jsonb_agg(link) FROM (VALUES
    (CASE WHEN artist.spotify_url IS NULL THEN NULL ELSE jsonb_build_object('kind','spotify','url',artist.spotify_url) END),
    (CASE WHEN artist.youtube_url IS NULL THEN NULL ELSE jsonb_build_object('kind','youtube','url',artist.youtube_url) END),
    (CASE WHEN artist.website_url IS NULL THEN NULL ELSE jsonb_build_object('kind','website','url',artist.website_url) END)
  ) links(link) WHERE link IS NOT NULL),'[]'::jsonb),
  CASE WHEN artist.slug_valid AND artist.slug_rank=1 THEN 'published' ELSE 'draft' END,
  'public','allowed',
  least(1, .25 + CASE WHEN artist.bio IS NOT NULL THEN .2 ELSE 0 END + CASE WHEN artist.hero_image_url IS NOT NULL THEN .15 ELSE 0 END),
  artist.created_at,coalesce(artist.updated_at,artist.created_at),
  CASE WHEN artist.slug_valid AND artist.slug_rank=1 THEN coalesce(artist.updated_at,artist.created_at) END
FROM prepared artist JOIN party ON party.id=artist.artist_party_id
ON CONFLICT (id) DO NOTHING;

INSERT INTO directory_profile (
  id,subject_party_id,profile_kind,public_name,slug,portfolio,profile_status,
  visibility,moderation_status,completeness_score
)
SELECT directory_stable_uuid('directory-profile:band',band.id::text),band.party_id,
  'band',band.name,'band-'||band.id::text,
  CASE WHEN band.photo_url IS NULL THEN '[]'::jsonb ELSE jsonb_build_array(jsonb_build_object('kind','image','url',band.photo_url,'source','band')) END,
  'draft','public','allowed',CASE WHEN band.photo_url IS NULL THEN .2 ELSE .35 END
FROM band JOIN party ON party.id=band.party_id
ON CONFLICT (id) DO NOTHING;

INSERT INTO directory_profile_manager (
  profile_id,account_party_id,can_view_private,can_edit,can_publish,can_contact,can_manage,active
)
SELECT profile.id,profile.subject_party_id,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE
FROM directory_profile profile
WHERE EXISTS (SELECT 1 FROM user_credential credential WHERE credential.party_id=profile.subject_party_id AND credential.active)
  AND EXISTS (SELECT 1 FROM artist_profile artist WHERE profile.id=directory_stable_uuid('directory-profile:artist',artist.id::text)
              UNION ALL SELECT 1 FROM band WHERE profile.id=directory_stable_uuid('directory-profile:band',band.id::text))
ON CONFLICT (profile_id,account_party_id) DO NOTHING;

INSERT INTO directory_legacy_link (
  id,profile_id,legacy_kind,legacy_id,source_table,source_slug,source_url,provenance,backfill_run_id
)
SELECT directory_stable_uuid('directory-legacy-link:artist',artist.id::text),
  directory_stable_uuid('directory-profile:artist',artist.id::text),'artist_profile',artist.id::text,
  'artist_profile',artist.slug,CASE WHEN artist.slug IS NULL THEN NULL ELSE '/artistas/'||artist.slug END,
  jsonb_build_object('migration','music-directory-legacy-v1','copiedFields',jsonb_build_array('bio','heroImageUrl','links')),
  directory_stable_uuid('directory-backfill','legacy-v1:apply')
FROM artist_profile artist
ON CONFLICT (legacy_kind,legacy_id) DO NOTHING;

INSERT INTO directory_legacy_link (
  id,profile_id,legacy_kind,legacy_id,source_table,provenance,backfill_run_id
)
SELECT directory_stable_uuid('directory-legacy-link:band',band.id::text),
  directory_stable_uuid('directory-profile:band',band.id::text),'band',band.id::text,'band',
  jsonb_build_object('migration','music-directory-legacy-v1','published',FALSE),
  directory_stable_uuid('directory-backfill','legacy-v1:apply')
FROM band
ON CONFLICT (legacy_kind,legacy_id) DO NOTHING;

-- Public profession and genre memberships cannot alter security roles.
INSERT INTO directory_profile_profession (profile_id,profession_id,sort_order)
SELECT directory_stable_uuid('directory-profile:artist',artist.id::text),profession.id,10
FROM artist_profile artist JOIN profession ON profession.code='artist'
ON CONFLICT (profile_id,profession_id) DO NOTHING;

INSERT INTO directory_profile_genre (profile_id,genre_id,sort_order)
SELECT directory_stable_uuid('directory-profile:artist',artist.id::text),membership.genre_id,membership.sort_order
FROM artist_profile artist
JOIN artist_profile_genre_membership membership ON membership.artist_party_id=artist.artist_party_id
JOIN genre ON genre.id=membership.genre_id
ON CONFLICT (profile_id,genre_id) DO NOTHING;

INSERT INTO directory_profile_location (
  id,profile_id,country_id,subdivision_id,city_id,metropolitan_area_id,
  public_latitude,public_longitude,precision,primary_location,onsite
)
SELECT directory_stable_uuid('directory-profile-location:artist',artist.id::text),
  directory_stable_uuid('directory-profile:artist',artist.id::text),country.id,
  city.subdivision_id,city.id,metro_city.metropolitan_area_id,city.latitude,city.longitude,
  'city',TRUE,TRUE
FROM artist_profile artist
JOIN country_reference country ON country.id=artist.country_id OR (artist.country_id IS NULL AND country.alpha2=upper(artist.country_code))
JOIN city_reference city ON city.country_id=country.id AND directory_normalize_text(city.name_es)=directory_normalize_text(artist.city)
LEFT JOIN metropolitan_area_city metro_city ON metro_city.city_id=city.id
WHERE artist.city IS NOT NULL
ON CONFLICT DO NOTHING;

-- Duplicate social artist models are linked only when their Party reference is
-- unambiguous and already has the canonical artist profile.
INSERT INTO directory_legacy_link (
  id,profile_id,legacy_kind,legacy_id,source_table,provenance,backfill_run_id
)
SELECT directory_stable_uuid('directory-legacy-link:social-artist',social.id::text),
  directory_stable_uuid('directory-profile:artist',artist.id::text),'social_artist_profile',social.id::text,
  'social_artist_profile',jsonb_build_object('migration','music-directory-legacy-v1','match','numeric-party-id'),
  directory_stable_uuid('directory-backfill','legacy-v1:apply')
FROM social_artist_profile social
JOIN party ON social.party_id ~ '^[0-9]+$' AND party.id=social.party_id::bigint
JOIN artist_profile artist ON artist.artist_party_id=party.id
ON CONFLICT (legacy_kind,legacy_id) DO NOTHING;

SELECT directory_refresh_profile_search(profile.id)
FROM directory_profile profile
WHERE EXISTS (SELECT 1 FROM directory_legacy_link link WHERE link.profile_id=profile.id AND link.backfill_run_id=directory_stable_uuid('directory-backfill','legacy-v1:apply'));
SELECT directory_refresh_legacy_event_search();

WITH run AS (SELECT directory_stable_uuid('directory-backfill','legacy-v1:apply') id), candidates AS (
  SELECT 'artist_profile'::text source_table,artist.id::text source_id,artist.slug source_slug,
    'created'::text disposition,directory_stable_uuid('directory-profile:artist',artist.id::text) target_profile_id,'canonical-party-profile'::text reason_code,
    jsonb_build_object('published',(SELECT profile_status='published' FROM directory_profile WHERE id=directory_stable_uuid('directory-profile:artist',artist.id::text))) evidence
  FROM artist_profile artist
  UNION ALL
  SELECT 'band',band.id::text,NULL,'created',directory_stable_uuid('directory-profile:band',band.id::text),'canonical-party-band',jsonb_build_object('published',FALSE) FROM band
  UNION ALL
  SELECT 'social_artist_profile',social.id::text,NULL,
    CASE WHEN link.profile_id IS NULL THEN 'ambiguous' ELSE 'mapped' END,link.profile_id,
    CASE WHEN link.profile_id IS NULL THEN 'unresolved-party-reference' ELSE 'numeric-party-id' END,
    jsonb_build_object('hasPartyReference',social.party_id IS NOT NULL)
  FROM social_artist_profile social LEFT JOIN directory_legacy_link link ON link.legacy_kind='social_artist_profile' AND link.legacy_id=social.id::text
  UNION ALL
  SELECT 'venue',venue.id::text,NULL,'skipped',NULL,'safe-public-event-projection-only',jsonb_build_object('hasPublishedEvent',EXISTS (SELECT 1 FROM directory_public_event event WHERE event.venue_id=venue.id)) FROM venue
  UNION ALL
  SELECT 'social_event',event.id::text,NULL,'skipped',NULL,CASE WHEN public.id IS NULL THEN 'not-public-listable' ELSE 'safe-public-event-projection' END,jsonb_build_object('publicListable',public.id IS NOT NULL) FROM social_event event LEFT JOIN directory_public_event public ON public.id=event.id
)
INSERT INTO directory_backfill_mapping (id,backfill_run_id,source_table,source_id,source_slug,disposition,target_profile_id,reason_code,evidence)
SELECT directory_stable_uuid('directory-backfill-apply-mapping',candidate.source_table||':'||candidate.source_id),run.id,candidate.*
FROM candidates candidate CROSS JOIN run
ON CONFLICT (backfill_run_id,source_table,source_id) DO UPDATE SET
  source_slug=EXCLUDED.source_slug,disposition=EXCLUDED.disposition,target_profile_id=EXCLUDED.target_profile_id,
  reason_code=EXCLUDED.reason_code,evidence=EXCLUDED.evidence;

UPDATE directory_backfill_run run SET status='completed',scanned_rows=counts.scanned,
  mapped_rows=counts.mapped,created_rows=counts.created,ambiguous_rows=counts.ambiguous,
  rejected_rows=counts.rejected,
  report=jsonb_build_object('mapped',counts.mapped,'created',counts.created,'ambiguous',counts.ambiguous,'skipped',counts.skipped,'rejected',counts.rejected),
  completed_at=now()
FROM (SELECT backfill_run_id,count(*) scanned,count(*) FILTER (WHERE disposition='mapped') mapped,
  count(*) FILTER (WHERE disposition='created') created,count(*) FILTER (WHERE disposition='ambiguous') ambiguous,
  count(*) FILTER (WHERE disposition='skipped') skipped,count(*) FILTER (WHERE disposition='rejected') rejected
  FROM directory_backfill_mapping WHERE backfill_run_id=directory_stable_uuid('directory-backfill','legacy-v1:apply') GROUP BY backfill_run_id) counts
WHERE run.id=counts.backfill_run_id;

COMMIT;

SELECT run_code,source_revision,dry_run,status,scanned_rows,mapped_rows,created_rows,
  ambiguous_rows,rejected_rows,report FROM directory_backfill_run
WHERE id=directory_stable_uuid('directory-backfill','legacy-v1:apply');
