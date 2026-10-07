-- Canonical profile preview images and artist-profile derived listings.
--
-- 1. One database resolver chooses the preview image for every directory
--    profile: designated cover, featured portfolio image, primary linked
--    profile media, avatar/logo, first valid portfolio image. Clients only
--    add the generic placeholder when the resolver returns NULL.
-- 2. Every publicly listed artist profile (profile_kind artist or band) owns
--    exactly one derived classified in the formal `artist-profile` category.
--    The classified references its source profile, is maintained only by
--    directory_sync_profile_listing() inside the same transaction as the
--    profile change, and follows the profile lifecycle through
--    published <-> paused, so republishing never creates a duplicate.
--
-- Forward-only and idempotent. Existing listings are created by the separate
-- 2026-10-07_directory_artist_listing_backfill_apply migration.
\set ON_ERROR_STOP on
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

-- ---------------------------------------------------------------------------
-- Image URL normalization shared by every preview source.
-- ---------------------------------------------------------------------------

-- Google Drive viewer links are web pages, not images. Rewrite file links to
-- the public content endpoint (the same form the web Drive helper produces).
CREATE OR REPLACE FUNCTION directory_safe_image_url(raw_value TEXT)
RETURNS TEXT
LANGUAGE SQL
IMMUTABLE
PARALLEL SAFE
AS $$
  WITH input AS (
    SELECT nullif(btrim(raw_value), '') AS value
  ), normalized AS (
    SELECT CASE
      WHEN input.value ~* '^https://(www\.)?drive\.google\.com/file/d/[A-Za-z0-9_-]+'
        THEN 'https://drive.google.com/uc?export=view&id='
          || substring(input.value FROM '(?i)^https://(?:www\.)?drive\.google\.com/file/d/([A-Za-z0-9_-]+)')
          || coalesce('&resourcekey=' || substring(input.value FROM '[?&]resourcekey=([A-Za-z0-9_-]+)'), '')
      WHEN input.value ~* '^https://(www\.)?drive\.google\.com/(open|uc|thumbnail)\?(.*&)?id=[A-Za-z0-9_-]+'
        THEN 'https://drive.google.com/uc?export=view&id='
          || substring(input.value FROM '[?&]id=([A-Za-z0-9_-]+)')
          || coalesce('&resourcekey=' || substring(input.value FROM '[?&]resourcekey=([A-Za-z0-9_-]+)'), '')
      ELSE input.value
    END AS value
    FROM input
  )
  SELECT normalized.value
  FROM normalized
  WHERE normalized.value IS NOT NULL
    AND strpos(normalized.value, chr(92)) = 0
    AND (
      normalized.value ~* '^https?://[^/?#[:space:][:cntrl:]@:%\[\]]+(:[0-9]+)?([/?#]|$)'
      OR normalized.value ~* '^https?://\[[0-9a-f:.]+\](:[0-9]+)?([/?#]|$)'
      OR normalized.value ~ '^/[^/[:space:][:cntrl:]][^[:space:][:cntrl:]]*$'
    )
    AND normalized.value !~ '[[:space:][:cntrl:]]';
$$;

-- Legacy `venue.contact` is free text that usually holds a JSON object.
CREATE OR REPLACE FUNCTION directory_try_jsonb_object(raw_value TEXT)
RETURNS JSONB
LANGUAGE plpgsql
IMMUTABLE
PARALLEL SAFE
AS $$
DECLARE
  parsed JSONB;
BEGIN
  IF raw_value IS NULL OR raw_value !~ '^\s*\{' THEN
    RETURN NULL;
  END IF;
  parsed := raw_value::jsonb;
  RETURN CASE WHEN jsonb_typeof(parsed) = 'object' THEN parsed END;
EXCEPTION WHEN others THEN
  RETURN NULL;
END
$$;

-- Portfolio images explicitly marked as cover/featured/primary.
CREATE OR REPLACE FUNCTION directory_profile_featured_image_url(portfolio_value JSONB)
RETURNS TEXT
LANGUAGE SQL
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT candidate.image_url
  FROM jsonb_array_elements(
    CASE WHEN jsonb_typeof(portfolio_value) = 'array' THEN portfolio_value ELSE '[]'::jsonb END
  ) WITH ORDINALITY entry(value, ordinality)
  CROSS JOIN LATERAL (VALUES
    (directory_safe_image_url(entry.value->>'url'), 0),
    (directory_safe_image_url(entry.value->>'thumbnailUrl'), 1)
  ) candidate(image_url, priority)
  WHERE jsonb_typeof(entry.value) = 'object'
    AND coalesce(entry.value->>'itemType', entry.value->>'kind') = 'image'
    AND (
      lower(coalesce(entry.value->>'role', '')) IN ('cover','featured','primary','preview')
      OR lower(coalesce(entry.value->>'featured', entry.value->>'primary', entry.value->>'cover', 'false')) = 'true'
    )
    AND candidate.image_url IS NOT NULL
  ORDER BY entry.ordinality, candidate.priority
  LIMIT 1;
$$;

-- ---------------------------------------------------------------------------
-- Designated profile cover.
-- ---------------------------------------------------------------------------

ALTER TABLE directory_profile ADD COLUMN IF NOT EXISTS cover_image_url TEXT;

DO $cover_constraint$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'directory_profile'::regclass
      AND conname = 'directory_profile_cover_image_url_check'
  ) THEN
    ALTER TABLE directory_profile
      ADD CONSTRAINT directory_profile_cover_image_url_check
      CHECK (cover_image_url IS NULL OR directory_safe_image_url(cover_image_url) IS NOT NULL);
  END IF;
END
$cover_constraint$;

-- ---------------------------------------------------------------------------
-- Canonical preview image resolver.
-- Priority: designated cover / featured portfolio image; primary linked
-- profile media (artist hero, band photo, venue image); avatar or logo
-- (linked social artist avatar, active merch store logo); first valid
-- portfolio image. NULL means "no valid media": the client shows the
-- placeholder for the entity kind.
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION directory_profile_preview_image_url(profile_id_value UUID)
RETURNS TEXT
LANGUAGE SQL
STABLE
PARALLEL SAFE
AS $$
  SELECT coalesce(
    directory_safe_image_url(profile.cover_image_url),
    directory_profile_featured_image_url(profile.portfolio),
    (SELECT directory_safe_image_url(artist.hero_image_url)
       FROM directory_legacy_link link
       JOIN artist_profile artist ON artist.id::text = link.legacy_id
      WHERE link.profile_id = profile.id AND link.legacy_kind = 'artist_profile'),
    (SELECT directory_safe_image_url(band.photo_url)
       FROM directory_legacy_link link
       JOIN band ON band.id::text = link.legacy_id
      WHERE link.profile_id = profile.id AND link.legacy_kind = 'band'),
    (SELECT directory_safe_image_url(directory_try_jsonb_object(venue.contact)->>'imageUrl')
       FROM directory_legacy_link link
       JOIN venue ON venue.id::text = link.legacy_id
      WHERE link.profile_id = profile.id AND link.legacy_kind = 'venue'),
    (SELECT directory_safe_image_url(social.avatar_url)
       FROM directory_legacy_link link
       JOIN social_artist_profile social ON social.id::text = link.legacy_id
      WHERE link.profile_id = profile.id AND link.legacy_kind = 'social_artist_profile'),
    (SELECT directory_safe_image_url(store.logo_image_url)
       FROM merch_store store
      WHERE store.directory_profile_id = profile.id
        AND store.application_status = 'approved'
        AND store.operational_status = 'active'),
    directory_safe_image_url(directory_profile_primary_image_url(profile.portfolio))
  )
  FROM directory_profile profile
  WHERE profile.id = profile_id_value;
$$;

-- ---------------------------------------------------------------------------
-- Formal artist classification and derived-listing schema.
-- ---------------------------------------------------------------------------

-- The directory taxonomy classifies artist entities by profile kind; persons,
-- projects, venues, studios, labels and organizations are not artists.
CREATE OR REPLACE FUNCTION directory_profile_kind_is_artist(kind_value TEXT)
RETURNS BOOLEAN
LANGUAGE SQL
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT kind_value IN ('artist','band');
$$;

ALTER TABLE classified
  ADD COLUMN IF NOT EXISTS source_profile_id UUID REFERENCES directory_profile(id);

DO $classified_source_constraint$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'classified'::regclass
      AND conname = 'classified_source_profile_author_check'
  ) THEN
    ALTER TABLE classified
      ADD CONSTRAINT classified_source_profile_author_check
      CHECK (source_profile_id IS NULL OR source_profile_id = author_profile_id);
  END IF;
END
$classified_source_constraint$;

-- At most one derived listing per profile, ever. Lifecycle changes reuse the
-- same row (published <-> paused), so this also bounds active listings.
CREATE UNIQUE INDEX IF NOT EXISTS classified_source_profile_uidx
  ON classified (source_profile_id)
  WHERE source_profile_id IS NOT NULL;

ALTER TABLE directory_search_document
  ADD COLUMN IF NOT EXISTS source_profile_id UUID;

CREATE INDEX IF NOT EXISTS directory_search_source_profile_idx
  ON directory_search_document (source_profile_id)
  WHERE source_profile_id IS NOT NULL;

INSERT INTO classified_category
  (id,catalog_id,code,name_es,name_en,description_es,description_en,current_slug,
   requirements,sort_order,active,workflow_state_id,source_name,source_version)
SELECT '22000000-0000-4000-8000-000000000016'::uuid, catalog.id, 'artist-profile',
       'Perfil de artista', 'Artist profile',
       'Anuncio generado y sincronizado automáticamente desde un perfil público de artista o banda.',
       'Listing generated and synchronized automatically from a public artist or band profile.',
       'perfil-artista', '{"required":[],"derivation":"artist-profile"}'::jsonb, 5, TRUE,
       state.id, 'TDF directory seed', '2026-10-07'
FROM catalog_definition catalog
JOIN workflow_definition workflow ON workflow.id = catalog.workflow_id
JOIN workflow_state state ON state.workflow_id = workflow.id AND state.code = 'published'
WHERE catalog.code = 'classified-categories'
ON CONFLICT (code) DO NOTHING;

-- Derived listings are not independently editable. Only the sync function
-- (which sets the transaction-local flag) writes their content; moderators may
-- still take them down through the declared moderated/withdrawn transitions.
CREATE OR REPLACE FUNCTION directory_guard_classified_derivation()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  derived_category BOOLEAN;
  syncing BOOLEAN := coalesce(current_setting('tdf.directory_listing_sync', true), '') = 'on';
  mutable_columns TEXT[] := ARRAY['status','moderation_status','closed_at','published_at','updated_at','version'];
BEGIN
  SELECT category.requirements ? 'derivation' INTO derived_category
  FROM classified_category category WHERE category.id = NEW.category_id;
  IF coalesce(derived_category, FALSE) <> (NEW.source_profile_id IS NOT NULL) THEN
    RAISE EXCEPTION 'derived classified categories require a source profile and vice versa'
      USING ERRCODE = '23514';
  END IF;
  IF syncing THEN
    RETURN NEW;
  END IF;
  IF TG_OP = 'INSERT' THEN
    IF NEW.source_profile_id IS NOT NULL THEN
      RAISE EXCEPTION 'derived classifieds are created only from their source profile'
        USING ERRCODE = '42501';
    END IF;
    RETURN NEW;
  END IF;
  IF OLD.source_profile_id IS DISTINCT FROM NEW.source_profile_id THEN
    RAISE EXCEPTION 'classified source profile is immutable' USING ERRCODE = '42501';
  END IF;
  IF NEW.source_profile_id IS NOT NULL THEN
    IF (to_jsonb(NEW) - mutable_columns) IS DISTINCT FROM (to_jsonb(OLD) - mutable_columns) THEN
      RAISE EXCEPTION 'derived classified content follows its source profile'
        USING ERRCODE = '42501';
    END IF;
    IF NEW.status IS DISTINCT FROM OLD.status AND NEW.status NOT IN ('moderated','withdrawn') THEN
      RAISE EXCEPTION 'derived classified status follows its source profile'
        USING ERRCODE = '42501';
    END IF;
  END IF;
  RETURN NEW;
END
$$;
DROP TRIGGER IF EXISTS directory_classified_derivation_guard_trigger ON classified;
CREATE TRIGGER directory_classified_derivation_guard_trigger
BEFORE INSERT OR UPDATE ON classified
FOR EACH ROW EXECUTE FUNCTION directory_guard_classified_derivation();

-- ---------------------------------------------------------------------------
-- Search projections.
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION directory_refresh_classified_search(classified_id_value UUID)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,summary,image_url,city_id,city_name,country_code,
    public_latitude,public_longitude,location_precision,profession_ids,service_ids,
    instrument_ids,genre_ids,search_text,search_vector,profile_completeness,
    reputation_score,availability_score,onsite,remote,available_to_travel,source_status,visibility,moderation_status,
    effective_at,expires_at,source_updated_at,source_version,sponsored,sponsor_disclosure,source_profile_id
  )
  SELECT
    'classified', classified.id::text, classified.slug, classified.title,
    category.name_es, classified.description,
    CASE WHEN classified.source_profile_id IS NOT NULL
      THEN directory_profile_preview_image_url(classified.source_profile_id)
      ELSE (SELECT directory_safe_image_url(attachment.asset_url)
              FROM classified_attachment attachment
             WHERE attachment.classified_id = classified.id
               AND attachment.media_type = 'image'
               AND attachment.scan_status = 'clean'
               AND directory_safe_image_url(attachment.asset_url) IS NOT NULL
             ORDER BY attachment.sort_order, attachment.created_at, attachment.id
             LIMIT 1)
    END,
    location.city_id, city.name_es, country.alpha2, city.latitude, city.longitude,
    CASE WHEN location.city_id IS NOT NULL THEN 'city'
         WHEN location.metropolitan_area_id IS NOT NULL THEN 'metro'
         WHEN location.subdivision_id IS NOT NULL THEN 'region'
         WHEN location.country_id IS NOT NULL THEN 'country'
         ELSE 'city' END,
    coalesce(professions.ids,'{}'::uuid[]),
    CASE WHEN classified.service_offering_id IS NULL THEN '{}'::uuid[] ELSE ARRAY[classified.service_offering_id] END,
    coalesce(instruments.ids,'{}'::uuid[]),coalesce(genres.ids,'{}'::uuid[]),
    search.content,to_tsvector('simple',search.content),
    coalesce(source_profile.completeness_score, 0),
    least(1, greatest(0, coalesce(source_profile.review_average / 5, 0))),
    CASE WHEN classified.status='published' THEN 1 ELSE 0 END,
    classified.onsite,classified.remote,classified.available_to_travel,
    classified.status,'public',classified.moderation_status,classified.published_at,
    nullif(classified.expires_at, 'infinity'::timestamptz),classified.updated_at,classified.version,FALSE,NULL,
    classified.source_profile_id
  FROM classified
  JOIN classified_category category ON category.id=classified.category_id
  LEFT JOIN directory_profile source_profile ON source_profile.id=classified.source_profile_id
  LEFT JOIN LATERAL (SELECT item.* FROM classified_location item WHERE item.classified_id=classified.id ORDER BY item.city_id NULLS LAST, item.id LIMIT 1) location ON TRUE
  LEFT JOIN city_reference city ON city.id=location.city_id
  LEFT JOIN country_reference country ON country.id=location.country_id
  LEFT JOIN LATERAL (SELECT array_agg(profession_id ORDER BY profession_id) ids FROM classified_profession WHERE classified_id=classified.id) professions ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(instrument_id ORDER BY instrument_id) ids FROM classified_instrument WHERE classified_id=classified.id) instruments ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(genre_id ORDER BY genre_id) ids FROM classified_genre WHERE classified_id=classified.id) genres ON TRUE
  LEFT JOIN LATERAL (
    SELECT directory_normalize_text(concat_ws(' ',classified.title,classified.description,category.name_es,category.name_en,
      (SELECT string_agg(coalesce(term.name_es,term.name_en),' ') FROM classified_genre member JOIN genre term ON term.id=member.genre_id WHERE member.classified_id=classified.id),
      (SELECT string_agg(coalesce(term.name_es,term.name_en),' ') FROM classified_instrument member JOIN instrument term ON term.id=member.instrument_id WHERE member.classified_id=classified.id),
      (SELECT string_agg(coalesce(term.name_es,term.name_en),' ') FROM classified_profession member JOIN profession term ON term.id=member.profession_id WHERE member.classified_id=classified.id)
    )) content
  ) search ON TRUE
  WHERE classified.id=classified_id_value
  ON CONFLICT (entity_kind,entity_id) DO UPDATE SET
    slug=EXCLUDED.slug,title=EXCLUDED.title,subtitle=EXCLUDED.subtitle,summary=EXCLUDED.summary,
    image_url=EXCLUDED.image_url,
    city_id=EXCLUDED.city_id,city_name=EXCLUDED.city_name,country_code=EXCLUDED.country_code,
    public_latitude=EXCLUDED.public_latitude,public_longitude=EXCLUDED.public_longitude,
    location_precision=EXCLUDED.location_precision,profession_ids=EXCLUDED.profession_ids,
    service_ids=EXCLUDED.service_ids,instrument_ids=EXCLUDED.instrument_ids,
    genre_ids=EXCLUDED.genre_ids,search_text=EXCLUDED.search_text,
    search_vector=EXCLUDED.search_vector,profile_completeness=EXCLUDED.profile_completeness,
    reputation_score=EXCLUDED.reputation_score,availability_score=EXCLUDED.availability_score,
    onsite=EXCLUDED.onsite,remote=EXCLUDED.remote,available_to_travel=EXCLUDED.available_to_travel,
    source_status=EXCLUDED.source_status,moderation_status=EXCLUDED.moderation_status,
    effective_at=EXCLUDED.effective_at,expires_at=EXCLUDED.expires_at,
    source_updated_at=EXCLUDED.source_updated_at,source_version=EXCLUDED.source_version,
    source_profile_id=EXCLUDED.source_profile_id;
END;
$$;

-- ---------------------------------------------------------------------------
-- Profile -> derived listing synchronization.
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION directory_sync_profile_listing(profile_id_value UUID)
RETURNS UUID
LANGUAGE plpgsql
AS $$
DECLARE
  profile directory_profile%ROWTYPE;
  listing classified%ROWTYPE;
  category_id_value UUID;
  eligible BOOLEAN;
  publicly_listed BOOLEAN;
  title_value TEXT;
  description_value TEXT;
  slug_value TEXT;
  target_status TEXT;
  primary_location directory_profile_location%ROWTYPE;
  content_changed BOOLEAN;
BEGIN
  SELECT * INTO profile FROM directory_profile WHERE id = profile_id_value;
  IF NOT FOUND THEN
    RETURN NULL;
  END IF;

  eligible := directory_profile_kind_is_artist(profile.profile_kind);
  publicly_listed := profile.profile_status = 'published'
    AND profile.visibility = 'public'
    AND profile.moderation_status = 'allowed'
    AND profile.canonical_profile_id IS NULL;

  SELECT * INTO listing FROM classified WHERE source_profile_id = profile.id;
  IF NOT FOUND AND NOT (eligible AND publicly_listed) THEN
    -- Draft, private, unlisted, moderated or non-artist profiles are never advertised.
    RETURN NULL;
  END IF;

  title_value := CASE
    WHEN length(btrim(profile.public_name)) >= 5 THEN left(btrim(profile.public_name), 160)
    ELSE btrim(profile.public_name) || CASE WHEN profile.profile_kind = 'band' THEN ' · Banda' ELSE ' · Artista' END
  END;
  description_value := CASE
    WHEN length(btrim(coalesce(profile.bio, ''))) >= 20 THEN left(btrim(profile.bio), 10000)
    ELSE btrim(profile.public_name)
      || CASE WHEN profile.profile_kind = 'band' THEN ' es una banda' ELSE ' es artista' END
      || ' del Directorio Musical TDF. Visita su perfil para conocer su música y contactarle.'
  END;

  PERFORM set_config('tdf.directory_listing_sync', 'on', true);

  IF listing.id IS NULL THEN
    SELECT id INTO category_id_value FROM classified_category WHERE code = 'artist-profile';
    IF category_id_value IS NULL THEN
      RAISE EXCEPTION 'artist-profile classified category is missing';
    END IF;
    slug_value := left(profile.slug, 150) || '-perfil';
    IF EXISTS (SELECT 1 FROM classified WHERE slug = slug_value) THEN
      slug_value := left(profile.slug, 140) || '-perfil-' || left(md5(profile.id::text), 8);
    END IF;
    INSERT INTO classified (
      author_profile_id, source_profile_id, category_id, title, slug, description,
      status, moderation_status, onsite, remote, available_to_travel, service_radius_km,
      expires_at, published_at
    ) VALUES (
      profile.id, profile.id, category_id_value, title_value, slug_value, description_value,
      'published', 'allowed',
      profile.onsite OR NOT (profile.onsite OR profile.remote OR profile.available_to_travel),
      profile.remote, profile.available_to_travel, profile.travel_radius_km,
      'infinity'::timestamptz, now()
    )
    ON CONFLICT (source_profile_id) WHERE source_profile_id IS NOT NULL DO NOTHING;
    SELECT * INTO listing FROM classified WHERE source_profile_id = profile.id;
  END IF;

  content_changed := listing.title IS DISTINCT FROM title_value
    OR listing.description IS DISTINCT FROM description_value
    OR listing.onsite IS DISTINCT FROM (profile.onsite OR NOT (profile.onsite OR profile.remote OR profile.available_to_travel))
    OR listing.remote IS DISTINCT FROM profile.remote
    OR listing.available_to_travel IS DISTINCT FROM profile.available_to_travel
    OR listing.service_radius_km IS DISTINCT FROM profile.travel_radius_km
    OR listing.expires_at IS DISTINCT FROM 'infinity'::timestamptz;
  IF content_changed THEN
    UPDATE classified SET
      title = title_value,
      description = description_value,
      onsite = profile.onsite OR NOT (profile.onsite OR profile.remote OR profile.available_to_travel),
      remote = profile.remote,
      available_to_travel = profile.available_to_travel,
      service_radius_km = profile.travel_radius_km,
      expires_at = 'infinity'::timestamptz,
      updated_at = now(),
      version = version + 1
    WHERE id = listing.id;
  END IF;

  -- Taxonomy mirrors the profile.
  DELETE FROM classified_genre item WHERE item.classified_id = listing.id
    AND NOT EXISTS (SELECT 1 FROM directory_profile_genre source WHERE source.profile_id = profile.id AND source.genre_id = item.genre_id);
  INSERT INTO classified_genre (classified_id, genre_id)
    SELECT listing.id, source.genre_id FROM directory_profile_genre source WHERE source.profile_id = profile.id
    ON CONFLICT DO NOTHING;
  DELETE FROM classified_instrument item WHERE item.classified_id = listing.id
    AND NOT EXISTS (SELECT 1 FROM directory_profile_instrument source WHERE source.profile_id = profile.id AND source.instrument_id = item.instrument_id);
  INSERT INTO classified_instrument (classified_id, instrument_id)
    SELECT listing.id, source.instrument_id FROM directory_profile_instrument source WHERE source.profile_id = profile.id
    ON CONFLICT DO NOTHING;
  DELETE FROM classified_profession item WHERE item.classified_id = listing.id
    AND NOT EXISTS (SELECT 1 FROM directory_profile_profession source WHERE source.profile_id = profile.id AND source.profession_id = item.profession_id);
  INSERT INTO classified_profession (classified_id, profession_id)
    SELECT listing.id, source.profession_id FROM directory_profile_profession source WHERE source.profile_id = profile.id
    ON CONFLICT DO NOTHING;

  -- Location never exceeds the profile's public precision: sector labels and
  -- commercial coordinates are reduced to the city; private locations are
  -- never read.
  SELECT * INTO primary_location FROM directory_profile_location item
   WHERE item.profile_id = profile.id
   ORDER BY item.primary_location DESC, item.created_at, item.id
   LIMIT 1;
  DELETE FROM classified_location item WHERE item.classified_id = listing.id
    AND (primary_location.id IS NULL OR NOT (
      item.country_id = primary_location.country_id
      AND item.subdivision_id IS NOT DISTINCT FROM CASE WHEN primary_location.precision = 'country' THEN NULL ELSE primary_location.subdivision_id END
      AND item.city_id IS NOT DISTINCT FROM CASE WHEN primary_location.precision IN ('city','sector','commercial_exact') THEN primary_location.city_id END
      AND item.metropolitan_area_id IS NOT DISTINCT FROM CASE WHEN primary_location.precision IN ('metro','city','sector','commercial_exact') THEN primary_location.metropolitan_area_id END
    ));
  IF primary_location.id IS NOT NULL THEN
    INSERT INTO classified_location (classified_id, country_id, subdivision_id, city_id, metropolitan_area_id, service_radius_km)
    SELECT listing.id, primary_location.country_id,
      CASE WHEN primary_location.precision = 'country' THEN NULL ELSE primary_location.subdivision_id END,
      CASE WHEN primary_location.precision IN ('city','sector','commercial_exact') THEN primary_location.city_id END,
      CASE WHEN primary_location.precision IN ('metro','city','sector','commercial_exact') THEN primary_location.metropolitan_area_id END,
      primary_location.service_radius_km
    WHERE NOT EXISTS (SELECT 1 FROM classified_location existing WHERE existing.classified_id = listing.id)
    ON CONFLICT DO NOTHING;
  END IF;

  -- Lifecycle: published while the profile is publicly listed, paused
  -- otherwise. Moderator decisions (moderated, withdrawn, rejected) are final
  -- and are never reverted by the profile.
  target_status := CASE WHEN eligible AND publicly_listed THEN 'published' ELSE 'paused' END;
  SELECT * INTO listing FROM classified WHERE id = listing.id;
  IF listing.status <> target_status AND listing.moderation_status = 'allowed' AND (
       (target_status = 'published' AND listing.status IN ('paused','expired','draft'))
    OR (target_status = 'paused' AND listing.status = 'published')
  ) THEN
    UPDATE classified SET status = target_status, expires_at = 'infinity'::timestamptz WHERE id = listing.id;
  END IF;

  PERFORM set_config('tdf.directory_listing_sync', 'off', true);
  PERFORM directory_refresh_classified_search(listing.id);
  RETURN listing.id;
END
$$;

CREATE OR REPLACE FUNCTION directory_refresh_profile_search(profile_id_value UUID)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,summary,image_url,city_id,city_name,
    country_code,public_latitude,public_longitude,location_precision,
    profession_ids,service_ids,instrument_ids,genre_ids,search_text,search_vector,
    profile_completeness,reputation_score,availability_score,onsite,remote,available_to_travel,source_status,
    visibility,moderation_status,effective_at,expires_at,source_updated_at,
    source_version,sponsored,sponsor_disclosure
  )
  SELECT
    'profile', profile.id::text, profile.slug, profile.public_name,
    nullif(concat_ws(' · ', profession_names.names, instrument_names.names), ''),
    profile.bio, directory_profile_preview_image_url(profile.id),
    location.city_id, city.name_es, country.alpha2,
    location.public_latitude, location.public_longitude, location.precision,
    coalesce(professions.ids, '{}'::uuid[]), coalesce(services.ids, '{}'::uuid[]),
    coalesce(instruments.ids, '{}'::uuid[]), coalesce(genres.ids, '{}'::uuid[]),
    search.content, to_tsvector('simple', search.content), profile.completeness_score,
    least(1, greatest(0, coalesce(profile.review_average / 5, 0))),
    CASE profile.availability_status WHEN 'available' THEN 1 WHEN 'limited' THEN .6 WHEN 'ask' THEN .35 ELSE 0 END,
    profile.onsite,profile.remote,profile.available_to_travel,
    profile.profile_status, profile.visibility, profile.moderation_status,
    profile.published_at, NULL, profile.updated_at, profile.version, FALSE, NULL
  FROM directory_profile profile
  LEFT JOIN LATERAL (
    SELECT item.* FROM directory_profile_location item
    WHERE item.profile_id=profile.id
    ORDER BY item.primary_location DESC, item.created_at, item.id LIMIT 1
  ) location ON TRUE
  LEFT JOIN city_reference city ON city.id=location.city_id
  LEFT JOIN country_reference country ON country.id=location.country_id
  LEFT JOIN LATERAL (SELECT array_agg(item.profession_id ORDER BY item.sort_order,item.profession_id) ids FROM directory_profile_profession item WHERE item.profile_id=profile.id) professions ON TRUE
  LEFT JOIN LATERAL (SELECT string_agg(coalesce(item.name_es,item.name_en), ' ') names FROM directory_profile_profession member JOIN profession item ON item.id=member.profession_id WHERE member.profile_id=profile.id) profession_names ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(item.service_offering_id ORDER BY item.sort_order,item.service_offering_id) ids FROM directory_profile_service item WHERE item.profile_id=profile.id) services ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(item.instrument_id ORDER BY item.sort_order,item.instrument_id) ids FROM directory_profile_instrument item WHERE item.profile_id=profile.id) instruments ON TRUE
  LEFT JOIN LATERAL (SELECT string_agg(coalesce(item.name_es,item.name_en), ' ') names FROM directory_profile_instrument member JOIN instrument item ON item.id=member.instrument_id WHERE member.profile_id=profile.id) instrument_names ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(item.genre_id ORDER BY item.sort_order,item.genre_id) ids FROM directory_profile_genre item WHERE item.profile_id=profile.id) genres ON TRUE
  LEFT JOIN LATERAL (
    SELECT directory_normalize_text(concat_ws(' ', profile.public_name,profile.bio,
      profile.experience_summary,profile.credits_summary,profile.equipment_summary,
      profession_names.names,instrument_names.names,
      (SELECT string_agg(coalesce(term.name_es,term.name_en), ' ') FROM directory_profile_genre member JOIN genre term ON term.id=member.genre_id WHERE member.profile_id=profile.id),
      (SELECT string_agg(coalesce(term.name_es,term.name_en), ' ') FROM directory_profile_service member JOIN service_offering term ON term.id=member.service_offering_id WHERE member.profile_id=profile.id)
    )) content
  ) search ON TRUE
  WHERE profile.id=profile_id_value
  ON CONFLICT (entity_kind,entity_id) DO UPDATE SET
    slug=EXCLUDED.slug,title=EXCLUDED.title,subtitle=EXCLUDED.subtitle,summary=EXCLUDED.summary,
    image_url=EXCLUDED.image_url,
    city_id=EXCLUDED.city_id,city_name=EXCLUDED.city_name,country_code=EXCLUDED.country_code,
    public_latitude=EXCLUDED.public_latitude,public_longitude=EXCLUDED.public_longitude,
    location_precision=EXCLUDED.location_precision,profession_ids=EXCLUDED.profession_ids,
    service_ids=EXCLUDED.service_ids,instrument_ids=EXCLUDED.instrument_ids,
    genre_ids=EXCLUDED.genre_ids,search_text=EXCLUDED.search_text,
    search_vector=EXCLUDED.search_vector,profile_completeness=EXCLUDED.profile_completeness,
    reputation_score=EXCLUDED.reputation_score,availability_score=EXCLUDED.availability_score,
    onsite=EXCLUDED.onsite,remote=EXCLUDED.remote,available_to_travel=EXCLUDED.available_to_travel,
    source_status=EXCLUDED.source_status,visibility=EXCLUDED.visibility,
    moderation_status=EXCLUDED.moderation_status,effective_at=EXCLUDED.effective_at,
    source_updated_at=EXCLUDED.source_updated_at,source_version=EXCLUDED.source_version;
  -- Taxonomy and location children are only written by handlers that end in
  -- this refresh, so the derived listing is re-derived at the same point.
  PERFORM directory_sync_profile_listing(profile_id_value);
END;
$$;

-- Lifecycle, moderation, merge and content changes on the profile row itself
-- (including administrative paths that do not refresh search) re-derive the
-- listing inside the same transaction.
CREATE OR REPLACE FUNCTION directory_profile_listing_sync_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  PERFORM directory_sync_profile_listing(NEW.id);
  RETURN NEW;
END
$$;
DROP TRIGGER IF EXISTS directory_profile_artist_listing_sync_trigger ON directory_profile;
CREATE TRIGGER directory_profile_artist_listing_sync_trigger
AFTER INSERT OR UPDATE ON directory_profile
FOR EACH ROW EXECUTE FUNCTION directory_profile_listing_sync_trigger();

-- Archived, suspended and merged profiles still withdraw their own manual
-- classifieds; the derived listing is paused by the sync trigger instead so a
-- restored profile resumes the same listing.
CREATE OR REPLACE FUNCTION directory_withdraw_profile_surfaces()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.profile_status IN ('archived','suspended','merged')
     AND OLD.profile_status IS DISTINCT FROM NEW.profile_status THEN
    UPDATE classified
      SET status = CASE WHEN status IN ('published','paused') THEN 'withdrawn' ELSE status END,
          closed_at = CASE WHEN status IN ('published','paused') THEN now() ELSE closed_at END,
          updated_at = now()
      WHERE author_profile_id = NEW.id
        AND source_profile_id IS NULL
        AND status IN ('published','paused');
    DELETE FROM directory_search_document
      WHERE (entity_kind = 'profile' AND entity_id = NEW.id::text)
         OR (entity_kind = 'classified' AND entity_id IN (
              SELECT id::text FROM classified WHERE author_profile_id = NEW.id
            ));
  END IF;
  RETURN NEW;
END
$$;

-- Recovery path: re-derive every listing that should exist or already exists.
-- Safe to run repeatedly; used by the backfill and by operators after any
-- incident (see docs/music-directory/artist-derived-listings.md).
CREATE OR REPLACE FUNCTION directory_reconcile_artist_listings()
RETURNS TABLE(profile_id UUID, classified_id UUID, listing_status TEXT)
LANGUAGE plpgsql
AS $$
DECLARE
  candidate RECORD;
  synced UUID;
BEGIN
  FOR candidate IN
    SELECT profile.id FROM directory_profile profile
    WHERE directory_profile_kind_is_artist(profile.profile_kind)
       OR EXISTS (SELECT 1 FROM classified item WHERE item.source_profile_id = profile.id)
    ORDER BY profile.id
  LOOP
    synced := directory_sync_profile_listing(candidate.id);
    profile_id := candidate.id;
    classified_id := synced;
    listing_status := (SELECT item.status FROM classified item WHERE item.id = synced);
    RETURN NEXT;
  END LOOP;
END
$$;

-- Read-only consistency audit. Returns one row per finding; never repairs.
CREATE OR REPLACE FUNCTION directory_artist_listing_audit()
RETURNS TABLE(finding_kind TEXT, profile_id UUID, classified_id UUID, detail JSONB)
LANGUAGE SQL
STABLE
AS $$
  WITH profiles AS (
    SELECT profile.*,
      directory_profile_kind_is_artist(profile.profile_kind) AS is_artist,
      (profile.profile_status = 'published' AND profile.visibility = 'public'
        AND profile.moderation_status = 'allowed' AND profile.canonical_profile_id IS NULL) AS is_public
    FROM directory_profile profile
  ), derived AS (
    SELECT item.*, document.image_url AS document_image_url, document.source_status AS document_status
    FROM classified item
    LEFT JOIN directory_search_document document
      ON document.entity_kind = 'classified' AND document.entity_id = item.id::text
    WHERE item.source_profile_id IS NOT NULL
  )
  SELECT 'missing_listing', profiles.id, NULL::uuid,
         jsonb_build_object('slug', profiles.slug, 'kind', profiles.profile_kind)
  FROM profiles
  WHERE profiles.is_artist AND profiles.is_public
    AND NOT EXISTS (SELECT 1 FROM classified item WHERE item.source_profile_id = profiles.id)
  UNION ALL
  SELECT 'duplicate_derived_listing', derived.source_profile_id, derived.id,
         jsonb_build_object('count', count(*) OVER (PARTITION BY derived.source_profile_id))
  FROM derived
  WHERE (SELECT count(*) FROM classified item WHERE item.source_profile_id = derived.source_profile_id) > 1
  UNION ALL
  SELECT 'public_listing_non_public_profile', profiles.id, derived.id,
         jsonb_build_object('profileStatus', profiles.profile_status, 'visibility', profiles.visibility,
                            'moderation', profiles.moderation_status, 'listingStatus', derived.status)
  FROM derived JOIN profiles ON profiles.id = derived.source_profile_id
  WHERE derived.status = 'published' AND NOT (profiles.is_public AND profiles.is_artist)
  UNION ALL
  SELECT 'unpublished_listing_public_profile', profiles.id, derived.id,
         jsonb_build_object('listingStatus', derived.status, 'listingModeration', derived.moderation_status)
  FROM derived JOIN profiles ON profiles.id = derived.source_profile_id
  WHERE derived.status <> 'published' AND profiles.is_public AND profiles.is_artist
  UNION ALL
  SELECT 'orphaned_listing_non_artist_profile', profiles.id, derived.id,
         jsonb_build_object('kind', profiles.profile_kind, 'listingStatus', derived.status)
  FROM derived JOIN profiles ON profiles.id = derived.source_profile_id
  WHERE NOT profiles.is_artist AND derived.status = 'published'
  UNION ALL
  SELECT 'inconsistent_listing_image', derived.source_profile_id, derived.id,
         jsonb_build_object('documentImage', derived.document_image_url,
                            'profileImage', directory_profile_preview_image_url(derived.source_profile_id))
  FROM derived
  WHERE derived.document_status IS NOT NULL
    AND derived.document_image_url IS DISTINCT FROM directory_profile_preview_image_url(derived.source_profile_id)
  UNION ALL
  SELECT 'stale_listing_search_document', derived.source_profile_id, derived.id,
         jsonb_build_object('listingStatus', derived.status, 'documentStatus', derived.document_status)
  FROM derived
  WHERE derived.status = 'published' AND derived.document_status IS DISTINCT FROM 'published'
  UNION ALL
  SELECT 'diverged_listing_content', profiles.id, derived.id,
         jsonb_build_object('listingTitle', derived.title, 'profileName', profiles.public_name)
  FROM derived JOIN profiles ON profiles.id = derived.source_profile_id
  WHERE derived.title NOT IN (left(btrim(profiles.public_name), 160),
                              btrim(profiles.public_name) || ' · Banda',
                              btrim(profiles.public_name) || ' · Artista')
  UNION ALL
  -- Manual listings by artist profiles that look like a profile ad. Their
  -- categories carry different semantics (seeking musician, services, ...),
  -- so they are reported for human review and never merged automatically.
  SELECT 'ambiguous_manual_artist_listing', profiles.id, item.id,
         jsonb_build_object('slug', item.slug, 'title', item.title, 'status', item.status,
                            'category', category.code)
  FROM classified item
  JOIN profiles ON profiles.id = item.author_profile_id
  JOIN classified_category category ON category.id = item.category_id
  WHERE item.source_profile_id IS NULL
    AND profiles.is_artist
    AND item.status IN ('draft','pending_moderation','published','paused')
    AND (directory_normalize_text(item.title) = directory_normalize_text(profiles.public_name)
         OR directory_normalize_text(item.title) LIKE directory_normalize_text(profiles.public_name) || '%');
$$;

-- ---------------------------------------------------------------------------
-- Project preview images onto existing search rows (profiles, events, venues).
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION directory_refresh_legacy_event_search()
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  DELETE FROM directory_search_document WHERE entity_kind IN ('event','venue');
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,summary,image_url,city_id,city_name,country_code,
    public_latitude,public_longitude,location_precision,search_text,search_vector,
    source_status,visibility,moderation_status,effective_at,expires_at,
    source_updated_at,source_version,sponsored
  )
  SELECT 'event',event.id::text,'evento-'||event.id::text,event.title,event.venue_name,
    event.description,directory_safe_image_url(directory_try_jsonb_object(source_event.metadata)->>'imageUrl'),event.city_id,event.city_name,event.country_code,event.public_latitude,
    event.public_longitude,'city',directory_normalize_text(concat_ws(' ',event.title,event.description,event.venue_name,event.city_name)),
    to_tsvector('simple',directory_normalize_text(concat_ws(' ',event.title,event.description,event.venue_name,event.city_name))),
    'published','public','allowed',event.start_time,event.end_time,event.updated_at,1,FALSE
  FROM directory_public_event event
  JOIN social_event source_event ON source_event.id=event.id;
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,image_url,city_id,city_name,country_code,
    public_latitude,public_longitude,location_precision,search_text,search_vector,
    source_status,visibility,moderation_status,source_updated_at,source_version,sponsored
  )
  SELECT 'venue',venue.id::text,'venue-'||venue.id::text,venue.name,venue.city_name,
    directory_safe_image_url(directory_try_jsonb_object(source_venue.contact)->>'imageUrl'),
    venue.city_id,venue.city_name,venue.country_code,venue.public_latitude,venue.public_longitude,
    'city',directory_normalize_text(concat_ws(' ',venue.name,venue.city_name)),
    to_tsvector('simple',directory_normalize_text(concat_ws(' ',venue.name,venue.city_name))),
    'published','public','allowed',venue.updated_at,1,FALSE
  FROM directory_public_venue venue
  JOIN venue source_venue ON source_venue.id=venue.id;
END;
$$;

UPDATE directory_search_document document
SET image_url = directory_profile_preview_image_url(profile.id)
FROM directory_profile profile
WHERE document.entity_kind = 'profile'
  AND document.entity_id = profile.id::text
  AND document.image_url IS DISTINCT FROM directory_profile_preview_image_url(profile.id);

UPDATE directory_search_document document
SET image_url = directory_safe_image_url(directory_try_jsonb_object(event.metadata)->>'imageUrl')
FROM social_event event
WHERE document.entity_kind = 'event'
  AND document.entity_id = event.id::text
  AND document.image_url IS DISTINCT FROM directory_safe_image_url(directory_try_jsonb_object(event.metadata)->>'imageUrl');

UPDATE directory_search_document document
SET image_url = directory_safe_image_url(directory_try_jsonb_object(venue.contact)->>'imageUrl')
FROM venue
WHERE document.entity_kind = 'venue'
  AND document.entity_id = venue.id::text
  AND document.image_url IS DISTINCT FROM directory_safe_image_url(directory_try_jsonb_object(venue.contact)->>'imageUrl');

-- Public projection gains the derivation link (appended column).
CREATE OR REPLACE VIEW directory_public_search_document AS
SELECT
  document.entity_kind,
  document.entity_id,
  document.slug,
  document.title,
  document.subtitle,
  document.summary,
  document.image_url,
  document.city_id,
  document.city_name,
  document.country_code,
  document.public_latitude,
  document.public_longitude,
  document.location_precision,
  document.profession_ids,
  document.service_ids,
  document.instrument_ids,
  document.genre_ids,
  document.search_text,
  document.search_vector,
  document.profile_completeness,
  document.reputation_score,
  document.availability_score,
  document.effective_at,
  document.expires_at,
  document.source_updated_at,
  document.source_version,
  document.sponsored,
  document.sponsor_disclosure,
  document.onsite,
  document.remote,
  document.available_to_travel,
  document.source_profile_id
FROM directory_search_document document
WHERE document.source_status = 'published'
  AND document.visibility = 'public'
  AND document.moderation_status = 'allowed'
  AND (document.effective_at IS NULL OR document.effective_at <= now())
  AND (document.expires_at IS NULL OR document.expires_at > now())
  AND CASE document.entity_kind
    WHEN 'event' THEN
      EXISTS (
        SELECT 1
        FROM directory_public_event event
        WHERE event.id = CASE
          WHEN document.entity_id ~ '^[0-9]+$' THEN document.entity_id::bigint
          ELSE NULL
        END
      )
    WHEN 'venue' THEN
      EXISTS (
        SELECT 1
        FROM directory_public_venue venue
        WHERE venue.id = CASE
          WHEN document.entity_id ~ '^[0-9]+$' THEN document.entity_id::bigint
          ELSE NULL
        END
      )
    -- A derived listing is public only while its source profile is.
    WHEN 'classified' THEN
      document.source_profile_id IS NULL
      OR EXISTS (
        SELECT 1
        FROM directory_public_profile profile
        WHERE profile.id = document.source_profile_id
      )
    ELSE TRUE
  END;

COMMIT;
