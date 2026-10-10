-- Non-destructive rollback for 2026-10-07_directory_artist_derived_listings.
-- Removes the derivation triggers and restores the previous projection
-- functions. Columns, the artist-profile category and derived classified rows
-- are kept (derived rows are paused) so a forward re-apply resumes them.
\set ON_ERROR_STOP on
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

DROP TRIGGER IF EXISTS directory_profile_artist_listing_sync_trigger ON directory_profile;
DROP TRIGGER IF EXISTS directory_classified_derivation_guard_trigger ON classified;
DROP TRIGGER IF EXISTS directory_artist_profile_media_trigger ON artist_profile;
DROP TRIGGER IF EXISTS directory_band_media_trigger ON band;
DROP TRIGGER IF EXISTS directory_venue_media_trigger ON venue;
DROP TRIGGER IF EXISTS directory_social_artist_media_trigger ON social_artist_profile;
DROP TRIGGER IF EXISTS directory_merch_store_media_trigger ON merch_store;

UPDATE classified SET status = 'paused'
WHERE source_profile_id IS NOT NULL AND status = 'published';
UPDATE directory_search_document SET source_status = 'paused'
WHERE entity_kind = 'classified' AND source_profile_id IS NOT NULL AND source_status = 'published';

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
    profile.bio, directory_profile_primary_image_url(profile.portfolio),
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
END;
$$;

CREATE OR REPLACE FUNCTION directory_refresh_classified_search(classified_id_value UUID)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,summary,city_id,city_name,country_code,
    public_latitude,public_longitude,location_precision,profession_ids,service_ids,
    instrument_ids,genre_ids,search_text,search_vector,profile_completeness,
    reputation_score,availability_score,onsite,remote,available_to_travel,source_status,visibility,moderation_status,
    effective_at,expires_at,source_updated_at,source_version,sponsored,sponsor_disclosure
  )
  SELECT
    'classified', classified.id::text, classified.slug, classified.title,
    category.name_es, classified.description, location.city_id, city.name_es,
    country.alpha2, city.latitude, city.longitude, 'city',
    coalesce(professions.ids,'{}'::uuid[]),
    CASE WHEN classified.service_offering_id IS NULL THEN '{}'::uuid[] ELSE ARRAY[classified.service_offering_id] END,
    coalesce(instruments.ids,'{}'::uuid[]),coalesce(genres.ids,'{}'::uuid[]),
    search.content,to_tsvector('simple',search.content),0,0,
    CASE WHEN classified.status='published' THEN 1 ELSE 0 END,
    classified.onsite,classified.remote,classified.available_to_travel,
    classified.status,'public',classified.moderation_status,classified.published_at,
    classified.expires_at,classified.updated_at,classified.version,FALSE,NULL
  FROM classified
  JOIN classified_category category ON category.id=classified.category_id
  LEFT JOIN LATERAL (SELECT item.* FROM classified_location item WHERE item.classified_id=classified.id ORDER BY item.city_id NULLS LAST LIMIT 1) location ON TRUE
  LEFT JOIN city_reference city ON city.id=location.city_id
  LEFT JOIN country_reference country ON country.id=location.country_id
  LEFT JOIN LATERAL (SELECT array_agg(profession_id) ids FROM classified_profession WHERE classified_id=classified.id) professions ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(instrument_id) ids FROM classified_instrument WHERE classified_id=classified.id) instruments ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(genre_id) ids FROM classified_genre WHERE classified_id=classified.id) genres ON TRUE
  LEFT JOIN LATERAL (SELECT directory_normalize_text(concat_ws(' ',classified.title,classified.description,category.name_es,category.name_en)) content) search ON TRUE
  WHERE classified.id=classified_id_value
  ON CONFLICT (entity_kind,entity_id) DO UPDATE SET
    slug=EXCLUDED.slug,title=EXCLUDED.title,subtitle=EXCLUDED.subtitle,summary=EXCLUDED.summary,
    city_id=EXCLUDED.city_id,city_name=EXCLUDED.city_name,country_code=EXCLUDED.country_code,
    public_latitude=EXCLUDED.public_latitude,public_longitude=EXCLUDED.public_longitude,
    location_precision=EXCLUDED.location_precision,profession_ids=EXCLUDED.profession_ids,
    service_ids=EXCLUDED.service_ids,instrument_ids=EXCLUDED.instrument_ids,
    genre_ids=EXCLUDED.genre_ids,search_text=EXCLUDED.search_text,
    search_vector=EXCLUDED.search_vector,availability_score=EXCLUDED.availability_score,
    onsite=EXCLUDED.onsite,remote=EXCLUDED.remote,available_to_travel=EXCLUDED.available_to_travel,
    source_status=EXCLUDED.source_status,moderation_status=EXCLUDED.moderation_status,
    effective_at=EXCLUDED.effective_at,expires_at=EXCLUDED.expires_at,
    source_updated_at=EXCLUDED.source_updated_at,source_version=EXCLUDED.source_version;
END;
$$;

-- The venue projection returns to the 2026-10-09 definition (no image).
CREATE OR REPLACE FUNCTION directory_sync_venue_search(target_venue_id BIGINT)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  IF target_venue_id IS NULL THEN
    RETURN;
  END IF;
  -- The statement below reads its sources after any concurrent sync committed.
  PERFORM directory_search_sync_lock();
  IF NOT EXISTS (SELECT 1 FROM directory_public_venue WHERE id = target_venue_id) THEN
    RETURN;
  END IF;
  INSERT INTO directory_search_document (
    entity_kind, entity_id, slug, title, subtitle, city_id, city_name, country_code,
    public_latitude, public_longitude, location_precision, search_text, search_vector,
    source_status, visibility, moderation_status, source_updated_at, source_version, sponsored
  )
  SELECT DISTINCT ON (venue.id)
    'venue', venue.id::text, 'venue-' || venue.id::text, venue.name, venue.city_name,
    venue.city_id, venue.city_name, venue.country_code, venue.public_latitude, venue.public_longitude,
    'city',
    directory_normalize_text(concat_ws(' ', venue.name, venue.city_name, 'venue venues local locales')),
    to_tsvector('simple', directory_normalize_text(concat_ws(' ', venue.name, venue.city_name, 'venue venues local locales'))),
    'published', 'public', 'allowed', venue.updated_at, 1, FALSE
  FROM directory_public_venue venue
  WHERE venue.id = target_venue_id
  ON CONFLICT (entity_kind, entity_id) DO UPDATE SET
    slug = EXCLUDED.slug, title = EXCLUDED.title, subtitle = EXCLUDED.subtitle,
    city_id = EXCLUDED.city_id, city_name = EXCLUDED.city_name, country_code = EXCLUDED.country_code,
    public_latitude = EXCLUDED.public_latitude, public_longitude = EXCLUDED.public_longitude,
    location_precision = EXCLUDED.location_precision, search_text = EXCLUDED.search_text,
    search_vector = EXCLUDED.search_vector, source_status = EXCLUDED.source_status,
    visibility = EXCLUDED.visibility, moderation_status = EXCLUDED.moderation_status,
    source_updated_at = EXCLUDED.source_updated_at,
    source_version = directory_search_document.source_version + 1
  -- An unchanged projection keeps its version: saved-search alerts fire on
  -- source_version, so a rebuild must not notify again.
  WHERE (directory_search_document.slug, directory_search_document.title,
         directory_search_document.subtitle, directory_search_document.city_id,
         directory_search_document.city_name, directory_search_document.country_code,
         directory_search_document.public_latitude, directory_search_document.public_longitude,
         directory_search_document.location_precision, directory_search_document.search_text,
         directory_search_document.source_status, directory_search_document.visibility,
         directory_search_document.moderation_status, directory_search_document.source_updated_at)
    IS DISTINCT FROM
        (EXCLUDED.slug, EXCLUDED.title, EXCLUDED.subtitle, EXCLUDED.city_id,
         EXCLUDED.city_name, EXCLUDED.country_code, EXCLUDED.public_latitude,
         EXCLUDED.public_longitude, EXCLUDED.location_precision, EXCLUDED.search_text,
         EXCLUDED.source_status, EXCLUDED.visibility, EXCLUDED.moderation_status,
         EXCLUDED.source_updated_at);
END;
$$;

-- Saved-search alerts return to the 2026-09-18 definition.
CREATE OR REPLACE FUNCTION directory_enqueue_saved_search_alerts()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.sponsored OR NEW.source_status<>'published' OR NEW.visibility<>'public'
     OR NEW.moderation_status<>'allowed' OR (NEW.expires_at IS NOT NULL AND NEW.expires_at<=now()) THEN
    RETURN NEW;
  END IF;
  WITH matches AS (
    SELECT saved.id,saved.account_party_id
    FROM directory_saved_search saved
    WHERE saved.alerts_enabled AND saved.alert_frequency<>'off'
      AND (saved.canonical_query->>'q' IS NULL OR saved.canonical_query->>'q'='' OR
        NEW.search_vector @@ plainto_tsquery('simple',directory_normalize_text(saved.canonical_query->>'q')) OR
        directory_text_similarity(NEW.search_text,saved.canonical_query->>'q')>=.2)
      AND (saved.canonical_query->>'entityType' IS NULL OR saved.canonical_query->>'entityType'=NEW.entity_kind)
      AND (saved.canonical_query->>'cityId' IS NULL OR saved.canonical_query->>'cityId'=NEW.city_id::text)
      AND CASE WHEN saved.canonical_query->>'professionId' IS NULL THEN TRUE WHEN saved.canonical_query->>'professionId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'professionId')::uuid=ANY(NEW.profession_ids) ELSE FALSE END
      AND CASE WHEN saved.canonical_query->>'instrumentId' IS NULL THEN TRUE WHEN saved.canonical_query->>'instrumentId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'instrumentId')::uuid=ANY(NEW.instrument_ids) ELSE FALSE END
      AND CASE WHEN saved.canonical_query->>'genreId' IS NULL THEN TRUE WHEN saved.canonical_query->>'genreId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'genreId')::uuid=ANY(NEW.genre_ids) ELSE FALSE END
  ), inserted AS (
    INSERT INTO directory_alert_delivery(saved_search_id,result_kind,result_id,result_version,email_status,push_status)
    SELECT matches.id,NEW.entity_kind,NEW.entity_id,NEW.source_version,'disabled','disabled'
    FROM matches
    ON CONFLICT(saved_search_id,result_kind,result_id,result_version) DO NOTHING
    RETURNING id,saved_search_id
  )
  INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at)
  SELECT saved.account_party_id,'directory.saved-search-match','Nueva coincidencia en tu alerta',
    'Hay un nuevo resultado para "'||saved.name||'".','directory_alert',inserted.id::text,FALSE,now()
  FROM inserted JOIN directory_saved_search saved ON saved.id=inserted.saved_search_id;
  UPDATE directory_alert_delivery delivery SET internal_notification_id=notification.id
  FROM notification WHERE delivery.id::text=notification.target_key
    AND notification.target_type='directory_alert' AND delivery.internal_notification_id IS NULL;
  UPDATE directory_saved_search saved SET last_evaluated_at=now()
  WHERE EXISTS (SELECT 1 FROM directory_alert_delivery delivery WHERE delivery.saved_search_id=saved.id AND delivery.result_kind=NEW.entity_kind AND delivery.result_id=NEW.entity_id AND delivery.result_version=NEW.source_version);
  RETURN NEW;
END;
$$;

-- directory_withdraw_profile_surfaces keeps excluding derived rows so an
-- archive during a rollback window cannot strand them in terminal withdrawn.

DROP FUNCTION IF EXISTS directory_profile_listing_sync_trigger();
DROP FUNCTION IF EXISTS directory_refresh_linked_profile_media();
DROP FUNCTION IF EXISTS directory_refresh_store_profile_media();
DROP FUNCTION IF EXISTS directory_guard_classified_derivation();
DROP FUNCTION IF EXISTS directory_reconcile_artist_listings();
DROP FUNCTION IF EXISTS directory_artist_listing_audit();
DROP FUNCTION IF EXISTS directory_sync_profile_listing(UUID);

COMMIT;
