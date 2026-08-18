-- Restore the original refresh behavior without deleting image URLs already
-- projected. Application rollback is therefore non-destructive.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

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
    profile.bio, NULL, location.city_id, city.name_es, country.alpha2,
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

DROP FUNCTION IF EXISTS directory_profile_primary_image_url(JSONB);

COMMIT;
