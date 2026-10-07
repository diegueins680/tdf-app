-- Integration assertions for 2026-10-07_directory_artist_derived_listings and
-- its backfill. Runs against a disposable database that already has both
-- migrations applied. Every block raises on the first violated expectation.
\set ON_ERROR_STOP on

CREATE OR REPLACE FUNCTION pg_temp.expect(condition BOOLEAN, message TEXT)
RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  IF condition IS DISTINCT FROM TRUE THEN
    RAISE EXCEPTION 'assertion failed: %', message;
  END IF;
END
$$;

-- Fixture ---------------------------------------------------------------------
INSERT INTO party (id, display_name, is_org, created_at) VALUES
  (910001, 'Listing Owner', FALSE, now()),
  (910002, 'Second Owner', FALSE, now()),
  (910003, 'Venue Org', TRUE, now())
ON CONFLICT (id) DO NOTHING;

CREATE OR REPLACE FUNCTION pg_temp.make_profile(
  slug_value TEXT, kind_value TEXT, name_value TEXT, party_value BIGINT,
  bio_value TEXT DEFAULT 'Proyecto musical de prueba con biografía suficientemente larga.')
RETURNS UUID LANGUAGE plpgsql AS $$
DECLARE profile_id_value UUID := gen_random_uuid();
BEGIN
  INSERT INTO directory_profile (id, subject_party_id, profile_kind, public_name, slug, bio,
    portfolio, profile_status, visibility, moderation_status, completeness_score)
  VALUES (profile_id_value, party_value, kind_value, name_value, slug_value, bio_value,
    '[]'::jsonb, 'draft', 'public', 'allowed', .8);
  INSERT INTO directory_profile_location (profile_id, country_id, city_id, public_latitude, public_longitude, precision, primary_location)
  VALUES (profile_id_value, '1cb3600a-c7e3-4f5f-8e67-55db001de6d5', '24000000-0000-4000-8000-000000000002',
    -0.180653, -78.467834, 'city', TRUE);
  INSERT INTO directory_profile_genre (profile_id, genre_id, sort_order)
  VALUES (profile_id_value, '2109e4ab-c2b5-493a-aa5e-97c00dd6fa9c', 0);
  RETURN profile_id_value;
END
$$;

CREATE OR REPLACE FUNCTION pg_temp.publish(profile_id_value UUID)
RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  UPDATE directory_profile SET profile_status = 'published', published_at = coalesce(published_at, now())
  WHERE id = profile_id_value;
  PERFORM directory_refresh_profile_search(profile_id_value);
END
$$;

CREATE OR REPLACE FUNCTION pg_temp.derived(profile_id_value UUID)
RETURNS classified LANGUAGE SQL AS $$
  SELECT * FROM classified WHERE source_profile_id = profile_id_value;
$$;

CREATE OR REPLACE FUNCTION pg_temp.derived_count(profile_id_value UUID)
RETURNS BIGINT LANGUAGE SQL AS $$
  SELECT count(*) FROM classified WHERE source_profile_id = profile_id_value;
$$;

CREATE OR REPLACE FUNCTION pg_temp.public_listing(profile_id_value UUID)
RETURNS directory_public_search_document LANGUAGE SQL AS $$
  SELECT * FROM directory_public_search_document
  WHERE entity_kind = 'classified' AND source_profile_id = profile_id_value;
$$;

-- Image resolution ------------------------------------------------------------
DO $images$
DECLARE
  profile_id_value UUID := pg_temp.make_profile('img-priority-artist', 'artist', 'Image Priority', 910001);
  artist_id BIGINT;
BEGIN
  PERFORM pg_temp.expect(directory_profile_preview_image_url(profile_id_value) IS NULL,
    'placeholder (NULL) only when no valid media exists');

  UPDATE directory_profile SET portfolio = jsonb_build_array(
    jsonb_build_object('itemType','image','title','Bad','url','javascript:alert(1)'),
    jsonb_build_object('itemType','image','title','Second','url','https://cdn.example.test/second.jpg'),
    jsonb_build_object('itemType','image','title','Featured','url','https://cdn.example.test/featured.jpg','featured',true))
  WHERE id = profile_id_value;
  PERFORM pg_temp.expect(directory_profile_preview_image_url(profile_id_value) = 'https://cdn.example.test/featured.jpg',
    'featured portfolio image outranks the first portfolio image');

  UPDATE directory_profile SET portfolio = jsonb_build_array(
    jsonb_build_object('itemType','image','title','Bad','url','javascript:alert(1)'),
    jsonb_build_object('itemType','image','title','First valid','url','https://cdn.example.test/first.jpg'))
  WHERE id = profile_id_value;
  PERFORM pg_temp.expect(directory_profile_preview_image_url(profile_id_value) = 'https://cdn.example.test/first.jpg',
    'first valid portfolio image is used and invalid media is skipped');

  INSERT INTO artist_profile (artist_party_id, slug, hero_image_url, created_at)
  VALUES (910001, 'img-priority-legacy', 'https://drive.google.com/file/d/AbC_123-x/view?usp=sharing', now())
  RETURNING id INTO artist_id;
  INSERT INTO directory_legacy_link (profile_id, legacy_kind, legacy_id, source_table)
  VALUES (profile_id_value, 'artist_profile', artist_id::text, 'artist_profile');
  PERFORM pg_temp.expect(directory_profile_preview_image_url(profile_id_value) = 'https://drive.google.com/uc?export=view&id=AbC_123-x',
    'primary linked artist image outranks portfolio images and Drive viewer links become image URLs');

  UPDATE directory_profile SET cover_image_url = 'https://cdn.example.test/cover.jpg' WHERE id = profile_id_value;
  PERFORM pg_temp.expect(directory_profile_preview_image_url(profile_id_value) = 'https://cdn.example.test/cover.jpg',
    'designated cover has the highest priority');

  BEGIN
    UPDATE directory_profile SET cover_image_url = 'javascript:alert(1)' WHERE id = profile_id_value;
    RAISE EXCEPTION 'unsafe cover accepted';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
END
$images$;

DO $avatar_logo$
DECLARE
  profile_id_value UUID := pg_temp.make_profile('img-logo-band', 'band', 'Logo Band', 910002);
  social_id BIGINT;
BEGIN
  UPDATE directory_profile SET portfolio = jsonb_build_array(
    jsonb_build_object('itemType','image','title','Gallery','url','/assets/serve/gallery.jpg'))
  WHERE id = profile_id_value;
  INSERT INTO social_artist_profile (name, avatar_url, created_at, updated_at)
  VALUES ('Logo Band', 'https://cdn.example.test/avatar.png', now(), now()) RETURNING id INTO social_id;
  INSERT INTO directory_legacy_link (profile_id, legacy_kind, legacy_id, source_table)
  VALUES (profile_id_value, 'social_artist_profile', social_id::text, 'social_artist_profile');
  PERFORM pg_temp.expect(directory_profile_preview_image_url(profile_id_value) = 'https://cdn.example.test/avatar.png',
    'avatar/logo outranks the first gallery image');
END
$avatar_logo$;

-- Creation --------------------------------------------------------------------
DO $creation$
DECLARE
  artist_id UUID := pg_temp.make_profile('create-artist', 'artist', 'Create Artist', 910001);
  second_id UUID := pg_temp.make_profile('create-artist-two', 'band', 'Second Project', 910001);
  venue_id UUID := pg_temp.make_profile('create-venue', 'venue', 'Create Venue', 910003);
  private_id UUID := pg_temp.make_profile('create-private', 'artist', 'Private Artist', 910002);
  short_id UUID := pg_temp.make_profile('create-short', 'artist', 'Ana', 910002, 'Corta');
  listing classified;
BEGIN
  PERFORM pg_temp.expect(pg_temp.derived_count(artist_id) = 0, 'draft artist profile is not advertised');

  PERFORM pg_temp.publish(artist_id);
  PERFORM pg_temp.expect(pg_temp.derived_count(artist_id) = 1, 'publishing an artist creates exactly one listing');
  listing := pg_temp.derived(artist_id);
  PERFORM pg_temp.expect(listing.status = 'published' AND listing.author_profile_id = artist_id,
    'derived listing is published and authored by its profile');
  PERFORM pg_temp.expect((SELECT code FROM classified_category WHERE id = listing.category_id) = 'artist-profile',
    'derived listing uses the formal artist-profile category');
  PERFORM pg_temp.expect(listing.title = 'Create Artist' AND listing.slug = 'create-artist-perfil',
    'title and slug derive from the profile');
  PERFORM pg_temp.expect(listing.expires_at = 'infinity'::timestamptz, 'derived listing never expires');
  PERFORM pg_temp.expect((pg_temp.public_listing(artist_id)).entity_id = listing.id::text,
    'derived listing appears in public search');
  PERFORM pg_temp.expect((pg_temp.public_listing(artist_id)).expires_at IS NULL,
    'public projection exposes no artificial expiry');

  PERFORM pg_temp.publish(second_id);
  PERFORM pg_temp.expect(pg_temp.derived_count(second_id) = 1 AND pg_temp.derived_count(artist_id) = 1,
    'each artist profile of one account owns its own listing');

  PERFORM pg_temp.publish(venue_id);
  PERFORM pg_temp.expect(pg_temp.derived_count(venue_id) = 0, 'non-artist profiles are not advertised');

  UPDATE directory_profile SET visibility = 'private' WHERE id = private_id;
  PERFORM pg_temp.publish(private_id);
  PERFORM pg_temp.expect(pg_temp.derived_count(private_id) = 0, 'private profiles are not advertised');

  PERFORM pg_temp.publish(short_id);
  listing := pg_temp.derived(short_id);
  PERFORM pg_temp.expect(listing.title = 'Ana · Artista' AND length(listing.description) >= 20,
    'short names and bios still satisfy listing constraints');
END
$creation$;

-- Synchronization -------------------------------------------------------------
DO $sync$
DECLARE
  artist_id UUID := pg_temp.make_profile('sync-artist', 'artist', 'Sync Artist', 910001);
  listing classified;
  document directory_public_search_document;
BEGIN
  PERFORM pg_temp.publish(artist_id);

  UPDATE directory_profile SET public_name = 'Sync Artist Renamed',
    bio = 'Nueva biografía del artista sincronizada con su anuncio derivado.',
    cover_image_url = 'https://cdn.example.test/new-cover.jpg'
  WHERE id = artist_id;
  listing := pg_temp.derived(artist_id);
  document := pg_temp.public_listing(artist_id);
  PERFORM pg_temp.expect(listing.title = 'Sync Artist Renamed', 'name propagates');
  PERFORM pg_temp.expect(listing.description LIKE 'Nueva biografía%', 'bio propagates');
  PERFORM pg_temp.expect(document.image_url = 'https://cdn.example.test/new-cover.jpg', 'image propagates');

  DELETE FROM directory_profile_genre WHERE profile_id = artist_id;
  INSERT INTO directory_profile_genre (profile_id, genre_id, sort_order)
  VALUES (artist_id, 'ea0ee25a-326a-4174-9682-72d063caf6f9', 0);
  INSERT INTO directory_profile_instrument (profile_id, instrument_id, sort_order)
  VALUES (artist_id, 'c9444759-fad8-4883-911e-c4cddd2ad721', 0);
  PERFORM directory_refresh_profile_search(artist_id);
  document := pg_temp.public_listing(artist_id);
  PERFORM pg_temp.expect(document.genre_ids = ARRAY['ea0ee25a-326a-4174-9682-72d063caf6f9'::uuid], 'genres propagate');
  PERFORM pg_temp.expect(document.instrument_ids = ARRAY['c9444759-fad8-4883-911e-c4cddd2ad721'::uuid], 'instruments propagate');

  BEGIN
    UPDATE classified SET title = 'Edited by hand' WHERE source_profile_id = artist_id;
    RAISE EXCEPTION 'derived content was edited independently';
  EXCEPTION WHEN insufficient_privilege THEN NULL;
  END;
  BEGIN
    UPDATE classified SET status = 'paused' WHERE source_profile_id = artist_id;
    RAISE EXCEPTION 'derived status was changed independently';
  EXCEPTION WHEN insufficient_privilege THEN NULL;
  END;
END
$sync$;

-- Privacy ---------------------------------------------------------------------
DO $privacy$
DECLARE
  artist_id UUID := pg_temp.make_profile('privacy-artist', 'artist', 'Privacy Artist', 910002);
  location_id UUID;
  document directory_public_search_document;
BEGIN
  SELECT id INTO location_id FROM directory_profile_location WHERE profile_id = artist_id;
  UPDATE directory_profile_location SET precision = 'sector', sector_label = 'La Floresta' WHERE id = location_id;
  INSERT INTO directory_private_location (profile_location_id, exact_address, private_latitude, private_longitude, access_reason, created_by)
  VALUES (location_id, 'Calle Secreta 123', -0.2101, -78.4888, 'Dirección privada solo para reservas confirmadas', 910002);
  PERFORM pg_temp.publish(artist_id);
  document := pg_temp.public_listing(artist_id);
  PERFORM pg_temp.expect(document.location_precision = 'city', 'sector precision is reduced to city');
  PERFORM pg_temp.expect(document.public_latitude = -0.180653 AND document.public_longitude = -78.467834,
    'listing exposes the city centroid, never private coordinates');
  PERFORM pg_temp.expect(position('Secreta' in document.search_text) = 0 AND position('floresta' in document.search_text) = 0,
    'private address and sector labels never reach the listing');

  UPDATE directory_profile_location SET precision = 'country', city_id = NULL, public_latitude = NULL, public_longitude = NULL,
    sector_label = NULL WHERE id = location_id;
  PERFORM directory_refresh_profile_search(artist_id);
  document := pg_temp.public_listing(artist_id);
  PERFORM pg_temp.expect(document.city_id IS NULL AND document.public_latitude IS NULL AND document.location_precision = 'country',
    'country-only profiles stay country-only in the listing');
END
$privacy$;

-- Lifecycle -------------------------------------------------------------------
DO $lifecycle$
DECLARE
  artist_id UUID := pg_temp.make_profile('lifecycle-artist', 'artist', 'Lifecycle Artist', 910001);
  original_id UUID;
BEGIN
  PERFORM pg_temp.publish(artist_id);
  original_id := (pg_temp.derived(artist_id)).id;

  UPDATE directory_profile SET profile_status = 'paused' WHERE id = artist_id;
  PERFORM pg_temp.expect((pg_temp.derived(artist_id)).status = 'paused', 'unpublishing pauses the listing');
  PERFORM pg_temp.expect(pg_temp.public_listing(artist_id) IS NULL, 'paused listing leaves public discovery');

  PERFORM pg_temp.publish(artist_id);
  PERFORM pg_temp.expect((pg_temp.derived(artist_id)).id = original_id AND pg_temp.derived_count(artist_id) = 1,
    'republishing reuses the same listing');
  PERFORM pg_temp.expect((pg_temp.public_listing(artist_id)).entity_id = original_id::text, 'republished listing is public again');

  UPDATE directory_profile SET visibility = 'unlisted' WHERE id = artist_id;
  PERFORM pg_temp.expect(pg_temp.public_listing(artist_id) IS NULL, 'unlisted profile hides its listing');
  UPDATE directory_profile SET visibility = 'public' WHERE id = artist_id;

  UPDATE directory_profile SET moderation_status = 'blocked' WHERE id = artist_id;
  PERFORM pg_temp.expect((pg_temp.derived(artist_id)).status = 'paused', 'moderated profile pauses its listing');
  UPDATE directory_profile SET moderation_status = 'allowed' WHERE id = artist_id;

  UPDATE directory_profile SET profile_status = 'archived', archived_at = now() WHERE id = artist_id;
  PERFORM pg_temp.expect((pg_temp.derived(artist_id)).status = 'paused', 'archived (soft-deleted) profile pauses, not withdraws');
  PERFORM pg_temp.expect(pg_temp.public_listing(artist_id) IS NULL, 'archived profile listing is not public');
  UPDATE directory_profile SET profile_status = 'published' WHERE id = artist_id;
  PERFORM directory_refresh_profile_search(artist_id);
  PERFORM pg_temp.expect((pg_temp.derived(artist_id)).id = original_id AND (pg_temp.derived(artist_id)).status = 'published',
    'restoring an archived profile restores the same listing');

  UPDATE directory_profile SET profile_kind = 'person' WHERE id = artist_id;
  PERFORM pg_temp.expect((pg_temp.derived(artist_id)).status = 'paused', 'profile no longer an artist pauses its listing');
  UPDATE directory_profile SET profile_kind = 'artist' WHERE id = artist_id;
  PERFORM pg_temp.expect((pg_temp.derived(artist_id)).status = 'published', 'artist again republishes the same listing');

  -- Moderators can still take the listing down and the profile never revives it.
  UPDATE classified SET status = 'moderated' WHERE id = original_id;
  UPDATE directory_profile SET bio = 'Biografía actualizada luego de la moderación del anuncio.' WHERE id = artist_id;
  PERFORM pg_temp.expect((pg_temp.derived(artist_id)).status = 'moderated', 'moderator decisions are preserved');
  PERFORM pg_temp.expect(pg_temp.public_listing(artist_id) IS NULL, 'moderated listing stays hidden');
END
$lifecycle$;

-- Idempotency -----------------------------------------------------------------
DO $idempotency$
DECLARE
  artist_id UUID := pg_temp.make_profile('idempotent-artist', 'artist', 'Idempotent Artist', 910002);
  category_value UUID := (SELECT id FROM classified_category WHERE code = 'artist-profile');
BEGIN
  PERFORM pg_temp.publish(artist_id);
  PERFORM directory_sync_profile_listing(artist_id);
  PERFORM directory_sync_profile_listing(artist_id);
  PERFORM pg_temp.publish(artist_id);
  PERFORM directory_reconcile_artist_listings();
  PERFORM directory_reconcile_artist_listings();
  PERFORM pg_temp.expect(pg_temp.derived_count(artist_id) = 1, 'retries, republish and reconcile never duplicate');

  BEGIN
    INSERT INTO classified (author_profile_id, source_profile_id, category_id, title, slug, description)
    VALUES (artist_id, artist_id, category_value, 'Duplicate listing', 'duplicate-listing-x',
      'Intento manual de duplicar el anuncio derivado del artista.');
    RAISE EXCEPTION 'manual derived listing insert accepted';
  EXCEPTION WHEN insufficient_privilege THEN NULL;
  END;

  BEGIN
    PERFORM set_config('tdf.directory_listing_sync', 'on', true);
    INSERT INTO classified (author_profile_id, source_profile_id, category_id, title, slug, description)
    VALUES (artist_id, artist_id, category_value, 'Duplicate listing', 'duplicate-listing-y',
      'Intento de duplicar el anuncio derivado aun con la bandera de sincronización.');
    RAISE EXCEPTION 'second derived listing accepted';
  EXCEPTION WHEN unique_violation THEN
    PERFORM set_config('tdf.directory_listing_sync', 'off', true);
  END;

  BEGIN
    INSERT INTO classified (author_profile_id, category_id, title, slug, description)
    VALUES (artist_id, category_value, 'Manual in derived category', 'manual-derived-category',
      'Un anuncio manual no puede usar la categoría derivada de perfiles de artista.');
    RAISE EXCEPTION 'manual listing accepted in the derived category';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
END
$idempotency$;

-- Backfill --------------------------------------------------------------------
ALTER TABLE directory_profile DISABLE TRIGGER directory_profile_artist_listing_sync_trigger;
INSERT INTO directory_profile (id, subject_party_id, profile_kind, public_name, slug, bio, profile_status,
  visibility, moderation_status, completeness_score, published_at)
VALUES
  ('a0000000-0000-4000-8000-000000000001', 910001, 'artist', 'Legacy Artist', 'legacy-artist',
   'Artista histórico creado antes de los anuncios derivados.', 'published', 'public', 'allowed', .8, now()),
  ('a0000000-0000-4000-8000-000000000002', 910002, 'band', 'Legacy Band', 'legacy-band',
   'Banda histórica creada antes de los anuncios derivados.', 'published', 'public', 'allowed', .8, now()),
  ('a0000000-0000-4000-8000-000000000003', 910003, 'studio', 'TDF Records', 'tdf-records-estudio',
   'TDF Records es un estudio de grabación en Quito.', 'published', 'public', 'allowed', .85, now()),
  ('a0000000-0000-4000-8000-000000000004', 910003, 'venue', 'Domo del Pululahua', 'domo-del-pululahua',
   'Domo del Pululahua es un venue para conciertos.', 'published', 'public', 'allowed', .85, now()),
  ('a0000000-0000-4000-8000-000000000005', 910001, 'artist', 'Legacy Draft', 'legacy-draft',
   'Artista histórico que nunca se publicó en el directorio.', 'draft', 'public', 'allowed', .8, NULL);
ALTER TABLE directory_profile ENABLE TRIGGER directory_profile_artist_listing_sync_trigger;
SELECT directory_refresh_profile_search(id) FROM directory_profile
WHERE id IN ('a0000000-0000-4000-8000-000000000003','a0000000-0000-4000-8000-000000000004');
-- The two legacy artists have no derived listing yet, exactly like
-- pre-migration production rows. A lookalike manual listing must be reported.
INSERT INTO classified (author_profile_id, category_id, title, slug, description, status)
SELECT 'a0000000-0000-4000-8000-000000000001', id, 'Legacy Artist', 'legacy-artist-manual',
  'Anuncio manual parecido al perfil del artista, creado antes del cambio.', 'published'
FROM classified_category WHERE code = 'offering-services';
