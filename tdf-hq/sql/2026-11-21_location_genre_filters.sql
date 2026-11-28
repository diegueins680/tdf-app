BEGIN;

-- Shared location and genre filters (TAX-FILTER-001) and artist genre
-- suggestions (TAX-GENRE-SUGGESTION-001).
--
-- 1. Geographic reference: the 24 provinces of Ecuador and their capitals plus
--    the main music cities, as public centroids with explicit provenance.
-- 2. Shared SQL predicates used by every filterable surface: genre closure
--    (a parent includes its subgenres), genre ancestors (for facet counts) and
--    location matching (country -> region -> city).
-- 3. Backfill of venue city/country ids from their free-text fields, with a
--    review list for values that do not resolve to exactly one reference.
-- 4. Artist genre suggestions and their review queue.
--
-- Additive and idempotent; the rollback is in
-- 2026-11-21_location_genre_filters_rollback.sql.

-- ---------------------------------------------------------------------------
-- 1. Ecuador provinces (ISO 3166-2:EC) and cities. Pichincha and Quito were
--    seeded on 2026-08-14 and keep their identifiers.
-- ---------------------------------------------------------------------------
INSERT INTO subdivision_reference
  (id,country_id,code,subdivision_type,name_es,name_en,description_es,description_en,
   source_version,last_synced_at,active,sort_order,version)
SELECT seed.id::uuid, country.id, seed.code, 'province', seed.name_es, seed.name_es,
       'Provincia del Ecuador', 'Province of Ecuador',
       'ISO 3166-2 reference; TDF snapshot 2026-11-21', now(), TRUE, seed.sort_order, 1
FROM (VALUES
  ('24000000-0000-4000-8000-000000000101','EC-A','Azuay',20),
  ('24000000-0000-4000-8000-000000000102','EC-B','Bolívar',30),
  ('24000000-0000-4000-8000-000000000103','EC-F','Cañar',40),
  ('24000000-0000-4000-8000-000000000104','EC-C','Carchi',50),
  ('24000000-0000-4000-8000-000000000105','EC-H','Chimborazo',60),
  ('24000000-0000-4000-8000-000000000106','EC-X','Cotopaxi',70),
  ('24000000-0000-4000-8000-000000000107','EC-O','El Oro',80),
  ('24000000-0000-4000-8000-000000000108','EC-E','Esmeraldas',90),
  ('24000000-0000-4000-8000-000000000109','EC-W','Galápagos',100),
  ('24000000-0000-4000-8000-000000000110','EC-G','Guayas',15),
  ('24000000-0000-4000-8000-000000000111','EC-I','Imbabura',110),
  ('24000000-0000-4000-8000-000000000112','EC-L','Loja',120),
  ('24000000-0000-4000-8000-000000000113','EC-R','Los Ríos',130),
  ('24000000-0000-4000-8000-000000000114','EC-M','Manabí',140),
  ('24000000-0000-4000-8000-000000000115','EC-S','Morona Santiago',150),
  ('24000000-0000-4000-8000-000000000116','EC-N','Napo',160),
  ('24000000-0000-4000-8000-000000000117','EC-D','Orellana',170),
  ('24000000-0000-4000-8000-000000000118','EC-Y','Pastaza',180),
  ('24000000-0000-4000-8000-000000000119','EC-SE','Santa Elena',190),
  ('24000000-0000-4000-8000-000000000120','EC-SD','Santo Domingo de los Tsáchilas',200),
  ('24000000-0000-4000-8000-000000000121','EC-U','Sucumbíos',210),
  ('24000000-0000-4000-8000-000000000122','EC-T','Tungurahua',220),
  ('24000000-0000-4000-8000-000000000123','EC-Z','Zamora Chinchipe',230)
) AS seed(id,code,name_es,sort_order)
JOIN country_reference country ON country.alpha2='EC'
ON CONFLICT (country_id,code) DO NOTHING;

INSERT INTO city_reference
  (id,country_id,subdivision_id,code,name_es,name_en,description_es,description_en,
   timezone,latitude,longitude,source_name,source_version,last_synced_at,active,sort_order,version)
SELECT seed.id::uuid, country.id, subdivision.id, seed.code, seed.name_es, seed.name_en,
       NULL, NULL, seed.timezone, seed.latitude, seed.longitude,
       'TDF curated geographic reference', '2026-11-21', now(), TRUE, seed.sort_order, 1
FROM (VALUES
  ('24000000-0000-4000-8000-000000000201','guayaquil-ec-g','EC-G','Guayaquil','Guayaquil','America/Guayaquil',-2.189400,-79.889100,15),
  ('24000000-0000-4000-8000-000000000202','cuenca-ec-a','EC-A','Cuenca','Cuenca','America/Guayaquil',-2.900600,-79.004500,20),
  ('24000000-0000-4000-8000-000000000203','guaranda-ec-b','EC-B','Guaranda','Guaranda','America/Guayaquil',-1.592600,-79.001000,30),
  ('24000000-0000-4000-8000-000000000204','azogues-ec-f','EC-F','Azogues','Azogues','America/Guayaquil',-2.739700,-78.848600,40),
  ('24000000-0000-4000-8000-000000000205','tulcan-ec-c','EC-C','Tulcán','Tulcán','America/Guayaquil',0.811700,-77.717300,50),
  ('24000000-0000-4000-8000-000000000206','riobamba-ec-h','EC-H','Riobamba','Riobamba','America/Guayaquil',-1.663600,-78.654600,60),
  ('24000000-0000-4000-8000-000000000207','latacunga-ec-x','EC-X','Latacunga','Latacunga','America/Guayaquil',-0.935200,-78.615500,70),
  ('24000000-0000-4000-8000-000000000208','machala-ec-o','EC-O','Machala','Machala','America/Guayaquil',-3.258100,-79.955400,80),
  ('24000000-0000-4000-8000-000000000209','esmeraldas-ec-e','EC-E','Esmeraldas','Esmeraldas','America/Guayaquil',0.968200,-79.651700,90),
  ('24000000-0000-4000-8000-000000000210','puerto-baquerizo-moreno-ec-w','EC-W','Puerto Baquerizo Moreno','Puerto Baquerizo Moreno','Pacific/Galapagos',-0.901700,-89.610300,100),
  ('24000000-0000-4000-8000-000000000211','ibarra-ec-i','EC-I','Ibarra','Ibarra','America/Guayaquil',0.351700,-78.122300,110),
  ('24000000-0000-4000-8000-000000000212','loja-ec-l','EC-L','Loja','Loja','America/Guayaquil',-3.993100,-79.204200,120),
  ('24000000-0000-4000-8000-000000000213','babahoyo-ec-r','EC-R','Babahoyo','Babahoyo','America/Guayaquil',-1.802200,-79.534400,130),
  ('24000000-0000-4000-8000-000000000214','portoviejo-ec-m','EC-M','Portoviejo','Portoviejo','America/Guayaquil',-1.054600,-80.454500,140),
  ('24000000-0000-4000-8000-000000000215','macas-ec-s','EC-S','Macas','Macas','America/Guayaquil',-2.308700,-78.111400,150),
  ('24000000-0000-4000-8000-000000000216','tena-ec-n','EC-N','Tena','Tena','America/Guayaquil',-0.993800,-77.812900,160),
  ('24000000-0000-4000-8000-000000000217','puerto-francisco-de-orellana-ec-d','EC-D','Puerto Francisco de Orellana','Puerto Francisco de Orellana','America/Guayaquil',-0.462500,-76.984200,170),
  ('24000000-0000-4000-8000-000000000218','puyo-ec-y','EC-Y','Puyo','Puyo','America/Guayaquil',-1.492400,-78.002400,180),
  ('24000000-0000-4000-8000-000000000219','santa-elena-ec-se','EC-SE','Santa Elena','Santa Elena','America/Guayaquil',-2.226200,-80.859000,190),
  ('24000000-0000-4000-8000-000000000220','santo-domingo-ec-sd','EC-SD','Santo Domingo','Santo Domingo','America/Guayaquil',-0.253000,-79.175400,200),
  ('24000000-0000-4000-8000-000000000221','nueva-loja-ec-u','EC-U','Nueva Loja','Nueva Loja','America/Guayaquil',0.084700,-76.882800,210),
  ('24000000-0000-4000-8000-000000000222','ambato-ec-t','EC-T','Ambato','Ambato','America/Guayaquil',-1.249100,-78.616800,220),
  ('24000000-0000-4000-8000-000000000223','zamora-ec-z','EC-Z','Zamora','Zamora','America/Guayaquil',-4.069200,-78.956700,230),
  ('24000000-0000-4000-8000-000000000224','manta-ec-m','EC-M','Manta','Manta','America/Guayaquil',-0.967700,-80.708900,141),
  ('24000000-0000-4000-8000-000000000225','duran-ec-g','EC-G','Durán','Durán','America/Guayaquil',-2.171000,-79.838000,16),
  ('24000000-0000-4000-8000-000000000226','samborondon-ec-g','EC-G','Samborondón','Samborondón','America/Guayaquil',-1.962800,-79.724700,17),
  ('24000000-0000-4000-8000-000000000227','otavalo-ec-i','EC-I','Otavalo','Otavalo','America/Guayaquil',0.234100,-78.261100,111),
  ('24000000-0000-4000-8000-000000000228','salinas-ec-se','EC-SE','Salinas','Salinas','America/Guayaquil',-2.214600,-80.952000,191),
  ('24000000-0000-4000-8000-000000000229','quevedo-ec-r','EC-R','Quevedo','Quevedo','America/Guayaquil',-1.028600,-79.463500,131),
  ('24000000-0000-4000-8000-000000000230','sangolqui-ec-p','EC-P','Sangolquí','Sangolquí','America/Guayaquil',-0.312600,-78.445400,11),
  ('24000000-0000-4000-8000-000000000231','banos-de-agua-santa-ec-t','EC-T','Baños de Agua Santa','Baños de Agua Santa','America/Guayaquil',-1.396400,-78.424700,221)
) AS seed(id,code,subdivision_code,name_es,name_en,timezone,latitude,longitude,sort_order)
JOIN country_reference country ON country.alpha2='EC'
JOIN subdivision_reference subdivision ON subdivision.country_id=country.id AND subdivision.code=seed.subdivision_code
ON CONFLICT (country_id,code) DO NOTHING;

-- Subgenres under the 2026-08-14 top-level genres, in both languages. A row is
-- skipped when its code exists or when any active genre already carries the
-- same normalized Spanish or English name (an administrator may have added it).
INSERT INTO genre
  (catalog_id,code,parent_id,name_es,name_en,current_slug,sort_order,active,
   workflow_state_id,published_revision,version,source_version)
SELECT catalog.id, seed.code, parent.id, seed.name_es, seed.name_en, seed.code,
       seed.sort_order, TRUE, state.id, 1, 1, 'TDF curated genre reference 2026-11-21'
FROM (VALUES
  ('cumbia','latin','Cumbia','Cumbia',10),
  ('reggaeton','latin','Reggaetón','Reggaeton',20),
  ('salsa','latin','Salsa','Salsa',30),
  ('bachata','latin','Bachata','Bachata',40),
  ('merengue','latin','Merengue','Merengue',50),
  ('bolero','latin','Bolero','Bolero',60),
  ('pasillo','latin','Pasillo','Pasillo',70),
  ('sanjuanito','latin','Sanjuanito','Sanjuanito',80),
  ('bomba-del-chota','latin','Bomba del Chota','Bomba del Chota',90),
  ('trap','hip-hop','Trap','Trap',10),
  ('pop-rock','rock','Pop rock','Pop Rock',10),
  ('rock-en-espanol','rock','Rock en español','Spanish-Language Rock',20),
  ('indie-rock','indie','Indie rock','Indie Rock',10),
  ('house','electronic','House','House',10),
  ('techno','electronic','Techno','Techno',20),
  ('drum-and-bass','electronic','Drum and bass','Drum and Bass',30)
) AS seed(code,parent_code,name_es,name_en,sort_order)
JOIN genre parent ON parent.code = seed.parent_code
JOIN catalog_definition catalog ON catalog.code = 'genres'
JOIN workflow_state state ON state.workflow_id = catalog.workflow_id AND state.code = 'published'
WHERE NOT EXISTS (
  SELECT 1 FROM genre existing
  WHERE existing.active
    AND (directory_normalize_text(existing.name_es) IN (directory_normalize_text(seed.name_es), directory_normalize_text(seed.name_en))
      OR directory_normalize_text(existing.name_en) IN (directory_normalize_text(seed.name_es), directory_normalize_text(seed.name_en))))
ON CONFLICT (code) DO NOTHING;

-- ---------------------------------------------------------------------------
-- 2. Shared predicates. Every surface filters through these so the OR-within /
--    AND-across rule, hierarchy expansion and "missing value is hidden" rule
--    are defined once.
-- ---------------------------------------------------------------------------

-- Selected genres plus every active descendant. NULL or empty -> empty array.
CREATE OR REPLACE FUNCTION taxonomy_genre_closure(selected UUID[])
RETURNS UUID[]
LANGUAGE sql
STABLE
PARALLEL SAFE
AS $$
  WITH RECURSIVE tree(id) AS (
    SELECT item.id FROM genre item WHERE item.id = ANY(coalesce(selected, '{}'::uuid[]))
    UNION
    SELECT child.id FROM genre child JOIN tree ON child.parent_id = tree.id WHERE child.active
  )
  SELECT coalesce(array_agg(id ORDER BY id), '{}'::uuid[]) FROM tree
$$;

-- The given genres plus all their ancestors; a subgenre counts for its parent.
CREATE OR REPLACE FUNCTION taxonomy_genre_ancestors(genre_ids UUID[])
RETURNS UUID[]
LANGUAGE sql
STABLE
PARALLEL SAFE
AS $$
  WITH RECURSIVE chain(id, parent_id, depth) AS (
    SELECT item.id, item.parent_id, 0 FROM genre item WHERE item.id = ANY(coalesce(genre_ids, '{}'::uuid[]))
    UNION
    SELECT parent.id, parent.parent_id, chain.depth + 1
    FROM genre parent JOIN chain ON parent.id = chain.parent_id
    WHERE chain.depth < 16
  )
  SELECT coalesce(array_agg(DISTINCT id), '{}'::uuid[]) FROM chain
$$;

-- An empty selection matches everything. A non-empty selection only matches an
-- item that carries at least one genre inside the (already expanded) closure.
CREATE OR REPLACE FUNCTION taxonomy_genres_match(item_genres UUID[], selected_closure UUID[])
RETURNS BOOLEAN
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT cardinality(coalesce(selected_closure, '{}'::uuid[])) = 0
      OR coalesce(item_genres, '{}'::uuid[]) && selected_closure
$$;

-- Country, region and city are ANDed; values inside each are ORed. A region or
-- country is derived from the item's city when the item does not carry it
-- directly. A missing value never matches an active filter.
CREATE OR REPLACE FUNCTION taxonomy_location_matches(
  item_city_id UUID, item_country_id UUID,
  country_ids UUID[], region_ids UUID[], city_ids UUID[])
RETURNS BOOLEAN
LANGUAGE sql
STABLE
PARALLEL SAFE
AS $$
  SELECT
    (cardinality(coalesce(country_ids, '{}'::uuid[])) = 0
      OR coalesce(coalesce(item_country_id,
           (SELECT city.country_id FROM city_reference city WHERE city.id = item_city_id)) = ANY(country_ids), FALSE))
    AND (cardinality(coalesce(region_ids, '{}'::uuid[])) = 0
      OR coalesce((SELECT city.subdivision_id FROM city_reference city WHERE city.id = item_city_id) = ANY(region_ids), FALSE))
    AND (cardinality(coalesce(city_ids, '{}'::uuid[])) = 0
      OR coalesce(item_city_id = ANY(city_ids), FALSE))
$$;

-- Resolve a free-text city to exactly one active reference (Spanish or English
-- name, accent- and case-insensitive); ambiguous or unknown text -> NULL.
CREATE OR REPLACE FUNCTION taxonomy_city_from_text(raw_city TEXT, raw_country_id UUID)
RETURNS UUID
LANGUAGE sql
STABLE
PARALLEL SAFE
AS $$
  SELECT (array_agg(city.id))[1]
  FROM city_reference city
  WHERE city.active
    AND btrim(coalesce(raw_city, '')) <> ''
    AND (raw_country_id IS NULL OR city.country_id = raw_country_id)
    AND directory_normalize_text(raw_city) IN (directory_normalize_text(city.name_es), directory_normalize_text(city.name_en))
  HAVING count(*) = 1
$$;

-- ---------------------------------------------------------------------------
-- 3. Backfill. Venues gain city/country ids from their text only when the text
--    resolves to exactly one reference; everything else goes to the review list.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS taxonomy_backfill_review (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  entity_table TEXT NOT NULL CHECK (entity_table IN ('venue','artist_profile','social_artist_profile')),
  entity_id TEXT NOT NULL,
  field_name TEXT NOT NULL CHECK (field_name IN ('city','country','genre')),
  raw_value TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  resolved_at TIMESTAMPTZ,
  UNIQUE (entity_table, entity_id, field_name)
);

UPDATE venue SET country_id = country.id, updated_at = now()
FROM country_reference country
WHERE venue.country_id IS NULL
  AND venue.country_code IS NOT NULL
  AND upper(btrim(venue.country_code)) = country.alpha2
  AND country.active;

UPDATE venue SET city_id = taxonomy_city_from_text(venue.city, venue.country_id), updated_at = now()
WHERE venue.city_id IS NULL
  AND taxonomy_city_from_text(venue.city, venue.country_id) IS NOT NULL;

INSERT INTO taxonomy_backfill_review (entity_table, entity_id, field_name, raw_value)
SELECT 'venue', venue.id::text, 'city', btrim(venue.city)
FROM venue
WHERE venue.city_id IS NULL AND btrim(coalesce(venue.city, '')) <> ''
ON CONFLICT (entity_table, entity_id, field_name) DO NOTHING;

INSERT INTO taxonomy_backfill_review (entity_table, entity_id, field_name, raw_value)
SELECT 'artist_profile', artist.id::text, 'city', btrim(artist.city)
FROM artist_profile artist
WHERE btrim(coalesce(artist.city, '')) <> ''
  AND taxonomy_city_from_text(artist.city, artist.country_id) IS NULL
ON CONFLICT (entity_table, entity_id, field_name) DO NOTHING;

-- The venue backfill above queues the deferred search sync trigger
-- (trg_directory_sync_venue_search) for every row it changes, and PostgreSQL
-- refuses to build an index on a table with pending trigger events. Run the
-- queued syncs now. Production release 11 (2026-11-27) applied exactly this
-- statement at this position.
SET CONSTRAINTS ALL IMMEDIATE;
CREATE INDEX IF NOT EXISTS ix_venue_taxonomy_location ON venue (city_id, country_id);

-- ---------------------------------------------------------------------------
-- 4. Artist genre suggestions. A suggestion that passes every automatic check
--    is approved on creation (resolved_genre_id set, reviewer NULL); one the
--    checks cannot settle stays pending for an administrator.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS genre_suggestion (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  suggested_by_party_id BIGINT NOT NULL REFERENCES party(id),
  parent_genre_id UUID REFERENCES genre(id),
  submitted_name TEXT NOT NULL CHECK (length(btrim(submitted_name)) BETWEEN 2 AND 60),
  name_es TEXT NOT NULL CHECK (length(btrim(name_es)) BETWEEN 2 AND 60),
  name_en TEXT NOT NULL CHECK (length(btrim(name_en)) BETWEEN 2 AND 60),
  normalized_key TEXT NOT NULL CHECK (normalized_key ~ '^[a-z0-9]+$'),
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','approved','rejected','merged')),
  review_reasons TEXT[] NOT NULL DEFAULT '{}',
  resolved_genre_id UUID REFERENCES genre(id),
  reviewed_by_party_id BIGINT REFERENCES party(id),
  reviewed_at TIMESTAMPTZ,
  review_note TEXT CHECK (review_note IS NULL OR length(review_note) <= 500),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT genre_suggestion_resolution_check CHECK (
    (status = 'pending' AND resolved_genre_id IS NULL AND reviewed_at IS NULL)
    OR (status IN ('approved','merged') AND resolved_genre_id IS NOT NULL AND reviewed_at IS NOT NULL)
    OR (status = 'rejected' AND resolved_genre_id IS NULL AND reviewed_at IS NOT NULL AND reviewed_by_party_id IS NOT NULL))
);
-- One open suggestion per normalized name, whoever proposed it.
CREATE UNIQUE INDEX IF NOT EXISTS genre_suggestion_pending_key_uidx
  ON genre_suggestion (normalized_key) WHERE status = 'pending';
CREATE INDEX IF NOT EXISTS ix_genre_suggestion_queue ON genre_suggestion (status, created_at);
CREATE INDEX IF NOT EXISTS ix_genre_suggestion_author ON genre_suggestion (suggested_by_party_id, created_at DESC);

-- Only forward transitions out of pending; decided suggestions are immutable.
CREATE OR REPLACE FUNCTION genre_suggestion_guard_transition()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  IF OLD.status <> 'pending' THEN
    RAISE EXCEPTION 'genre suggestion % is already %', OLD.id, OLD.status;
  END IF;
  IF NEW.status NOT IN ('approved','rejected','merged') THEN
    RAISE EXCEPTION 'genre suggestion % cannot move from pending to %', OLD.id, NEW.status;
  END IF;
  IF NEW.suggested_by_party_id <> OLD.suggested_by_party_id OR NEW.submitted_name <> OLD.submitted_name THEN
    RAISE EXCEPTION 'genre suggestion % authorship is immutable', OLD.id;
  END IF;
  NEW.updated_at := now();
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS genre_suggestion_transition_trigger ON genre_suggestion;
CREATE TRIGGER genre_suggestion_transition_trigger
  BEFORE UPDATE ON genre_suggestion
  FOR EACH ROW EXECUTE FUNCTION genre_suggestion_guard_transition();

COMMIT;
