-- Daily venue discovery for Quito, Guayaquil, Cuenca, Bogotá, Cali and
-- Medellín. Registers the six cities, admits schema.org linked-data sources,
-- and seeds the venue and platform sources verified on 2026-11-14.
-- Imports stay private drafts: no publication approval is created here.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

ALTER TABLE event_discovery_source
  DROP CONSTRAINT IF EXISTS event_discovery_source_type_check;
ALTER TABLE event_discovery_source
  ADD CONSTRAINT event_discovery_source_type_check
  CHECK (source_type IN ('ticketmaster', 'buenplan', 'ical', 'json', 'jsonld', 'web'));

INSERT INTO event_city (name, normalized_name, country_code, time_zone, created_at, updated_at)
VALUES
  ('Quito', 'quito', 'EC', 'America/Guayaquil', now(), now()),
  ('Guayaquil', 'guayaquil', 'EC', 'America/Guayaquil', now(), now()),
  ('Cuenca', 'cuenca', 'EC', 'America/Guayaquil', now(), now()),
  ('Bogotá', 'bogotá', 'CO', 'America/Bogota', now(), now()),
  ('Cali', 'cali', 'CO', 'America/Bogota', now(), now()),
  ('Medellín', 'medellín', 'CO', 'America/Bogota', now(), now())
ON CONFLICT (normalized_name, country_code) DO UPDATE
SET time_zone = COALESCE(event_city.time_zone, EXCLUDED.time_zone),
    updated_at = now();

-- Venue-owned calendars and schema.org event pages. A source with a city
-- covers one venue; a source without one is a platform whose events must name
-- a registered city. Existing rows are left exactly as an operator set them.
INSERT INTO event_discovery_source
  (source_key, name, source_type, feed_url, city_id, enabled, priority, configuration,
   consecutive_failures, created_at, updated_at)
SELECT seed.source_key, seed.name, seed.source_type, seed.feed_url, city.id, TRUE,
       seed.priority, seed.configuration, 0, now(), now()
FROM (
  VALUES
    ('teatro-sucre-ical', 'Teatro Nacional Sucre', 'ical',
     'https://teatrosucre.com/events/?ical=1', 'quito', 'EC', 400, NULL),
    ('teatro-colon-bogota-ical', 'Teatro Colón', 'ical',
     'https://www.eneldelia.gov.co/events/?ical=1', 'bogotá', 'CO', 400, NULL),
    ('teatro-centro-de-arte-jsonld', 'Teatro Centro de Arte', 'jsonld',
     'https://www.teatrocentrodearte.org/eventos', 'guayaquil', 'EC', 400,
     '{"venueName":"Teatro Centro de Arte"}'),
    ('tuboleta-movistar-arena', 'Movistar Arena (TuBoleta)', 'jsonld',
     'https://www.tuboleta.com/es/venue/el-corazon-de-bogota-arena', 'bogotá', 'CO', 410,
     '{"linkPrefix":"https://www.tuboleta.com/es/eventos/","maxPages":150,"venueName":"El Corazón de Bogotá - Arena","dateOnlyLocalTime":"07:00"}'),
    ('tuboleta-teatro-mayor', 'Teatro Mayor Julio Mario Santo Domingo (TuBoleta)', 'jsonld',
     'https://www.tuboleta.com/es/venue/teatro-mayor-julio-mario-santo-domingo', 'bogotá', 'CO', 410,
     '{"linkPrefix":"https://www.tuboleta.com/es/eventos/","maxPages":150,"venueName":"Teatro Mayor Julio Mario Santo Domingo","dateOnlyLocalTime":"07:00"}'),
    ('tuboleta-jorge-eliecer-gaitan', 'Teatro Jorge Eliécer Gaitán (TuBoleta)', 'jsonld',
     'https://www.tuboleta.com/es/venue/jorge-eliecer-gaitan', 'bogotá', 'CO', 410,
     '{"linkPrefix":"https://www.tuboleta.com/es/eventos/","maxPages":150,"venueName":"Teatro Jorge Eliécer Gaitán","dateOnlyLocalTime":"07:00"}'),
    ('tuboleta-daviarena', 'DAVIarena (TuBoleta)', 'jsonld',
     'https://www.tuboleta.com/es/venue/daviarena', 'medellín', 'CO', 410,
     '{"linkPrefix":"https://www.tuboleta.com/es/eventos/","maxPages":150,"venueName":"DAVIarena","dateOnlyLocalTime":"07:00"}'),
    ('tuboleta-teatro-metropolitano', 'Teatro Metropolitano (TuBoleta)', 'jsonld',
     'https://www.tuboleta.com/es/venue/teatro-metropolitano', 'medellín', 'CO', 410,
     '{"linkPrefix":"https://www.tuboleta.com/es/eventos/","maxPages":150,"venueName":"Teatro Metropolitano","dateOnlyLocalTime":"07:00"}')
) AS seed(source_key, name, source_type, feed_url, city_name, country_code, priority, configuration)
JOIN event_city city
  ON city.normalized_name = seed.city_name AND city.country_code = seed.country_code
ON CONFLICT (source_key) DO NOTHING;

INSERT INTO event_discovery_source
  (source_key, name, source_type, feed_url, city_id, enabled, priority, configuration,
   consecutive_failures, created_at, updated_at)
VALUES
  ('meet2go-jsonld', 'Meet2Go (eventos)', 'jsonld',
   'https://www.meet2go.com/seo/sitemap.xml', NULL, TRUE, 420,
   '{"linkPrefix":"https://www.meet2go.com/ev/","rewriteFrom":"https://www.meet2go.com/ev/","rewriteTo":"https://www.meet2go.com/seo/ev/","maxPages":300}',
   0, now(), now()),
  ('latiquetera-jsonld', 'La Tiquetera', 'jsonld',
   'https://latiquetera.com/', NULL, TRUE, 420,
   '{"linkPrefix":"https://latiquetera.com/event/","maxPages":120}',
   0, now(), now())
ON CONFLICT (source_key) DO NOTHING;

COMMIT;
