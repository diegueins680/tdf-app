-- Stops the sources seeded by 2026-11-14_latam_venue_discovery_sources.sql.
-- Imported events, references, run history and the six registered cities are
-- retained. The 'jsonld' type stays admitted because disabled rows still use
-- it; removing those rows would orphan their run and reference history.
BEGIN;
SET LOCAL lock_timeout = '10s';

UPDATE event_discovery_source
SET enabled = FALSE,
    updated_at = now()
WHERE source_key IN (
    'teatro-sucre-ical',
    'teatro-colon-bogota-ical',
    'teatro-centro-de-arte-jsonld',
    'tuboleta-movistar-arena',
    'tuboleta-teatro-mayor',
    'tuboleta-jorge-eliecer-gaitan',
    'tuboleta-daviarena',
    'tuboleta-teatro-metropolitano',
    'meet2go-jsonld',
    'latiquetera-jsonld'
  )
  AND enabled;

COMMIT;
