\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'catalog-cutover-2026-08-07'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif
\if :{?safety_threshold}
\else
  \set safety_threshold 100
\endif

BEGIN;
SET LOCAL statement_timeout = '15min';
SET LOCAL lock_timeout = '2s';
SET LOCAL idle_in_transaction_session_timeout = '16min';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-catalog-backfill-v1', 0));

-- The coordinated cutover removes the legacy party_role writer and clean
-- installs may never have that table. Snapshot it only when present so the
-- same idempotent script handles upgrades and clean installations.
CREATE TEMP TABLE catalog_legacy_party_role_source (
  id bigint NOT NULL,
  party_id bigint NOT NULL,
  role text NOT NULL,
  active boolean NOT NULL
) ON COMMIT DROP;

DO $legacy_party_roles$
BEGIN
  IF to_regclass('public.party_role') IS NOT NULL THEN
    EXECUTE $copy$
      INSERT INTO catalog_legacy_party_role_source (id, party_id, role, active)
      SELECT id, party_id, role::text, active FROM public.party_role
    $copy$;
  END IF;
END
$legacy_party_roles$;

INSERT INTO catalog_backfill_run (
  id, run_code, candidate_revision, dry_run, status, safety_threshold,
  started_at, correlation_id
)
VALUES (
  gen_random_uuid(), :'run_code', :'candidate_revision', FALSE, 'mapping',
  :safety_threshold, now(), :'run_code' || ':' || :'candidate_revision'
)
ON CONFLICT (run_code, candidate_revision, dry_run)
DO UPDATE SET status='mapping', safety_threshold=EXCLUDED.safety_threshold,
  completed_at=NULL;

SELECT id AS backfill_run_id
FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset

CREATE TEMP TABLE reviewed_service_map (
  source_name text PRIMARY KEY,
  expected_kind text NOT NULL,
  entity_code text,
  preference_rank integer NOT NULL,
  decision text NOT NULL,
  evidence text NOT NULL
) ON COMMIT DROP;

INSERT INTO reviewed_service_map VALUES
  ('recording', 'Recording', 'recording', 10, 'mapped', 'exact reviewed English label plus matching service kind'),
  ('mixing', 'Mixing', 'mixing', 20, 'mapped', 'exact reviewed English label plus matching service kind'),
  ('mastering', 'Mastering', 'mastering', 10, 'mapped', 'exact reviewed label plus matching service kind'),
  ('rehearsal', 'Rehearsal', 'rehearsal', 20, 'mapped', 'exact reviewed English label plus matching service kind; source rate retained'),
  ('classes', 'Classes', 'classes', 20, 'mapped', 'exact reviewed English label plus matching service kind'),
  ('event production', 'EventProduction', 'event-production', 20, 'mapped', 'exact reviewed English label plus matching service kind'),
  ('grabación de banda', 'Recording', 'band-recording', 10, 'mapped', 'exact reviewed Spanish label plus matching service kind'),
  ('grabación de voz', 'Recording', 'voice-recording', 10, 'mapped', 'exact reviewed Spanish label plus matching service kind'),
  ('mezcla', 'Mixing', 'mixing', 10, 'mapped', 'reviewed bilingual equivalent plus matching service kind; source rate retained'),
  ('ensayo', 'Rehearsal', 'rehearsal', 10, 'mapped', 'reviewed bilingual equivalent plus matching service kind; source rate retained independently'),
  ('podcast', 'EventProduction', 'podcast-recording', 10, 'mapped', 'unique reviewed label plus matching legacy service kind'),
  ('clases', 'Classes', 'classes', 10, 'mapped', 'reviewed bilingual equivalent plus matching service kind'),
  ('producción de eventos', 'EventProduction', 'event-production', 10, 'mapped', 'reviewed bilingual equivalent plus matching service kind'),
  ('práctica en dj booth', 'Rehearsal', 'dj-booth-practice', 10, 'mapped', 'unique reviewed DJ-booth label plus matching service kind'),
  ('grabación audiovisual live', 'Recording', 'audiovisual-live-recording', 10, 'mapped', 'unique reviewed audiovisual label plus matching service kind');

INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT
  gen_random_uuid(), :'backfill_run_id'::uuid, 'service_catalog', 'name', source.id::text,
  source.name, lower(btrim(source.name)), catalog.id, target.id,
  CASE WHEN reviewed.decision='mapped' AND target.id IS NOT NULL THEN 'mapped'
       WHEN reviewed.decision='withheld' THEN 'withheld'
       ELSE 'unresolved' END,
  reviewed.evidence, 1, now()
FROM service_catalog source
LEFT JOIN reviewed_service_map reviewed
  ON reviewed.source_name=lower(btrim(source.name))
 AND reviewed.expected_kind=source.kind::text
JOIN catalog_definition catalog ON catalog.code='services'
LEFT JOIN service_offering target ON target.code=reviewed.entity_code
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET normalized_value=EXCLUDED.normalized_value, entity_id=EXCLUDED.entity_id,
  status=EXCLUDED.status, evidence=EXCLUDED.evidence, source_count=EXCLUDED.source_count;

WITH booking_map(original_value, entity_code, evidence) AS (
  VALUES
    ('Grabación de Banda'::text, 'band-recording'::text, 'exact label and unique service'),
    ('Grabación de Voz', 'voice-recording', 'exact label and unique service'),
    ('Recording', 'recording', 'exact label and unique service'),
    ('Rehearsal (DJ)', 'dj-booth-practice', 'explicit reviewed DJ qualifier mapping')
)
INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, 'booking', 'service_type', booking.id::text,
  booking.service_type, lower(btrim(booking.service_type)), catalog.id, target.id,
  CASE WHEN target.id IS NULL THEN 'unresolved' ELSE 'mapped' END,
  COALESCE(mapping.evidence, 'no reviewed deterministic mapping'), 1, now()
FROM booking
JOIN catalog_definition catalog ON catalog.code='services'
LEFT JOIN booking_map mapping ON mapping.original_value=btrim(booking.service_type)
LEFT JOIN service_offering target ON target.code=mapping.entity_code
WHERE NULLIF(btrim(booking.service_type), '') IS NOT NULL
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id, status=EXCLUDED.status,
  evidence=EXCLUDED.evidence, source_count=EXCLUDED.source_count;

WITH genre_sources AS (
  SELECT 'artist_genre'::text AS source_table, 'genre'::text AS source_column,
    artist_id::text || ':' || genre AS source_id, genre AS original_value
  FROM artist_genre
  UNION ALL
  SELECT 'artist_profile', 'genres', profile.artist_party_id::text || ':' || token.position::text,
    token.value
  FROM artist_profile profile
  CROSS JOIN LATERAL regexp_split_to_table(profile.genres, '\s*,\s*')
    WITH ORDINALITY AS token(value, position)
  WHERE profile.genres IS NOT NULL AND NULLIF(btrim(token.value), '') IS NOT NULL
  UNION ALL
  SELECT 'fan_profile', 'favorite_genres', profile.fan_party_id::text || ':' || token.position::text,
    token.value
  FROM fan_profile profile
  CROSS JOIN LATERAL regexp_split_to_table(profile.favorite_genres, '\s*,\s*')
    WITH ORDINALITY AS token(value, position)
  WHERE profile.favorite_genres IS NOT NULL AND NULLIF(btrim(token.value), '') IS NOT NULL
  UNION ALL
  SELECT 'radio_stream', 'genre', id::text, genre
  FROM radio_stream WHERE NULLIF(btrim(genre), '') IS NOT NULL
), candidates AS (
  SELECT source.*, catalog.id AS catalog_id,
    (SELECT count(*) FROM genre candidate
      JOIN workflow_state state ON state.id=candidate.workflow_state_id
      WHERE candidate.active AND candidate.catalog_id=catalog.id
      AND state.active AND state.code='published'
      AND lower(btrim(source.original_value)) IN (lower(candidate.code), lower(candidate.name_es), lower(candidate.name_en))) AS candidate_count,
    (SELECT candidate.id FROM genre candidate
      JOIN workflow_state state ON state.id=candidate.workflow_state_id
      WHERE candidate.active AND candidate.catalog_id=catalog.id
      AND state.active AND state.code='published'
      AND lower(btrim(source.original_value)) IN (lower(candidate.code), lower(candidate.name_es), lower(candidate.name_en))
      ORDER BY candidate.id LIMIT 1) AS entity_id
  FROM genre_sources source
  JOIN catalog_definition catalog ON catalog.code='genres'
)
INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, source_table, source_column,
  source_id, original_value, lower(btrim(original_value)), catalog_id,
  CASE WHEN candidate_count=1 THEN entity_id END,
  CASE WHEN candidate_count=1 THEN 'mapped' WHEN candidate_count=0 THEN 'unresolved' ELSE 'ambiguous' END,
  CASE WHEN candidate_count=1 THEN 'unique normalized code/name match'
       WHEN candidate_count=0 THEN 'no deterministic genre candidate'
       ELSE 'multiple normalized genre candidates' END,
  1, now()
FROM candidates
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id, status=EXCLUDED.status,
  evidence=EXCLUDED.evidence, source_count=EXCLUDED.source_count;

WITH country_sources AS (
  SELECT id::text AS source_id, country AS original_value
  FROM radio_stream
  WHERE NULLIF(btrim(country), '') IS NOT NULL
), candidates AS (
  SELECT source.*, catalog.id AS catalog_id,
    (SELECT count(*) FROM country_reference candidate
      WHERE candidate.active AND candidate.deprecated_at IS NULL
      AND lower(btrim(source.original_value)) IN (
        lower(candidate.alpha2), lower(candidate.alpha3),
        lower(candidate.name_es), lower(candidate.name_en))) AS candidate_count,
    (SELECT candidate.id FROM country_reference candidate
      WHERE candidate.active AND candidate.deprecated_at IS NULL
      AND lower(btrim(source.original_value)) IN (
        lower(candidate.alpha2), lower(candidate.alpha3),
        lower(candidate.name_es), lower(candidate.name_en))
      ORDER BY candidate.id LIMIT 1) AS entity_id
  FROM country_sources source
  JOIN catalog_definition catalog ON catalog.code='countries'
)
INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, 'radio_stream', 'country',
  source_id, original_value, lower(btrim(original_value)), catalog_id,
  CASE WHEN candidate_count=1 THEN entity_id END,
  CASE WHEN candidate_count=1 THEN 'mapped' WHEN candidate_count=0 THEN 'unresolved' ELSE 'ambiguous' END,
  CASE WHEN candidate_count=1 THEN 'unique normalized ISO code/name match'
       WHEN candidate_count=0 THEN 'no deterministic active country candidate'
       ELSE 'multiple normalized country candidates' END,
  1, now()
FROM candidates
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id, status=EXCLUDED.status,
  evidence=EXCLUDED.evidence, source_count=EXCLUDED.source_count;

WITH reference_sources(source_table, source_column, source_record_id, original_value, catalog_code, entity_id) AS (
  SELECT 'user_locale_preferences', 'locale', preference.id::text, preference.locale, 'locales',
    (SELECT id FROM locale_reference WHERE code=preference.locale AND active AND deprecated_at IS NULL)
  FROM user_locale_preferences preference
  WHERE preference.locale IS NOT NULL
  UNION ALL
  SELECT 'user_locale_preferences', 'currency', preference.id::text, preference.currency, 'currencies',
    (SELECT id FROM currency_reference WHERE code=preference.currency AND active AND deprecated_at IS NULL)
  FROM user_locale_preferences preference
  WHERE preference.currency IS NOT NULL
  UNION ALL
  SELECT 'user_locale_preferences', 'country_code', preference.id::text, preference.country_code, 'countries',
    (SELECT id FROM country_reference WHERE alpha2=preference.country_code AND active)
  FROM user_locale_preferences preference WHERE preference.country_code IS NOT NULL
  UNION ALL
  SELECT 'party', 'country_code', source.id::text, source.country_code, 'countries',
    (SELECT id FROM country_reference WHERE alpha2=source.country_code AND active)
  FROM party source WHERE source.country_code IS NOT NULL
  UNION ALL
  SELECT 'artist_profile', 'country_code', source.id::text, source.country_code, 'countries',
    (SELECT id FROM country_reference WHERE alpha2=source.country_code AND active)
  FROM artist_profile source WHERE source.country_code IS NOT NULL
  UNION ALL
  SELECT 'social_artist_profile', 'country_code', source.id::text, source.country_code, 'countries',
    (SELECT id FROM country_reference WHERE alpha2=source.country_code AND active)
  FROM social_artist_profile source WHERE source.country_code IS NOT NULL
  UNION ALL
  SELECT 'venue', 'country_code', source.id::text, source.country_code, 'countries',
    (SELECT id FROM country_reference WHERE alpha2=source.country_code AND active)
  FROM venue source WHERE source.country_code IS NOT NULL
  UNION ALL
  SELECT 'event_ticket_tier', 'currency', source.id::text, source.currency, 'currencies',
    (SELECT id FROM currency_reference WHERE code=source.currency AND active)
  FROM event_ticket_tier source
  UNION ALL
  SELECT 'pipeline_card', 'service_kind', source.id::text, source.service_kind::text, 'services',
    (SELECT id FROM service_offering WHERE code=CASE source.service_kind::text
      WHEN 'Recording' THEN 'recording' WHEN 'Mixing' THEN 'mixing'
      WHEN 'Mastering' THEN 'mastering' WHEN 'Rehearsal' THEN 'rehearsal'
      WHEN 'Classes' THEN 'classes' WHEN 'EventProduction' THEN 'event-production' END AND active)
  FROM pipeline_card source
)
INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, source.source_table,
  source.source_column, source.source_record_id, source.original_value,
  lower(btrim(source.original_value)), catalog.id, source.entity_id,
  CASE WHEN source.entity_id IS NULL THEN 'unresolved' ELSE 'mapped' END,
  CASE WHEN source.entity_id IS NULL THEN 'no unique active code match' ELSE 'unique stable code match' END,
  1, now()
FROM reference_sources source
JOIN catalog_definition catalog ON catalog.code=source.catalog_code
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id, status=EXCLUDED.status,
  evidence=EXCLUDED.evidence, source_count=EXCLUDED.source_count;

WITH legacy_role_map(legacy_value, entity_code) AS (
  VALUES
    ('Admin'::text,'admin'::text), ('Manager','manager'),
    ('StudioManager','studio-manager'), ('Engineer','engineer'),
    ('Teacher','teacher'), ('Reception','reception'), ('Accounting','accounting'),
    ('LiveSessionsProducer','live-sessions-producer'), ('Intern','intern'),
    ('Artist','artist'), ('Artista','artista'), ('Webmaster','webmaster'),
    ('Promotor','promotor'), ('Promoter','promoter'), ('Producer','producer'),
    ('Agency','agency'), ('Songwriter','songwriter'), ('DJ','dj'),
    ('Publicist','publicist'), ('TourManager','tour-manager'),
    ('LabelRep','label-rep'), ('StageManager','stage-manager'),
    ('RoadCrew','road-crew'), ('Photographer','photographer'), ('AandR','a-and-r'),
    ('Student','student'), ('Vendor','vendor'), ('ReadOnly','read-only'),
    ('Customer','customer'), ('Fan','fan'), ('Maintenance','maintenance')
)
INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, 'party_role', 'role', source.id::text,
  source.role::text, COALESCE(mapping.entity_code, lower(source.role::text)),
  catalog.id, target.id,
  CASE WHEN target.id IS NULL THEN 'unresolved' ELSE 'mapped' END,
  CASE WHEN mapping.entity_code IS NULL THEN 'legacy role has no reviewed stable-code mapping'
       WHEN target.id IS NULL THEN 'reviewed stable-code target is missing or inactive'
       ELSE 'exhaustive RoleEnum constructor mapped to its reviewed stable registry code' END,
  1, now()
FROM catalog_legacy_party_role_source source
CROSS JOIN catalog_definition catalog
LEFT JOIN legacy_role_map mapping ON mapping.legacy_value=source.role::text
LEFT JOIN security_role target ON target.code=mapping.entity_code AND target.active
WHERE catalog.code='security-roles'
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET normalized_value=EXCLUDED.normalized_value, entity_id=EXCLUDED.entity_id,
  status=EXCLUDED.status, evidence=EXCLUDED.evidence, source_count=EXCLUDED.source_count;

-- Deliberate division-by-zero aborts and rolls back the entire run if the
-- reviewed unresolved-row budget is exceeded.
SELECT 1 / CASE WHEN count(*) <= :safety_threshold THEN 1 ELSE 0 END AS safety_gate
FROM catalog_migration_mapping
WHERE run_id=:'backfill_run_id'::uuid AND status<>'mapped';

-- A partially migrated row with copied evidence that identifies a different
-- entity is not safe to normalize automatically. Preserve it and stop for
-- review instead of silently choosing either side of the conflict.
SELECT 1 / CASE WHEN count(*)=0 THEN 1 ELSE 0 END AS preference_identity_conflict_gate
FROM catalog_migration_mapping mapping
JOIN user_locale_preferences preference
  ON preference.id::text=mapping.source_record_id
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='user_locale_preferences'
  AND (
    (mapping.source_column='locale' AND preference.locale IS NOT NULL
      AND preference.locale_id IS NOT NULL
      AND preference.locale_id IS DISTINCT FROM mapping.entity_id)
    OR (mapping.source_column='currency' AND preference.currency IS NOT NULL
      AND preference.currency_id IS NOT NULL
      AND preference.currency_id IS DISTINCT FROM mapping.entity_id)
    OR (mapping.source_column='country_code' AND preference.country_code IS NOT NULL
      AND preference.country_id IS NOT NULL
      AND preference.country_id IS DISTINCT FROM mapping.entity_id)
  );

-- Select one stable source row per canonical service. Non-null operational
-- values win only where the reviewed mapping is not conflicted.
-- Clear only stale positional assignments left by an older candidate. This
-- permits a safe one-transaction swap without changing already-correct rows.
WITH intended AS (
  SELECT DISTINCT ON (mapping.entity_code)
    mapping.entity_code, source.id
  FROM reviewed_service_map mapping
  JOIN service_catalog source
    ON lower(btrim(source.name))=mapping.source_name
   AND source.kind::text=mapping.expected_kind
  WHERE mapping.decision='mapped'
  ORDER BY mapping.entity_code, mapping.preference_rank,
    source.default_rate_cents IS NULL, source.id
)
UPDATE service_offering target
SET legacy_service_catalog_id=NULL, updated_at=now(), version=target.version+1
WHERE target.legacy_service_catalog_id IS NOT NULL
  AND EXISTS (
    SELECT 1
    FROM reviewed_service_map mapping
    JOIN service_catalog source
      ON lower(btrim(source.name))=mapping.source_name
     AND source.kind::text=mapping.expected_kind
    WHERE mapping.decision='mapped'
      AND source.id=target.legacy_service_catalog_id
  )
  AND NOT EXISTS (
    SELECT 1 FROM intended
    WHERE intended.entity_code=target.code
      AND intended.id=target.legacy_service_catalog_id
  );

-- Preserve exact legacy tax rates as governed reference rows. The numeric
-- basis-point value is deterministic evidence; no statutory jurisdiction or
-- current legal applicability is inferred from the legacy service table.
INSERT INTO tax_rate_reference (
  id, code, name_es, name_en, description_es, description_en, rate_bps,
  country_id, standard, source_version, effective_from, effective_until,
  deprecated_at, replacement_id, last_synced_at, active, sort_order, version
)
SELECT gen_random_uuid(), 'tax-' || source.tax_bps || 'bps',
  'Tasa heredada de servicio (' || source.tax_bps || ' puntos base)',
  'Legacy service rate (' || source.tax_bps || ' basis points)',
  'Importada exactamente desde service_catalog; requiere revisión normativa antes de cambiar su aplicabilidad.',
  'Imported exactly from service_catalog; regulatory applicability must be reviewed before it changes.',
  source.tax_bps, NULL, 'legacy-service-catalog', :'candidate_revision',
  NULL, NULL, NULL, NULL, now(), TRUE, source.tax_bps, 1
FROM (SELECT DISTINCT tax_bps FROM service_catalog WHERE tax_bps IS NOT NULL) source
ON CONFLICT (code) DO NOTHING;

-- Abort instead of silently reusing a conflicting governed-reference code.
SELECT 1 / CASE WHEN count(*)=0 THEN 1 ELSE 0 END AS tax_reference_safety_gate
FROM (
  SELECT DISTINCT source.tax_bps
  FROM service_catalog source
  LEFT JOIN tax_rate_reference target
    ON target.code='tax-' || source.tax_bps || 'bps'
   AND target.rate_bps=source.tax_bps
   AND target.active
  WHERE source.tax_bps IS NOT NULL AND target.id IS NULL
) conflict;

WITH preferred_source AS (
  SELECT DISTINCT ON (mapping.entity_code)
    mapping.entity_code, source.id, source.default_rate_cents, source.tax_bps,
    source.currency, source.billing_unit,
    CASE source.pricing_model::text
      WHEN 'Hourly' THEN 'hourly' WHEN 'PerSong' THEN 'per-song'
      WHEN 'Package' THEN 'package' WHEN 'Quote' THEN 'quote'
      WHEN 'Retainer' THEN 'retainer' ELSE lower(source.pricing_model::text)
    END AS pricing_model_code
  FROM reviewed_service_map mapping
  JOIN service_catalog source
    ON lower(btrim(source.name))=mapping.source_name
   AND source.kind::text=mapping.expected_kind
  WHERE mapping.decision='mapped'
  ORDER BY mapping.entity_code, mapping.preference_rank,
    source.default_rate_cents IS NULL, source.id
), preferred AS (
  SELECT source.*, pricing.id AS pricing_model_id,
    currency.id AS currency_id, tax_rate.id AS tax_rate_id
  FROM preferred_source source
  JOIN service_pricing_model pricing
    ON pricing.code=source.pricing_model_code AND pricing.active
  JOIN currency_reference currency
    ON currency.code=source.currency AND currency.active
  LEFT JOIN tax_rate_reference tax_rate
    ON source.tax_bps IS NOT NULL
   AND tax_rate.code='tax-' || source.tax_bps || 'bps'
   AND tax_rate.rate_bps=source.tax_bps
   AND tax_rate.active
)
UPDATE service_offering target
SET legacy_service_catalog_id=preferred.id,
    pricing_model_id=preferred.pricing_model_id,
    pricing_model_code=NULL,
    default_rate_cents=preferred.default_rate_cents,
    tax_rate_id=preferred.tax_rate_id,
    tax_rate_code=NULL,
    currency_id=preferred.currency_id,
    billing_unit_es=COALESCE(preferred.billing_unit, target.billing_unit_es),
    billing_unit_en=target.billing_unit_en,
    updated_at=now(), version=target.version+1
FROM preferred
WHERE target.code=preferred.entity_code
  AND (target.legacy_service_catalog_id, target.pricing_model_id, target.pricing_model_code,
       target.default_rate_cents, target.tax_rate_id, target.tax_rate_code,
       target.currency_id, target.billing_unit_es)
      IS DISTINCT FROM
      (preferred.id, preferred.pricing_model_id, NULL, preferred.default_rate_cents,
       preferred.tax_rate_id, NULL, preferred.currency_id,
       COALESCE(preferred.billing_unit, target.billing_unit_es));

-- Older candidate revisions could leave copied relationship codes on a
-- canonical offering even when no legacy service_catalog row selected it.
-- Require an exact active target before clearing every remaining copy.
SELECT 1 / CASE WHEN count(*)=0 THEN 1 ELSE 0 END AS copied_service_relationship_safety_gate
FROM service_offering target
WHERE (target.pricing_model_code IS NOT NULL AND NOT EXISTS (
        SELECT 1 FROM service_pricing_model pricing
        WHERE pricing.code=target.pricing_model_code AND pricing.active
      ))
   OR (target.tax_rate_code IS NOT NULL AND NOT EXISTS (
        SELECT 1 FROM tax_rate_reference tax_rate
        WHERE tax_rate.code=target.tax_rate_code AND tax_rate.active
      ));

UPDATE service_offering target
SET pricing_model_id=COALESCE(
      target.pricing_model_id,
      (SELECT pricing.id FROM service_pricing_model pricing
       WHERE pricing.code=target.pricing_model_code AND pricing.active)
    ),
    pricing_model_code=NULL,
    tax_rate_id=COALESCE(
      target.tax_rate_id,
      (SELECT tax_rate.id FROM tax_rate_reference tax_rate
       WHERE tax_rate.code=target.tax_rate_code AND tax_rate.active)
    ),
    tax_rate_code=NULL,
    updated_at=now(),
    version=target.version+1
WHERE target.pricing_model_code IS NOT NULL OR target.tax_rate_code IS NOT NULL;

UPDATE service_order target
SET service_offering_id=mapping.entity_id
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='service_catalog' AND mapping.status='mapped'
  AND target.catalog_id::text=mapping.source_record_id
  AND target.service_offering_id IS DISTINCT FROM mapping.entity_id;

UPDATE booking target
SET service_offering_id=mapping.entity_id
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='booking' AND mapping.status='mapped'
  AND target.id::text=mapping.source_record_id
  AND target.service_offering_id IS DISTINCT FROM mapping.entity_id;

UPDATE artist_genre target
SET genre_id=mapping.entity_id
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='artist_genre' AND mapping.status='mapped'
  AND target.artist_id::text || ':' || target.genre=mapping.source_record_id
  AND target.genre_id IS DISTINCT FROM mapping.entity_id;

INSERT INTO artist_genre_membership (artist_id, genre_id, sort_order, created_at)
SELECT target.artist_id, mapping.entity_id,
  row_number() OVER (PARTITION BY target.artist_id ORDER BY target.genre)::integer - 1,
  now()
FROM artist_genre target
JOIN catalog_migration_mapping mapping
  ON mapping.run_id=:'backfill_run_id'::uuid
 AND mapping.source_table='artist_genre'
 AND mapping.status='mapped'
 AND target.artist_id::text || ':' || target.genre=mapping.source_record_id
WHERE mapping.entity_id IS NOT NULL
ON CONFLICT (artist_id, genre_id) DO NOTHING;

INSERT INTO artist_profile_genre_membership (artist_party_id, genre_id, sort_order, created_at)
SELECT split_part(mapping.source_record_id, ':', 1)::bigint,
  mapping.entity_id,
  split_part(mapping.source_record_id, ':', 2)::integer - 1,
  now()
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='artist_profile'
  AND mapping.source_column='genres'
  AND mapping.status='mapped'
  AND mapping.entity_id IS NOT NULL
ON CONFLICT (artist_party_id, genre_id) DO NOTHING;

INSERT INTO fan_profile_genre_membership (fan_party_id, genre_id, sort_order, created_at)
SELECT split_part(mapping.source_record_id, ':', 1)::bigint,
  mapping.entity_id,
  split_part(mapping.source_record_id, ':', 2)::integer - 1,
  now()
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='fan_profile'
  AND mapping.source_column='favorite_genres'
  AND mapping.status='mapped'
  AND mapping.entity_id IS NOT NULL
ON CONFLICT (fan_party_id, genre_id) DO NOTHING;

UPDATE radio_stream target
SET genre_id=mapping.entity_id
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='genre'
  AND mapping.status='mapped'
  AND target.id::text=mapping.source_record_id
  AND target.genre_id IS DISTINCT FROM mapping.entity_id;

-- Preserve provider/legacy genre text as reviewable evidence instead of
-- continuing to write it into radio_stream.genre. This aggregate is
-- deliberately idempotent: rerunning the same candidate revision does not
-- inflate its observation count.
INSERT INTO radio_stream_genre_observation (
  stream_id, original_value, normalized_value, genre_id, status, source,
  first_observed_at, last_observed_at, observation_count
)
SELECT target.id, mapping.original_value, mapping.normalized_value,
  mapping.entity_id, mapping.status,
  'catalog-backfill/' || :'candidate_revision', now(), now(), 1
FROM catalog_migration_mapping mapping
JOIN radio_stream target ON target.id::text=mapping.source_record_id
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='genre'
  AND mapping.status IN ('mapped', 'unresolved', 'ambiguous')
ON CONFLICT (stream_id, normalized_value, source)
DO UPDATE SET
  original_value=EXCLUDED.original_value,
  genre_id=EXCLUDED.genre_id,
  status=EXCLUDED.status,
  last_observed_at=EXCLUDED.last_observed_at;

-- Record every deterministic current candidate, including all candidates for
-- an ambiguous value, so review does not have to infer identity again.
UPDATE radio_stream_genre_observation_candidate existing
SET active=false, last_matched_at=now()
FROM radio_stream_genre_observation observation
JOIN catalog_migration_mapping mapping
  ON mapping.source_record_id=observation.stream_id::text
 AND mapping.normalized_value=observation.normalized_value
WHERE existing.observation_id=observation.id
  AND observation.source='catalog-backfill/' || :'candidate_revision'
  AND mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='genre'
  AND NOT EXISTS (
    SELECT 1
    FROM genre candidate
    JOIN workflow_state state
      ON state.id=candidate.workflow_state_id
     AND state.active
     AND state.code='published'
    WHERE candidate.id=existing.genre_id
      AND candidate.catalog_id=mapping.catalog_id
      AND candidate.active
      AND mapping.normalized_value IN (
        lower(btrim(candidate.code)),
        lower(btrim(candidate.name_es)),
        lower(btrim(candidate.name_en))
      )
  );

INSERT INTO radio_stream_genre_observation_candidate (
  observation_id, genre_id, active, first_matched_at, last_matched_at
)
SELECT observation.id, candidate.id, true, now(), now()
FROM catalog_migration_mapping mapping
JOIN radio_stream target ON target.id::text=mapping.source_record_id
JOIN radio_stream_genre_observation observation
  ON observation.stream_id=target.id
 AND observation.normalized_value=mapping.normalized_value
 AND observation.source='catalog-backfill/' || :'candidate_revision'
JOIN genre candidate
  ON candidate.catalog_id=mapping.catalog_id
 AND candidate.active
 AND mapping.normalized_value IN (
   lower(btrim(candidate.code)),
   lower(btrim(candidate.name_es)),
   lower(btrim(candidate.name_en))
 )
JOIN workflow_state state
  ON state.id=candidate.workflow_state_id
 AND state.active
 AND state.code='published'
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='genre'
ON CONFLICT (observation_id, genre_id)
DO UPDATE SET active=true, last_matched_at=EXCLUDED.last_matched_at;

UPDATE radio_stream target
SET country_id=mapping.entity_id
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='country'
  AND mapping.status='mapped'
  AND target.id::text=mapping.source_record_id
  AND target.country_id IS DISTINCT FROM mapping.entity_id;

-- Preserve each provider/legacy country string as review evidence. A rerun of
-- the same candidate revision refreshes the evidence without inflating count.
INSERT INTO radio_stream_country_observation (
  stream_id, original_value, normalized_value, country_id, status, source,
  first_observed_at, last_observed_at, observation_count
)
SELECT target.id, mapping.original_value, mapping.normalized_value,
  mapping.entity_id, mapping.status,
  'catalog-backfill/' || :'candidate_revision', now(), now(), 1
FROM catalog_migration_mapping mapping
JOIN radio_stream target ON target.id::text=mapping.source_record_id
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='country'
  AND mapping.status IN ('mapped', 'unresolved', 'ambiguous')
ON CONFLICT (stream_id, normalized_value, source)
DO UPDATE SET
  original_value=EXCLUDED.original_value,
  country_id=EXCLUDED.country_id,
  status=EXCLUDED.status,
  last_observed_at=EXCLUDED.last_observed_at;

UPDATE radio_stream_country_observation_candidate existing
SET active=false, last_matched_at=now()
FROM radio_stream_country_observation observation
JOIN catalog_migration_mapping mapping
  ON mapping.source_record_id=observation.stream_id::text
 AND mapping.normalized_value=observation.normalized_value
WHERE existing.observation_id=observation.id
  AND observation.source='catalog-backfill/' || :'candidate_revision'
  AND mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='country'
  AND NOT EXISTS (
    SELECT 1 FROM country_reference candidate
    WHERE candidate.id=existing.country_id
      AND candidate.active
      AND candidate.deprecated_at IS NULL
      AND mapping.normalized_value IN (
        lower(btrim(candidate.alpha2)), lower(btrim(candidate.alpha3)),
        lower(btrim(candidate.name_es)), lower(btrim(candidate.name_en))
      )
  );

INSERT INTO radio_stream_country_observation_candidate (
  observation_id, country_id, active, first_matched_at, last_matched_at
)
SELECT observation.id, candidate.id, true, now(), now()
FROM catalog_migration_mapping mapping
JOIN radio_stream target ON target.id::text=mapping.source_record_id
JOIN radio_stream_country_observation observation
  ON observation.stream_id=target.id
 AND observation.normalized_value=mapping.normalized_value
 AND observation.source='catalog-backfill/' || :'candidate_revision'
JOIN country_reference candidate
  ON candidate.active
 AND candidate.deprecated_at IS NULL
 AND mapping.normalized_value IN (
   lower(btrim(candidate.alpha2)), lower(btrim(candidate.alpha3)),
   lower(btrim(candidate.name_es)), lower(btrim(candidate.name_en))
 )
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='country'
ON CONFLICT (observation_id, country_id)
DO UPDATE SET active=true, last_matched_at=EXCLUDED.last_matched_at;

UPDATE user_locale_preferences preference
SET locale_id=COALESCE(
      preference.locale_id,
      (SELECT id FROM locale_reference WHERE code=preference.locale AND active AND deprecated_at IS NULL)
    ),
    currency_id=COALESCE(
      preference.currency_id,
      (SELECT id FROM currency_reference WHERE code=preference.currency AND active AND deprecated_at IS NULL)
    ),
    country_id=COALESCE(
      preference.country_id,
      (SELECT id FROM country_reference WHERE alpha2=preference.country_code AND active)
    ),
    locale=NULL,
    currency=NULL,
    country_code=NULL
WHERE (preference.locale_id IS NOT NULL OR EXISTS (
        SELECT 1 FROM locale_reference
        WHERE code=preference.locale AND active AND deprecated_at IS NULL
      ))
  AND (preference.currency_id IS NOT NULL OR EXISTS (
        SELECT 1 FROM currency_reference
        WHERE code=preference.currency AND active AND deprecated_at IS NULL
      ))
  AND (preference.locale IS NOT NULL OR preference.currency IS NOT NULL
       OR preference.country_code IS NOT NULL OR preference.locale_id IS NULL
       OR preference.currency_id IS NULL);

-- The coordinated preference contract cannot start while any existing row
-- still depends on copied locale/currency codes. A failure aborts the whole
-- transaction and retains the original evidence for review.
SELECT 1 / CASE WHEN count(*)=0 THEN 1 ELSE 0 END AS canonical_preference_gate
FROM user_locale_preferences preference
WHERE preference.locale_id IS NULL OR preference.currency_id IS NULL;

UPDATE party target SET country_id=country.id
FROM country_reference country
WHERE country.alpha2=target.country_code AND country.active
  AND target.country_id IS DISTINCT FROM country.id;

UPDATE artist_profile target SET country_id=country.id
FROM country_reference country
WHERE country.alpha2=target.country_code AND country.active
  AND target.country_id IS DISTINCT FROM country.id;

UPDATE social_artist_profile target SET country_id=country.id
FROM country_reference country
WHERE country.alpha2=target.country_code AND country.active
  AND target.country_id IS DISTINCT FROM country.id;

UPDATE venue target SET country_id=country.id
FROM country_reference country
WHERE country.alpha2=target.country_code AND country.active
  AND target.country_id IS DISTINCT FROM country.id;

UPDATE event_ticket_tier target SET currency_id=currency.id
FROM currency_reference currency
WHERE currency.code=target.currency AND currency.active
  AND target.currency_id IS DISTINCT FROM currency.id;

-- The resume migration restores legacy-writer semantics before this pending
-- backfill. Block concurrent writers while pairing the copied service label
-- with its canonical offering so the mapping snapshot stays coherent.
LOCK TABLE pipeline_card IN SHARE ROW EXCLUSIVE MODE;

UPDATE pipeline_card target SET service_offering_id=offering.id
FROM service_offering offering
WHERE offering.code=CASE target.service_kind::text
  WHEN 'Recording' THEN 'recording' WHEN 'Mixing' THEN 'mixing'
  WHEN 'Mastering' THEN 'mastering' WHEN 'Rehearsal' THEN 'rehearsal'
  WHEN 'Classes' THEN 'classes' WHEN 'EventProduction' THEN 'event-production' END
  AND target.service_offering_id IS DISTINCT FROM offering.id;

-- Bootstrap provenance is reserved for the reviewed legacy migration. Existing
-- canonical grants always win and are never overwritten by a rerun.
INSERT INTO party_security_role (
  id, party_id, role_id, granted_by, approved_by, approval_mode,
  emergency_reason, source_revision_id, active, created_at, revoked_at, version
)
SELECT gen_random_uuid(), source.party_id, mapping.entity_id, NULL, NULL,
  'bootstrap', NULL, NULL, source.active, now(),
  CASE WHEN source.active THEN NULL ELSE now() END, 1
FROM catalog_migration_mapping mapping
JOIN catalog_legacy_party_role_source source ON source.id::text=mapping.source_record_id
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='party_role' AND mapping.source_column='role'
  AND mapping.status='mapped' AND mapping.entity_id IS NOT NULL
ON CONFLICT (party_id, role_id) DO NOTHING;

INSERT INTO security_audit_event (
  id, revision_id, entity_kind, party_id, role_id, permission_id, operation,
  previous_active, new_active, actor_id, reviewer_id, approver_id, occurred_at,
  source_platform, reason, correlation_id, approval_mode, result
)
SELECT gen_random_uuid(), NULL, 'party-role', source.party_id, mapping.entity_id,
  NULL, 'bootstrap-mapped', NULL, source.active, NULL, NULL, NULL, now(),
  'production-migration', mapping.evidence,
  :'run_code' || ':party_role:' || mapping.source_record_id,
  'bootstrap', mapping.status
FROM catalog_migration_mapping mapping
JOIN catalog_legacy_party_role_source source ON source.id::text=mapping.source_record_id
JOIN party_security_role assignment
  ON assignment.party_id=source.party_id AND assignment.role_id=mapping.entity_id
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='party_role' AND mapping.source_column='role'
  AND mapping.status='mapped' AND mapping.entity_id IS NOT NULL
  AND NOT EXISTS (
    SELECT 1 FROM security_audit_event audit
    WHERE audit.correlation_id=:'run_code' || ':party_role:' || mapping.source_record_id
  );

INSERT INTO catalog_audit_event (
  id, catalog_id, entity_id, operation, occurred_at, source_platform,
  reason, correlation_id, result, affected_relationships
)
SELECT gen_random_uuid(), mapping.catalog_id, mapping.entity_id, 'backfilled', now(),
  'production-migration', mapping.evidence,
  :'run_code' || ':' || mapping.source_table || ':' || mapping.source_column || ':' || mapping.source_record_id,
  mapping.status,
  jsonb_build_object('sourceTable',mapping.source_table,'sourceColumn',mapping.source_column,
    'sourceRecordId',mapping.source_record_id,'originalValue',mapping.original_value)
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid AND mapping.entity_id IS NOT NULL
  AND mapping.source_table<>'party_role'
  AND NOT EXISTS (
    SELECT 1 FROM catalog_audit_event audit
    WHERE audit.correlation_id=:'run_code' || ':' || mapping.source_table || ':' || mapping.source_column || ':' || mapping.source_record_id
  );

UPDATE catalog_backfill_run
SET status='completed',
    scanned_rows=(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'backfill_run_id'::uuid),
    mapped_rows=(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'backfill_run_id'::uuid AND status='mapped'),
    ambiguous_rows=(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'backfill_run_id'::uuid AND status IN ('ambiguous','withheld')),
    rejected_rows=(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'backfill_run_id'::uuid AND status='unresolved'),
    completed_at=now(),
    report=jsonb_build_object(
      'mapped',(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'backfill_run_id'::uuid AND status='mapped'),
      'ambiguousOrWithheld',(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'backfill_run_id'::uuid AND status IN ('ambiguous','withheld')),
      'unresolved',(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'backfill_run_id'::uuid AND status='unresolved'),
      'securityAssignmentsMapped',(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'backfill_run_id'::uuid AND source_table='party_role' AND status='mapped'),
      'securityAssignmentsActive',(SELECT count(*) FROM party_security_role WHERE active)
    )::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object(
  'runId', id, 'runCode', run_code, 'revision', candidate_revision, 'status', status,
  'scanned', scanned_rows, 'mapped', mapped_rows, 'ambiguous', ambiguous_rows,
  'rejected', rejected_rows, 'report', report
)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
