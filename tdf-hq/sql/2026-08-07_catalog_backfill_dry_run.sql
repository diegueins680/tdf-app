\set ON_ERROR_STOP on

-- New installations no longer create the legacy party_role table. Keep the
-- dry run valid in both states without recreating a legacy writer: an empty
-- session-local source means there is simply no role data to backfill.
DROP TABLE IF EXISTS pg_temp.catalog_legacy_party_role_source;
CREATE TEMP TABLE catalog_legacy_party_role_source (
  id bigint NOT NULL,
  party_id bigint NOT NULL,
  role text NOT NULL,
  active boolean NOT NULL
) ON COMMIT PRESERVE ROWS;

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

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '120s';
SET LOCAL lock_timeout = '2s';
SET LOCAL idle_in_transaction_session_timeout = '150s';

-- The explicit map is reviewed evidence, not fuzzy matching. Numeric legacy
-- IDs are deliberately excluded: seed order differs between installations.
-- Identity is established by an exact normalized label plus the service kind;
-- operational rate differences remain visible and are not identity keys.
WITH reviewed_service_map(source_name, expected_kind, entity_code, preference_rank, decision, evidence) AS (
  VALUES
    ('recording'::text, 'Recording'::text, 'recording'::text, 10, 'mapped'::text, 'exact reviewed English label plus matching service kind'),
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
    ('grabación audiovisual live', 'Recording', 'audiovisual-live-recording', 10, 'mapped', 'unique reviewed audiovisual label plus matching service kind')
), service_candidates AS (
  SELECT
    source.id,
    source.name,
    source.kind::text AS kind,
    source.pricing_model::text AS pricing_model,
    source.default_rate_cents,
    source.currency,
    source.tax_bps,
    CASE WHEN source.tax_bps IS NULL THEN NULL ELSE 'tax-' || source.tax_bps || 'bps' END AS tax_reference_code,
    CASE
      WHEN source.tax_bps IS NULL THEN 'not-applicable'
      WHEN tax_target.id IS NULL THEN 'create-from-exact-legacy-rate'
      WHEN tax_target.rate_bps=source.tax_bps AND tax_target.active THEN 'reuse-exact-active-reference'
      ELSE 'conflict'
    END AS tax_reference_decision,
    mapping.entity_code,
    mapping.decision,
    mapping.evidence,
    target.id AS target_id,
    target.code AS target_code
  FROM service_catalog source
  LEFT JOIN reviewed_service_map mapping
    ON mapping.source_name = lower(btrim(source.name))
   AND mapping.expected_kind = source.kind::text
  LEFT JOIN service_offering target ON target.code = mapping.entity_code
  LEFT JOIN tax_rate_reference tax_target
    ON tax_target.code='tax-' || source.tax_bps || 'bps'
)
SELECT jsonb_build_object(
  'report', 'service-catalog-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (WHERE decision='mapped' AND target_id IS NOT NULL),
  'withheld', count(*) FILTER (WHERE decision='withheld'),
  'missingReviewedDecision', count(*) FILTER (WHERE decision IS NULL),
  'missingTarget', count(*) FILTER (WHERE decision='mapped' AND target_id IS NULL),
  'taxReferencesToCreate', count(DISTINCT tax_reference_code) FILTER (
    WHERE tax_bps IS NOT NULL AND tax_reference_decision='create-from-exact-legacy-rate'
  ),
  'conflictingTaxReferences', count(DISTINCT tax_reference_code) FILTER (
    WHERE tax_reference_decision='conflict'
  ),
  'rows', jsonb_agg(to_jsonb(service_candidates) ORDER BY id)
) FROM service_candidates;

WITH booking_candidates AS (
  SELECT
    booking.id,
    booking.service_type,
    CASE btrim(booking.service_type)
      WHEN 'Grabación de Banda' THEN 'band-recording'
      WHEN 'Grabación de Voz' THEN 'voice-recording'
      WHEN 'Recording' THEN 'recording'
      WHEN 'Rehearsal (DJ)' THEN 'dj-booth-practice'
    END AS target_code
  FROM booking
  WHERE NULLIF(btrim(service_type), '') IS NOT NULL
)
SELECT jsonb_build_object(
  'report', 'booking-service-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (WHERE target.id IS NOT NULL),
  'unresolved', count(*) FILTER (WHERE target.id IS NULL),
  'rows', jsonb_agg(jsonb_build_object('id', candidate.id, 'value', candidate.service_type, 'targetCode', candidate.target_code, 'targetId', target.id) ORDER BY candidate.id)
)
FROM booking_candidates candidate
LEFT JOIN service_offering target ON target.code=candidate.target_code;

WITH genre_sources AS (
  SELECT 'artist_genre'::text AS source_table, artist_id::text AS source_id, genre AS original_value FROM artist_genre
  UNION ALL
  SELECT 'artist_profile', profile.artist_party_id::text || ':' || token.position::text, token.value
  FROM artist_profile profile
  CROSS JOIN LATERAL regexp_split_to_table(profile.genres, '\s*,\s*')
    WITH ORDINALITY AS token(value, position)
  WHERE profile.genres IS NOT NULL AND NULLIF(btrim(token.value), '') IS NOT NULL
  UNION ALL
  SELECT 'fan_profile', profile.fan_party_id::text || ':' || token.position::text, token.value
  FROM fan_profile profile
  CROSS JOIN LATERAL regexp_split_to_table(profile.favorite_genres, '\s*,\s*')
    WITH ORDINALITY AS token(value, position)
  WHERE profile.favorite_genres IS NOT NULL AND NULLIF(btrim(token.value), '') IS NOT NULL
  UNION ALL
  SELECT 'radio_stream', id::text, genre FROM radio_stream WHERE NULLIF(btrim(genre), '') IS NOT NULL
), genre_matches AS (
  SELECT source.*,
    candidates.candidate_count,
    candidates.candidate_ids,
    candidates.candidate_codes
  FROM genre_sources source
  CROSS JOIN catalog_definition catalog
  LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count,
      array_agg(candidate.id ORDER BY candidate.id) AS candidate_ids,
      array_agg(candidate.code ORDER BY candidate.id) AS candidate_codes
    FROM genre candidate
    JOIN workflow_state state
      ON state.id=candidate.workflow_state_id
     AND state.active
     AND state.code='published'
    WHERE candidate.active
      AND candidate.catalog_id=catalog.id
      AND lower(btrim(source.original_value)) IN (
        lower(candidate.code),
        lower(candidate.name_es),
        lower(candidate.name_en)
      )
  ) candidates ON TRUE
  WHERE catalog.code='genres' AND catalog.active
)
SELECT jsonb_build_object(
  'report', 'genre-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (WHERE candidate_count=1),
  'unresolved', count(*) FILTER (WHERE candidate_count=0),
  'ambiguous', count(*) FILTER (WHERE candidate_count>1),
  'radioObservationRows', count(*) FILTER (WHERE source_table='radio_stream'),
  'rows', jsonb_agg(to_jsonb(genre_matches) ORDER BY source_table, source_id)
) FROM genre_matches;

WITH country_sources AS (
  SELECT id::text AS source_id, country AS original_value
  FROM radio_stream
  WHERE NULLIF(btrim(country), '') IS NOT NULL
), country_matches AS (
  SELECT source.*,
    candidates.candidate_count,
    candidates.candidate_ids,
    candidates.candidate_codes
  FROM country_sources source
  LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count,
      array_agg(candidate.id ORDER BY candidate.id) AS candidate_ids,
      array_agg(candidate.alpha2 ORDER BY candidate.id) AS candidate_codes
    FROM country_reference candidate
    WHERE candidate.active
      AND candidate.deprecated_at IS NULL
      AND lower(btrim(source.original_value)) IN (
        lower(candidate.alpha2),
        lower(candidate.alpha3),
        lower(candidate.name_es),
        lower(candidate.name_en)
      )
  ) candidates ON TRUE
)
SELECT jsonb_build_object(
  'report', 'radio-country-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (WHERE candidate_count=1),
  'unresolved', count(*) FILTER (WHERE candidate_count=0),
  'ambiguous', count(*) FILTER (WHERE candidate_count>1),
  'rows', jsonb_agg(to_jsonb(country_matches) ORDER BY source_id)
) FROM country_matches;

WITH legacy_role_map(legacy_value, entity_code) AS (
  VALUES
    ('Admin','admin'), ('Manager','manager'), ('StudioManager','studio-manager'),
    ('Engineer','engineer'), ('Teacher','teacher'), ('Reception','reception'),
    ('Accounting','accounting'), ('LiveSessionsProducer','live-sessions-producer'),
    ('Intern','intern'), ('Artist','artist'), ('Artista','artista'),
    ('Webmaster','webmaster'), ('Promotor','promotor'), ('Promoter','promoter'),
    ('Producer','producer'), ('Agency','agency'), ('Songwriter','songwriter'),
    ('DJ','dj'), ('Publicist','publicist'), ('TourManager','tour-manager'),
    ('LabelRep','label-rep'), ('StageManager','stage-manager'),
    ('RoadCrew','road-crew'), ('Photographer','photographer'), ('AandR','a-and-r'),
    ('Student','student'), ('Vendor','vendor'), ('ReadOnly','read-only'),
    ('Customer','customer'), ('Fan','fan'), ('Maintenance','maintenance')
), role_candidates AS (
  SELECT source.id, source.party_id, source.role::text AS legacy_value,
    source.active, mapping.entity_code, target.id AS target_id
  FROM catalog_legacy_party_role_source source
  LEFT JOIN legacy_role_map mapping ON mapping.legacy_value=source.role::text
  LEFT JOIN security_role target ON target.code=mapping.entity_code AND target.active
)
SELECT jsonb_build_object(
  'report', 'party-security-role-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (WHERE target_id IS NOT NULL),
  'activeMapped', count(*) FILTER (WHERE active AND target_id IS NOT NULL),
  'unresolved', count(*) FILTER (WHERE target_id IS NULL),
  'rows', jsonb_agg(to_jsonb(role_candidates) ORDER BY id)
) FROM role_candidates;

SELECT jsonb_build_object(
  'report', 'locale-reference-map',
  'preferenceRows', count(*),
  'canonicalRows', count(*) FILTER (
    WHERE preference.locale_id IS NOT NULL AND preference.currency_id IS NOT NULL
  ),
  'rowsRequiringBackfill', count(*) FILTER (
    WHERE preference.locale_id IS NULL OR preference.currency_id IS NULL
  ),
  'missingLocale', count(*) FILTER (
    WHERE preference.locale_id IS NULL AND locale_ref.id IS NULL
  ),
  'missingCurrency', count(*) FILTER (
    WHERE preference.currency_id IS NULL AND currency_ref.id IS NULL
  ),
  'nonSelectableLocale', count(*) FILTER (WHERE NOT EXISTS (
    SELECT 1 FROM locale_reference item
    JOIN deployment_locale_enablement enabled
      ON enabled.locale_id=item.id AND enabled.deployment_code='default' AND enabled.enabled
    WHERE item.id=COALESCE(preference.locale_id, locale_ref.id)
      AND item.active AND item.deprecated_at IS NULL
  )),
  'nonSelectableCurrency', count(*) FILTER (WHERE NOT EXISTS (
    SELECT 1 FROM currency_reference item
    JOIN deployment_currency_enablement enabled
      ON enabled.currency_id=item.id AND enabled.deployment_code='default' AND enabled.enabled
    WHERE item.id=COALESCE(preference.currency_id, currency_ref.id)
      AND item.active AND item.deprecated_at IS NULL
  )),
  'missingCountry', count(*) FILTER (WHERE preference.country_code IS NOT NULL AND country_ref.id IS NULL)
)
FROM user_locale_preferences preference
LEFT JOIN locale_reference locale_ref ON locale_ref.code=preference.locale
  AND locale_ref.active AND locale_ref.deprecated_at IS NULL
LEFT JOIN currency_reference currency_ref ON currency_ref.code=preference.currency
  AND currency_ref.active AND currency_ref.deprecated_at IS NULL
LEFT JOIN country_reference country_ref ON country_ref.alpha2=preference.country_code AND country_ref.active;

SELECT jsonb_build_object(
  'report', 'safety-summary',
  'brokenCatalogForeignKeys', (
    SELECT count(*) FROM booking WHERE service_offering_id IS NOT NULL
      AND NOT EXISTS (SELECT 1 FROM service_offering WHERE id=booking.service_offering_id)
  ),
  'duplicateCatalogCodes', (
    SELECT count(*) FROM (SELECT code FROM catalog_definition GROUP BY code HAVING count(*)>1) duplicate
  ),
  'duplicateServiceCodes', (
    SELECT count(*) FROM (SELECT code FROM service_offering GROUP BY code HAVING count(*)>1) duplicate
  ),
  'conflictingLegacyTaxRates', (
    SELECT count(*)
    FROM (SELECT DISTINCT tax_bps FROM service_catalog WHERE tax_bps IS NOT NULL) source
    JOIN tax_rate_reference target ON target.code='tax-' || source.tax_bps || 'bps'
    WHERE target.rate_bps<>source.tax_bps OR NOT target.active
  ),
  'unknownLegacySecurityRoles', (
    SELECT count(*)
    FROM catalog_legacy_party_role_source source
    WHERE source.role::text NOT IN (
      'Admin','Manager','StudioManager','Engineer','Teacher','Reception','Accounting',
      'LiveSessionsProducer','Intern','Artist','Artista','Webmaster','Promotor',
      'Promoter','Producer','Agency','Songwriter','DJ','Publicist','TourManager',
      'LabelRep','StageManager','RoadCrew','Photographer','AandR','Student','Vendor',
      'ReadOnly','Customer','Fan','Maintenance'
    )
  )
);

ROLLBACK;
DROP TABLE catalog_legacy_party_role_source;
