-- PostgreSQL integration test for the real catalog backfill scripts.
--
-- Run only against a disposable database whose schema and catalog seeds have
-- already been created by the application. The production scripts commit by
-- design, so this test deliberately leaves immutable migration/audit evidence
-- behind; the disposable database must be discarded after the test.
--
--   psql "$TEST_DATABASE_URL" -v ON_ERROR_STOP=1 \
--     -f test/integration/catalog_backfill_postgres.sql

\set ON_ERROR_STOP on
\set run_code 'catalog-backfill-postgres-integration'
\set candidate_revision 'integration-fixture-v1'
\set safety_threshold 10000

-- Preserve the optional legacy source independently from the scripts' own
-- transaction-local adapters so assertions work on both upgrades and clean
-- installations.
CREATE TEMP TABLE catalog_backfill_integration_legacy_roles (
  id bigint NOT NULL,
  party_id bigint NOT NULL,
  role text NOT NULL,
  active boolean NOT NULL
) ON COMMIT PRESERVE ROWS;

DO $legacy_party_roles$
BEGIN
  IF to_regclass('public.party_role') IS NOT NULL THEN
    EXECUTE $copy$
      INSERT INTO catalog_backfill_integration_legacy_roles (id, party_id, role, active)
      SELECT id, party_id, role::text, active FROM public.party_role
    $copy$;
  END IF;
END
$legacy_party_roles$;

-- Exercise the radio country and genre cutover even on a clean installation.
-- Copied text is migration evidence; canonical writes use UUID relations.
INSERT INTO radio_stream (
  stream_url, name, country, country_id, genre, genre_id, is_active,
  last_checked_at, created_at, updated_at
)
VALUES (
  'https://radio.example.test/catalog-backfill-rock',
  'Catalog backfill fixture', 'EC', NULL, 'Rock', NULL, true,
  now(), now(), now()
)
ON CONFLICT (stream_url) DO UPDATE SET
  country='EC', country_id=NULL, genre='Rock', genre_id=NULL,
  is_active=true, updated_at=now();

-- Exercise the locale-preference country cutover from preserved ISO evidence.
-- Disable the candidate-only canonical trigger while constructing a fixture
-- that represents a row written before cutover, then restore enforcement.
ALTER TABLE user_locale_preferences DISABLE TRIGGER user_locale_preference_regional_integrity;
INSERT INTO user_locale_preferences (
  user_id, locale, currency, timezone, country_code, locale_id, currency_id,
  country_id, updated_at
)
SELECT id, 'es', 'USD', 'America/Guayaquil', 'EC', NULL, NULL, NULL, now()
FROM party
ORDER BY id
LIMIT 1
ON CONFLICT (user_id) DO UPDATE SET
  locale='es', currency='USD', timezone='America/Guayaquil',
  country_code='EC', locale_id=NULL, currency_id=NULL, country_id=NULL,
  updated_at=now();
ALTER TABLE user_locale_preferences ENABLE TRIGGER user_locale_preference_regional_integrity;

\ir ../../sql/2026-08-07_catalog_backfill_dry_run.sql
\ir ../../sql/2026-08-07_catalog_backfill_apply.sql

CREATE TEMP TABLE catalog_backfill_integration_snapshot AS
SELECT
  (SELECT count(*) FROM catalog_migration_mapping mapping
    JOIN catalog_backfill_run run ON run.id=mapping.run_id
    WHERE run.run_code=:'run_code'
      AND run.candidate_revision=:'candidate_revision'
      AND NOT run.dry_run) AS total_mappings,
  (SELECT count(*) FROM catalog_migration_mapping mapping
    JOIN catalog_backfill_run run ON run.id=mapping.run_id
    WHERE run.run_code=:'run_code'
      AND run.candidate_revision=:'candidate_revision'
      AND NOT run.dry_run
      AND mapping.source_table='party_role') AS role_mappings,
  (SELECT count(*) FROM security_audit_event
    WHERE operation='bootstrap-mapped'
      AND correlation_id LIKE :'run_code' || ':party_role:%') AS bootstrap_audits,
  (SELECT count(*) FROM party_security_role) AS canonical_assignments,
  (SELECT count(DISTINCT assignment.party_id)
    FROM party_security_role assignment
    JOIN security_role role ON role.id=assignment.role_id
    WHERE assignment.active AND role.active AND role.emergency_administrator)
    AS emergency_administrators,
  (SELECT md5(COALESCE(string_agg(
      concat_ws('|', code, legacy_service_catalog_id::text,
        default_rate_cents::text, pricing_model_id::text, tax_rate_id::text,
        currency_id::text, version::text,
        COALESCE((
          SELECT string_agg(
            concat_ws(':', relationship.resource_id::text,
              relationship.selection_mode_id::text,
              relationship.selection_mode, relationship.sort_order::text,
              relationship.active::text),
            ',' ORDER BY relationship.sort_order, relationship.resource_id
          )
          FROM service_offering_default_resource relationship
          WHERE relationship.service_offering_id=service_offering.id
        ), '')),
      ';' ORDER BY code), ''))
    FROM service_offering) AS service_offering_digest,
  (SELECT count(*) FROM service_offering_default_resource)
    AS service_default_resource_relationships,
  (SELECT count(*) FROM radio_stream_genre_observation
    WHERE source='catalog-backfill/' || :'candidate_revision')
    AS radio_genre_observations,
  (SELECT count(*)
    FROM radio_stream_genre_observation_candidate candidate
    JOIN radio_stream_genre_observation observation
      ON observation.id=candidate.observation_id
    WHERE observation.source='catalog-backfill/' || :'candidate_revision')
    AS radio_genre_observation_candidates,
  (SELECT count(*) FROM radio_stream_country_observation
    WHERE source='catalog-backfill/' || :'candidate_revision')
    AS radio_country_observations,
  (SELECT count(*)
    FROM radio_stream_country_observation_candidate candidate
    JOIN radio_stream_country_observation observation
      ON observation.id=candidate.observation_id
    WHERE observation.source='catalog-backfill/' || :'candidate_revision')
    AS radio_country_observation_candidates;

DO $integration$
DECLARE
  source_roles bigint;
  mapped_roles bigint;
  unresolved_roles bigint;
BEGIN
  SELECT count(*) INTO source_roles FROM catalog_backfill_integration_legacy_roles;
  SELECT count(*), count(*) FILTER (WHERE mapping.status<>'mapped')
    INTO mapped_roles, unresolved_roles
  FROM catalog_migration_mapping mapping
  JOIN catalog_backfill_run run ON run.id=mapping.run_id
  WHERE run.run_code='catalog-backfill-postgres-integration'
    AND run.candidate_revision='integration-fixture-v1'
    AND NOT run.dry_run
    AND mapping.source_table='party_role';

  IF mapped_roles <> source_roles OR unresolved_roles <> 0 THEN
    RAISE EXCEPTION
      'security role backfill mismatch: source=%, mapped=%, unresolved=%',
      source_roles, mapped_roles, unresolved_roles;
  END IF;

  IF EXISTS (
    SELECT 1
    FROM catalog_backfill_integration_legacy_roles legacy
    JOIN catalog_migration_mapping mapping
      ON mapping.source_table='party_role'
     AND mapping.source_record_id=legacy.id::text
     AND mapping.status='mapped'
    LEFT JOIN party_security_role assignment
      ON assignment.party_id=legacy.party_id
     AND assignment.role_id=mapping.entity_id
    WHERE assignment.id IS NULL
  ) THEN
    RAISE EXCEPTION 'a mapped legacy role has no canonical assignment';
  END IF;

  IF EXISTS (
    WITH expected(source_name, expected_kind, target_code) AS (
      VALUES
        ('recording'::text, 'Recording'::text, 'recording'::text),
        ('mixing', 'Mixing', 'mixing'),
        ('mastering', 'Mastering', 'mastering'),
        ('rehearsal', 'Rehearsal', 'rehearsal'),
        ('classes', 'Classes', 'classes'),
        ('event production', 'EventProduction', 'event-production'),
        ('grabación de banda', 'Recording', 'band-recording'),
        ('grabación de voz', 'Recording', 'voice-recording'),
        ('mezcla', 'Mixing', 'mixing'),
        ('ensayo', 'Rehearsal', 'rehearsal'),
        ('podcast', 'EventProduction', 'podcast-recording'),
        ('clases', 'Classes', 'classes'),
        ('producción de eventos', 'EventProduction', 'event-production'),
        ('práctica en dj booth', 'Rehearsal', 'dj-booth-practice'),
        ('grabación audiovisual live', 'Recording', 'audiovisual-live-recording')
    )
    SELECT 1
    FROM service_catalog source
    LEFT JOIN expected
      ON expected.source_name=lower(btrim(source.name))
     AND expected.expected_kind=source.kind::text
    LEFT JOIN catalog_migration_mapping mapping
      ON mapping.source_table='service_catalog'
     AND mapping.source_column='name'
     AND mapping.source_record_id=source.id::text
     AND mapping.run_id=(
       SELECT id FROM catalog_backfill_run
       WHERE run_code='catalog-backfill-postgres-integration'
         AND candidate_revision='integration-fixture-v1' AND NOT dry_run
     )
    LEFT JOIN service_offering target ON target.id=mapping.entity_id
    WHERE expected.target_code IS NULL
       OR mapping.status<>'mapped'
       OR target.code IS DISTINCT FROM expected.target_code
  ) THEN
    RAISE EXCEPTION 'a service_catalog row was mapped by position or to the wrong canonical service';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM service_offering
    WHERE pricing_model_id IS NULL
       OR currency_id IS NULL
       OR pricing_model_code IS NOT NULL
       OR tax_rate_code IS NOT NULL
  ) THEN
    RAISE EXCEPTION 'a service offering retained copied pricing/tax codes or is missing canonical pricing/currency relations';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM (SELECT DISTINCT tax_bps FROM service_catalog WHERE tax_bps IS NOT NULL) source
    LEFT JOIN tax_rate_reference target
      ON target.code='tax-' || source.tax_bps || 'bps'
     AND target.rate_bps=source.tax_bps
     AND target.active
    WHERE target.id IS NULL
       OR NULLIF(btrim(target.standard), '') IS NULL
       OR NULLIF(btrim(target.source_version), '') IS NULL
  ) THEN
    RAISE EXCEPTION 'an exact legacy tax value lacks an active governed reference with provenance';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM service_offering_default_resource relationship
    LEFT JOIN service_offering offering
      ON offering.id=relationship.service_offering_id
    LEFT JOIN resource default_resource
      ON default_resource.id=relationship.resource_id
    LEFT JOIN service_resource_selection_mode selection_mode
      ON selection_mode.id=relationship.selection_mode_id
    WHERE relationship.active
      AND (offering.id IS NULL OR NOT offering.active
        OR default_resource.id IS NULL OR NOT default_resource.active
        OR selection_mode.id IS NULL OR NOT selection_mode.active)
  ) THEN
    RAISE EXCEPTION 'an active default service resource relation points to an unavailable offering or resource';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM catalog_migration_mapping mapping
    JOIN catalog_backfill_run run ON run.id=mapping.run_id
    JOIN radio_stream stream ON stream.id::text=mapping.source_record_id
    LEFT JOIN radio_stream_genre_observation observation
      ON observation.stream_id=stream.id
     AND observation.normalized_value=mapping.normalized_value
     AND observation.source='catalog-backfill/integration-fixture-v1'
    WHERE run.run_code='catalog-backfill-postgres-integration'
      AND run.candidate_revision='integration-fixture-v1'
      AND NOT run.dry_run
      AND mapping.source_table='radio_stream'
      AND mapping.source_column='genre'
      AND (
        observation.id IS NULL
        OR observation.original_value IS DISTINCT FROM mapping.original_value
        OR observation.status IS DISTINCT FROM mapping.status
        OR observation.genre_id IS DISTINCT FROM mapping.entity_id
        OR observation.observation_count <> 1
        OR (mapping.status='mapped' AND stream.genre_id IS DISTINCT FROM mapping.entity_id)
      )
  ) THEN
    RAISE EXCEPTION 'a radio genre mapping lacks matching canonical state or preserved observation evidence';
  END IF;

  IF NOT EXISTS (
    SELECT 1
    FROM radio_stream stream
    JOIN genre target ON target.id=stream.genre_id AND target.code='rock'
    JOIN radio_stream_genre_observation observation
      ON observation.stream_id=stream.id
     AND observation.genre_id=target.id
     AND observation.status='mapped'
     AND observation.source='catalog-backfill/integration-fixture-v1'
    JOIN radio_stream_genre_observation_candidate candidate
      ON candidate.observation_id=observation.id
     AND candidate.genre_id=target.id
     AND candidate.active
    WHERE stream.stream_url='https://radio.example.test/catalog-backfill-rock'
      AND stream.genre='Rock'
  ) THEN
    RAISE EXCEPTION 'radio genre fixture was not cut over to the persisted rock genre with review evidence';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM catalog_migration_mapping mapping
    JOIN catalog_backfill_run run ON run.id=mapping.run_id
    JOIN radio_stream stream ON stream.id::text=mapping.source_record_id
    LEFT JOIN radio_stream_country_observation observation
      ON observation.stream_id=stream.id
     AND observation.normalized_value=mapping.normalized_value
     AND observation.source='catalog-backfill/integration-fixture-v1'
    WHERE run.run_code='catalog-backfill-postgres-integration'
      AND run.candidate_revision='integration-fixture-v1'
      AND NOT run.dry_run
      AND mapping.source_table='radio_stream'
      AND mapping.source_column='country'
      AND (
        observation.id IS NULL
        OR observation.original_value IS DISTINCT FROM mapping.original_value
        OR observation.status IS DISTINCT FROM mapping.status
        OR observation.country_id IS DISTINCT FROM mapping.entity_id
        OR observation.observation_count <> 1
        OR (mapping.status='mapped' AND stream.country_id IS DISTINCT FROM mapping.entity_id)
      )
  ) THEN
    RAISE EXCEPTION 'a radio country mapping lacks matching canonical state or preserved observation evidence';
  END IF;

  IF NOT EXISTS (
    SELECT 1
    FROM radio_stream stream
    JOIN country_reference target ON target.id=stream.country_id AND target.alpha2='EC'
    JOIN radio_stream_country_observation observation
      ON observation.stream_id=stream.id
     AND observation.country_id=target.id
     AND observation.status='mapped'
     AND observation.source='catalog-backfill/integration-fixture-v1'
    JOIN radio_stream_country_observation_candidate candidate
      ON candidate.observation_id=observation.id
     AND candidate.country_id=target.id
     AND candidate.active
    WHERE stream.stream_url='https://radio.example.test/catalog-backfill-rock'
      AND stream.country='EC'
  ) THEN
    RAISE EXCEPTION 'radio country fixture was not cut over to the persisted EC country with review evidence';
  END IF;
END
$integration$;

-- Database enforcement is authoritative even if an API client bypasses its
-- own validation. Every attempted mutation below is isolated in a PL/pgSQL
-- subtransaction so the expected exception cannot alter the fixture.
DO $radio_integrity$
DECLARE
  fixture_stream_id bigint;
  published_genre_id uuid;
  inactive_genre_id uuid;
  observation_id bigint;
  published_country_id uuid;
  inactive_country_id uuid;
  country_observation_id bigint;
  preference_party_id bigint;
  published_locale_id uuid;
  alternate_locale_id uuid;
  published_currency_id uuid;
  alternate_currency_id uuid;
BEGIN
  SELECT id INTO STRICT fixture_stream_id
  FROM radio_stream
  WHERE stream_url='https://radio.example.test/catalog-backfill-rock';

  SELECT id INTO STRICT published_genre_id
  FROM genre
  WHERE code='rock' AND active;

  SELECT id INTO STRICT inactive_genre_id
  FROM genre
  WHERE code='alternative' AND active;

  SELECT observation.id INTO STRICT observation_id
  FROM radio_stream_genre_observation observation
  WHERE observation.stream_id=fixture_stream_id
    AND observation.source='catalog-backfill/integration-fixture-v1';

  SELECT id INTO STRICT published_country_id
  FROM country_reference
  WHERE alpha2='EC' AND active AND deprecated_at IS NULL;

  SELECT id INTO STRICT inactive_country_id
  FROM country_reference
  WHERE alpha2='AF' AND active AND deprecated_at IS NULL;

  SELECT observation.id INTO STRICT country_observation_id
  FROM radio_stream_country_observation observation
  WHERE observation.stream_id=fixture_stream_id
    AND observation.source='catalog-backfill/integration-fixture-v1';

  SELECT id INTO STRICT preference_party_id FROM party ORDER BY id LIMIT 1;
  SELECT id INTO STRICT published_locale_id FROM locale_reference WHERE code='es' AND active;
  SELECT id INTO STRICT alternate_locale_id FROM locale_reference WHERE code='en' AND active;
  SELECT id INTO STRICT published_currency_id FROM currency_reference WHERE code='USD' AND active;
  SELECT id INTO STRICT alternate_currency_id FROM currency_reference WHERE code='EUR' AND active;
  IF NOT EXISTS (
    SELECT 1 FROM user_locale_preferences
    WHERE user_id=preference_party_id
      AND country_id=published_country_id
      AND locale_id=published_locale_id
      AND currency_id=published_currency_id
      AND locale IS NULL
      AND currency IS NULL
      AND country_code IS NULL
  ) THEN
    RAISE EXCEPTION 'preference evidence was not cut over to canonical UUIDs with copied codes cleared';
  END IF;

  -- A runtime preference write retains only canonical IDs plus the
  -- non-catalog timezone value.
  UPDATE user_locale_preferences
  SET locale=NULL, currency=NULL, country_code=NULL,
      locale_id=alternate_locale_id, currency_id=alternate_currency_id,
      country_id=published_country_id
  WHERE user_id=preference_party_id;

  BEGIN
    UPDATE user_locale_preferences
    SET locale_id='00000000-0000-0000-0000-000000000001'::uuid
    WHERE user_id=preference_party_id;
    RAISE EXCEPTION 'an unknown user preference locale UUID was accepted';
  EXCEPTION
    WHEN foreign_key_violation OR check_violation OR not_null_violation THEN NULL;
  END;

  BEGIN
    UPDATE user_locale_preferences
    SET locale='de', locale_id=published_locale_id
    WHERE user_id=preference_party_id;
    RAISE EXCEPTION 'conflicting copied locale evidence was accepted';
  EXCEPTION
    WHEN check_violation THEN NULL;
  END;

  UPDATE deployment_locale_enablement
  SET enabled=false
  WHERE deployment_code='default' AND locale_id=alternate_locale_id;
  BEGIN
    UPDATE user_locale_preferences
    SET locale_id=alternate_locale_id
    WHERE user_id=preference_party_id;
    RAISE EXCEPTION 'a deployment-disabled locale UUID was accepted';
  EXCEPTION
    WHEN check_violation THEN NULL;
  END;
  UPDATE deployment_locale_enablement
  SET enabled=true
  WHERE deployment_code='default' AND locale_id=alternate_locale_id;

  UPDATE deployment_currency_enablement
  SET enabled=false
  WHERE deployment_code='default' AND currency_id=alternate_currency_id;
  BEGIN
    UPDATE user_locale_preferences
    SET currency_id=alternate_currency_id
    WHERE user_id=preference_party_id;
    RAISE EXCEPTION 'a deployment-disabled currency UUID was accepted';
  EXCEPTION
    WHEN check_violation THEN NULL;
  END;
  UPDATE deployment_currency_enablement
  SET enabled=true
  WHERE deployment_code='default' AND currency_id=alternate_currency_id;

  -- The published canonical relation remains writable.
  UPDATE radio_stream
  SET genre_id=published_genre_id
  WHERE id=fixture_stream_id;

  BEGIN
    UPDATE radio_stream
    SET genre_id='00000000-0000-0000-0000-000000000001'::uuid
    WHERE id=fixture_stream_id;
    RAISE EXCEPTION 'an unknown radio genre UUID was accepted';
  EXCEPTION
    WHEN check_violation THEN NULL;
  END;

  UPDATE genre SET active=false WHERE id=inactive_genre_id;
  BEGIN
    UPDATE radio_stream
    SET genre_id=inactive_genre_id
    WHERE id=fixture_stream_id;
    RAISE EXCEPTION 'an inactive radio genre UUID was accepted';
  EXCEPTION
    WHEN check_violation THEN NULL;
  END;
  UPDATE genre SET active=true WHERE id=inactive_genre_id;

  -- The active canonical country relation remains writable.
  UPDATE radio_stream
  SET country_id=published_country_id
  WHERE id=fixture_stream_id;

  BEGIN
    UPDATE radio_stream
    SET country_id='00000000-0000-0000-0000-000000000001'::uuid
    WHERE id=fixture_stream_id;
    RAISE EXCEPTION 'an unknown radio country UUID was accepted';
  EXCEPTION
    WHEN foreign_key_violation OR check_violation THEN NULL;
  END;

  BEGIN
    UPDATE user_locale_preferences
    SET country_id='00000000-0000-0000-0000-000000000001'::uuid
    WHERE user_id=preference_party_id;
    RAISE EXCEPTION 'an unknown user preference country UUID was accepted';
  EXCEPTION
    WHEN foreign_key_violation OR check_violation THEN NULL;
  END;

  UPDATE country_reference SET active=false WHERE id=inactive_country_id;
  BEGIN
    UPDATE radio_stream
    SET country_id=inactive_country_id
    WHERE id=fixture_stream_id;
    RAISE EXCEPTION 'an inactive radio country UUID was accepted';
  EXCEPTION
    WHEN check_violation THEN NULL;
  END;
  BEGIN
    UPDATE user_locale_preferences
    SET country_id=inactive_country_id
    WHERE user_id=preference_party_id;
    RAISE EXCEPTION 'an inactive user preference country UUID was accepted';
  EXCEPTION
    WHEN check_violation THEN NULL;
  END;
  UPDATE country_reference SET active=true WHERE id=inactive_country_id;

  BEGIN
    DELETE FROM radio_stream_genre_observation WHERE id=observation_id;
    RAISE EXCEPTION 'immutable radio genre observation evidence was deleted';
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN NULL;
  END;

  IF NOT EXISTS (
    SELECT 1
    FROM radio_stream_genre_observation
    WHERE id=observation_id
  ) THEN
    RAISE EXCEPTION 'radio genre observation evidence disappeared after a rejected delete';
  END IF;

  BEGIN
    DELETE FROM radio_stream_country_observation WHERE id=country_observation_id;
    RAISE EXCEPTION 'immutable radio country observation evidence was deleted';
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN NULL;
  END;

  IF NOT EXISTS (
    SELECT 1 FROM radio_stream_country_observation
    WHERE id=country_observation_id
  ) THEN
    RAISE EXCEPTION 'radio country observation evidence disappeared after a rejected delete';
  END IF;
END
$radio_integrity$;

DO $radio_auto_stop_integrity$
DECLARE
  v_catalog_id uuid;
  v_default_option_id uuid;
  v_alternate_option_id uuid;
BEGIN
  SELECT id INTO STRICT v_catalog_id
  FROM catalog_definition
  WHERE code='radio-auto-stop-options' AND entity_kind='radio_auto_stop_option';

  IF (SELECT count(*) FROM radio_auto_stop_option WHERE catalog_id=v_catalog_id) <> 6 THEN
    RAISE EXCEPTION 'Radio auto-stop bootstrap must contain exactly six persisted options';
  END IF;

  SELECT scoped.entity_id INTO STRICT v_default_option_id
  FROM catalog_scoped_default scoped
  WHERE scoped.catalog_id=v_catalog_id
    AND scoped.scope_kind='radio-broadcast'
    AND scoped.scope_id='global'
    AND scoped.locale_id IS NULL
    AND scoped.active;

  IF NOT EXISTS (
    SELECT 1 FROM radio_auto_stop_option option
    WHERE option.id=v_default_option_id
      AND option.catalog_id=v_catalog_id
      AND option.duration_minutes=120
      AND option.active
      AND option.deprecated_at IS NULL
  ) THEN
    RAISE EXCEPTION 'Radio auto-stop default does not resolve to the active persisted 120-minute option';
  END IF;

  SELECT id INTO STRICT v_alternate_option_id
  FROM radio_auto_stop_option
  WHERE catalog_id=v_catalog_id AND duration_minutes=60;

  BEGIN
    INSERT INTO catalog_scoped_default (
      catalog_id, entity_id, scope_kind, scope_id, locale_id, active, created_by, version
    ) VALUES (
      v_catalog_id, v_alternate_option_id, 'radio-broadcast', 'global', NULL, true, NULL, 1
    );
    RAISE EXCEPTION 'a second active Radio broadcast default was accepted';
  EXCEPTION WHEN unique_violation THEN
    NULL;
  END;

  BEGIN
    UPDATE radio_auto_stop_option SET active=false WHERE id=v_default_option_id;
    RAISE EXCEPTION 'the active Radio broadcast default was deactivated';
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM = 'the active Radio broadcast default was deactivated' THEN
      RAISE;
    END IF;
  END;

  BEGIN
    UPDATE radio_auto_stop_option SET duration_minutes=1441 WHERE id=v_alternate_option_id;
    RAISE EXCEPTION 'an out-of-range Radio auto-stop duration was accepted';
  EXCEPTION WHEN check_violation THEN
    NULL;
  END;

  BEGIN
    DELETE FROM radio_auto_stop_option WHERE id=v_alternate_option_id;
    RAISE EXCEPTION 'a published Radio auto-stop option was hard-deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN
    NULL;
  END;
END
$radio_auto_stop_integrity$;

DO $appearance_mode_integrity$
DECLARE
  v_catalog_id uuid;
  v_default_option_id uuid;
  v_alternate_option_id uuid;
BEGIN
  SELECT id INTO STRICT v_catalog_id
  FROM catalog_definition
  WHERE code='appearance-modes' AND entity_kind='appearance_mode_option';

  IF (SELECT count(*) FROM appearance_mode_option WHERE catalog_id=v_catalog_id) <> 3 THEN
    RAISE EXCEPTION 'appearance bootstrap must contain exactly three persisted options';
  END IF;

  SELECT scoped.entity_id INTO STRICT v_default_option_id
  FROM catalog_scoped_default scoped
  WHERE scoped.catalog_id=v_catalog_id
    AND scoped.scope_kind='appearance-mode'
    AND scoped.scope_id='global'
    AND scoped.locale_id IS NULL
    AND scoped.active;

  IF NOT EXISTS (
    SELECT 1 FROM appearance_mode_option option
    WHERE option.id=v_default_option_id
      AND option.catalog_id=v_catalog_id
      AND option.code='system'
      AND option.active
      AND option.deprecated_at IS NULL
  ) THEN
    RAISE EXCEPTION 'appearance default does not resolve to the active persisted system option';
  END IF;

  SELECT id INTO STRICT v_alternate_option_id
  FROM appearance_mode_option
  WHERE catalog_id=v_catalog_id AND code='light';

  BEGIN
    INSERT INTO catalog_scoped_default (
      catalog_id, entity_id, scope_kind, scope_id, locale_id, active, created_by, version
    ) VALUES (
      v_catalog_id, v_alternate_option_id, 'appearance-mode', 'global', NULL, true, NULL, 1
    );
    RAISE EXCEPTION 'a second active appearance default was accepted';
  EXCEPTION WHEN unique_violation THEN
    NULL;
  END;

  BEGIN
    UPDATE appearance_mode_option SET active=false WHERE id=v_default_option_id;
    RAISE EXCEPTION 'the active appearance default was deactivated';
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM = 'the active appearance default was deactivated' THEN
      RAISE;
    END IF;
  END;

  BEGIN
    UPDATE appearance_mode_option SET code='sepia' WHERE id=v_alternate_option_id;
    RAISE EXCEPTION 'an unknown appearance renderer code was accepted';
  EXCEPTION WHEN check_violation THEN
    NULL;
  END;

  BEGIN
    DELETE FROM appearance_mode_option WHERE id=v_alternate_option_id;
    RAISE EXCEPTION 'a published appearance mode was hard-deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN
    NULL;
  END;
END
$appearance_mode_integrity$;

-- The exact same apply must update the run report without creating mappings,
-- assignments, or immutable audit events a second time.
\ir ../../sql/2026-08-07_catalog_backfill_apply.sql

DO $integration$
DECLARE
  before_snapshot catalog_backfill_integration_snapshot%ROWTYPE;
  current_total_mappings bigint;
  current_role_mappings bigint;
  current_bootstrap_audits bigint;
  current_assignments bigint;
  current_service_offering_digest text;
  current_service_default_resources bigint;
  current_radio_genre_observations bigint;
  current_radio_genre_observation_candidates bigint;
  current_radio_country_observations bigint;
  current_radio_country_observation_candidates bigint;
BEGIN
  SELECT * INTO before_snapshot FROM catalog_backfill_integration_snapshot;

  SELECT count(*) INTO current_total_mappings
  FROM catalog_migration_mapping mapping
  JOIN catalog_backfill_run run ON run.id=mapping.run_id
  WHERE run.run_code='catalog-backfill-postgres-integration'
    AND run.candidate_revision='integration-fixture-v1'
    AND NOT run.dry_run;

  SELECT count(*) INTO current_role_mappings
  FROM catalog_migration_mapping mapping
  JOIN catalog_backfill_run run ON run.id=mapping.run_id
  WHERE run.run_code='catalog-backfill-postgres-integration'
    AND run.candidate_revision='integration-fixture-v1'
    AND NOT run.dry_run
    AND mapping.source_table='party_role';

  SELECT count(*) INTO current_bootstrap_audits
  FROM security_audit_event
  WHERE operation='bootstrap-mapped'
    AND correlation_id LIKE 'catalog-backfill-postgres-integration:party_role:%';

  SELECT count(*) INTO current_assignments FROM party_security_role;

  SELECT md5(COALESCE(string_agg(
      concat_ws('|', code, legacy_service_catalog_id::text,
        default_rate_cents::text, pricing_model_id::text, tax_rate_id::text,
        currency_id::text, version::text,
        COALESCE((
          SELECT string_agg(
            concat_ws(':', relationship.resource_id::text,
              relationship.selection_mode_id::text,
              relationship.selection_mode, relationship.sort_order::text,
              relationship.active::text),
            ',' ORDER BY relationship.sort_order, relationship.resource_id
          )
          FROM service_offering_default_resource relationship
          WHERE relationship.service_offering_id=service_offering.id
        ), '')),
      ';' ORDER BY code), ''))
    INTO current_service_offering_digest
  FROM service_offering;

  SELECT count(*) INTO current_service_default_resources
  FROM service_offering_default_resource;

  SELECT count(*) INTO current_radio_genre_observations
  FROM radio_stream_genre_observation
  WHERE source='catalog-backfill/integration-fixture-v1';

  SELECT count(*) INTO current_radio_genre_observation_candidates
  FROM radio_stream_genre_observation_candidate candidate
  JOIN radio_stream_genre_observation observation
    ON observation.id=candidate.observation_id
  WHERE observation.source='catalog-backfill/integration-fixture-v1';

  SELECT count(*) INTO current_radio_country_observations
  FROM radio_stream_country_observation
  WHERE source='catalog-backfill/integration-fixture-v1';

  SELECT count(*) INTO current_radio_country_observation_candidates
  FROM radio_stream_country_observation_candidate candidate
  JOIN radio_stream_country_observation observation
    ON observation.id=candidate.observation_id
  WHERE observation.source='catalog-backfill/integration-fixture-v1';

  IF current_total_mappings <> before_snapshot.total_mappings
     OR current_role_mappings <> before_snapshot.role_mappings
     OR current_bootstrap_audits <> before_snapshot.bootstrap_audits
     OR current_assignments <> before_snapshot.canonical_assignments
     OR current_service_offering_digest <> before_snapshot.service_offering_digest
     OR current_service_default_resources <> before_snapshot.service_default_resource_relationships
     OR current_radio_genre_observations <> before_snapshot.radio_genre_observations
     OR current_radio_genre_observation_candidates <> before_snapshot.radio_genre_observation_candidates
     OR current_radio_country_observations <> before_snapshot.radio_country_observations
     OR current_radio_country_observation_candidates <> before_snapshot.radio_country_observation_candidates THEN
    RAISE EXCEPTION
      'apply is not idempotent: total mappings %=>%, role mappings %=>%, audits %=>%, assignments %=>%, services %=>%, default resources %=>%, radio genre observations %=>%, radio genre candidates %=>%, radio country observations %=>%, radio country candidates %=>%',
      before_snapshot.total_mappings, current_total_mappings,
      before_snapshot.role_mappings, current_role_mappings,
      before_snapshot.bootstrap_audits, current_bootstrap_audits,
      before_snapshot.canonical_assignments, current_assignments,
      before_snapshot.service_offering_digest, current_service_offering_digest,
      before_snapshot.service_default_resource_relationships, current_service_default_resources,
      before_snapshot.radio_genre_observations, current_radio_genre_observations,
      before_snapshot.radio_genre_observation_candidates, current_radio_genre_observation_candidates,
      before_snapshot.radio_country_observations, current_radio_country_observations,
      before_snapshot.radio_country_observation_candidates, current_radio_country_observation_candidates;
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM user_locale_preferences
    WHERE user_id=(SELECT id FROM party ORDER BY id LIMIT 1)
      AND locale_id=(SELECT id FROM locale_reference WHERE code='en')
      AND currency_id=(SELECT id FROM currency_reference WHERE code='EUR')
      AND locale IS NULL AND currency IS NULL
  ) THEN
    RAISE EXCEPTION 'rerunning the backfill overwrote a newer canonical preference selection';
  END IF;
END
$integration$;

-- Restore the exact first-cutover IDs so immediate rollback can prove that it
-- reverses only relationships introduced by this migration run.
UPDATE user_locale_preferences
SET locale_id=(SELECT id FROM locale_reference WHERE code='es'),
    currency_id=(SELECT id FROM currency_reference WHERE code='USD')
WHERE user_id=(SELECT id FROM party ORDER BY id LIMIT 1);

\ir ../../sql/2026-08-07_catalog_backfill_rollback.sql

ALTER TABLE catalog_backfill_integration_snapshot
  ADD COLUMN rollback_audits bigint;

UPDATE catalog_backfill_integration_snapshot
SET rollback_audits=(
  SELECT count(*) FROM security_audit_event
  WHERE operation='backfill-rollback-writer-selection'
    AND correlation_id LIKE :'run_code' || ':rollback:party_role:%'
);

-- Repeating rollback must retain canonical security rows and must not duplicate
-- its immutable audit evidence.
\ir ../../sql/2026-08-07_catalog_backfill_rollback.sql

DO $integration$
DECLARE
  before_snapshot catalog_backfill_integration_snapshot%ROWTYPE;
  current_rollback_audits bigint;
  current_assignments bigint;
  current_emergency_administrators bigint;
  run_status text;
BEGIN
  SELECT * INTO before_snapshot FROM catalog_backfill_integration_snapshot;
  SELECT count(*) INTO current_rollback_audits
  FROM security_audit_event
  WHERE operation='backfill-rollback-writer-selection'
    AND correlation_id LIKE 'catalog-backfill-postgres-integration:rollback:party_role:%';
  SELECT count(*) INTO current_assignments FROM party_security_role;
  SELECT count(DISTINCT assignment.party_id)
    INTO current_emergency_administrators
  FROM party_security_role assignment
  JOIN security_role role ON role.id=assignment.role_id
  WHERE assignment.active AND role.active AND role.emergency_administrator;
  SELECT status INTO run_status
  FROM catalog_backfill_run
  WHERE run_code='catalog-backfill-postgres-integration'
    AND candidate_revision='integration-fixture-v1'
    AND NOT dry_run;

  IF run_status <> 'rolled-back' THEN
    RAISE EXCEPTION 'expected rolled-back status, got %', run_status;
  END IF;
  IF current_rollback_audits <> before_snapshot.rollback_audits THEN
    RAISE EXCEPTION 'rollback audit events were duplicated: %=>%',
      before_snapshot.rollback_audits, current_rollback_audits;
  END IF;
  IF current_assignments <> before_snapshot.canonical_assignments THEN
    RAISE EXCEPTION 'rollback removed canonical assignments: %=>%',
      before_snapshot.canonical_assignments, current_assignments;
  END IF;
  IF current_emergency_administrators <> before_snapshot.emergency_administrators THEN
    RAISE EXCEPTION 'rollback changed emergency administrators: %=>%',
      before_snapshot.emergency_administrators, current_emergency_administrators;
  END IF;
  IF EXISTS (
    SELECT 1 FROM user_locale_preferences
    WHERE user_id=(SELECT id FROM party ORDER BY id LIMIT 1)
      AND (locale_id IS NOT NULL OR currency_id IS NOT NULL OR country_id IS NOT NULL
        OR locale IS DISTINCT FROM 'es' OR currency IS DISTINCT FROM 'USD'
        OR country_code IS DISTINCT FROM 'EC')
  ) THEN
    RAISE EXCEPTION 'rollback did not restore the copied regional writer evidence';
  END IF;
  IF EXISTS (
    SELECT 1 FROM radio_stream
    WHERE stream_url='https://radio.example.test/catalog-backfill-rock'
      AND genre_id IS NOT NULL
  ) THEN
    RAISE EXCEPTION 'rollback did not clear the canonical radio genre relationship introduced by the run';
  END IF;
  IF EXISTS (
    SELECT 1 FROM radio_stream
    WHERE stream_url='https://radio.example.test/catalog-backfill-rock'
      AND country_id IS NOT NULL
  ) THEN
    RAISE EXCEPTION 'rollback did not clear the canonical radio country relationship introduced by the run';
  END IF;
  IF (SELECT count(*) FROM radio_stream_genre_observation
      WHERE source='catalog-backfill/integration-fixture-v1')
      <> before_snapshot.radio_genre_observations THEN
    RAISE EXCEPTION 'rollback removed immutable radio genre observation evidence';
  END IF;
  IF (SELECT count(*) FROM radio_stream_country_observation
      WHERE source='catalog-backfill/integration-fixture-v1')
      <> before_snapshot.radio_country_observations THEN
    RAISE EXCEPTION 'rollback removed immutable radio country observation evidence';
  END IF;
END
$integration$;

SELECT 'catalog backfill PostgreSQL integration checks passed' AS result;
