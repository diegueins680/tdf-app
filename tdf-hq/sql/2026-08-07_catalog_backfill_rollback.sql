\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'catalog-cutover-2026-08-07'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif

BEGIN;
SET LOCAL statement_timeout = '15min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-catalog-backfill-v1', 0));

-- Rollback restores writer selection without requiring the removed legacy
-- table on clean installations. When the table exists this temporary source
-- retains the exact historical rows used to produce security audit evidence.
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

SELECT id AS backfill_run_id
FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset

-- Rollback changes writer selection only. Catalog entities, mappings, aliases,
-- translations, audit events, and provenance are intentionally retained.
-- The previous backend writes copied locale/currency codes and cannot satisfy
-- the canonical trigger. Remove only this cutover trigger before restoring its
-- writer selection; immutable mapping rows retain the exact source evidence.
DROP TRIGGER IF EXISTS user_locale_preference_regional_integrity ON user_locale_preferences;
UPDATE service_order target SET service_offering_id=NULL
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='service_catalog'
  AND target.catalog_id::text=mapping.source_record_id
  AND target.service_offering_id=mapping.entity_id;

UPDATE booking target SET service_offering_id=NULL
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='booking'
  AND target.id::text=mapping.source_record_id
  AND target.service_offering_id=mapping.entity_id;

DELETE FROM artist_genre_membership membership
USING catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='artist_genre'
  AND mapping.status='mapped'
  AND membership.genre_id=mapping.entity_id
  AND membership.artist_id::text=split_part(mapping.source_record_id, ':', 1);

DELETE FROM artist_profile_genre_membership membership
USING catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='artist_profile'
  AND mapping.source_column='genres'
  AND mapping.status='mapped'
  AND membership.genre_id=mapping.entity_id
  AND membership.artist_party_id::text=split_part(mapping.source_record_id, ':', 1);

DELETE FROM fan_profile_genre_membership membership
USING catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='fan_profile'
  AND mapping.source_column='favorite_genres'
  AND mapping.status='mapped'
  AND membership.genre_id=mapping.entity_id
  AND membership.fan_party_id::text=split_part(mapping.source_record_id, ':', 1);

UPDATE artist_genre target SET genre_id=NULL
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='artist_genre'
  AND target.artist_id::text || ':' || target.genre=mapping.source_record_id
  AND target.genre_id=mapping.entity_id;

UPDATE radio_stream target SET genre_id=NULL
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='genre'
  AND target.id::text=mapping.source_record_id
  AND target.genre_id=mapping.entity_id;

UPDATE radio_stream target SET country_id=NULL
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='radio_stream'
  AND mapping.source_column='country'
  AND target.id::text=mapping.source_record_id
  AND target.country_id=mapping.entity_id;

UPDATE user_locale_preferences target SET locale=mapping.original_value, locale_id=NULL
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='user_locale_preferences' AND mapping.source_column='locale'
  AND target.id::text=mapping.source_record_id AND target.locale_id=mapping.entity_id;

UPDATE user_locale_preferences target SET currency=mapping.original_value, currency_id=NULL
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='user_locale_preferences' AND mapping.source_column='currency'
  AND target.id::text=mapping.source_record_id AND target.currency_id=mapping.entity_id;

UPDATE user_locale_preferences target
SET country_code=mapping.original_value, country_id=NULL
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='user_locale_preferences' AND mapping.source_column='country_code'
  AND target.id::text=mapping.source_record_id AND target.country_id=mapping.entity_id;

UPDATE party target SET country_id=NULL FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid AND mapping.source_table='party'
  AND mapping.source_column='country_code' AND target.id::text=mapping.source_record_id
  AND target.country_id=mapping.entity_id;

UPDATE artist_profile target SET country_id=NULL FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid AND mapping.source_table='artist_profile'
  AND mapping.source_column='country_code' AND target.id::text=mapping.source_record_id
  AND target.country_id=mapping.entity_id;

UPDATE social_artist_profile target SET country_id=NULL FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid AND mapping.source_table='social_artist_profile'
  AND mapping.source_column='country_code' AND target.id::text=mapping.source_record_id
  AND target.country_id=mapping.entity_id;

UPDATE venue target SET country_id=NULL FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid AND mapping.source_table='venue'
  AND mapping.source_column='country_code' AND target.id::text=mapping.source_record_id
  AND target.country_id=mapping.entity_id;

UPDATE event_ticket_tier target SET currency_id=NULL FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid AND mapping.source_table='event_ticket_tier'
  AND mapping.source_column='currency' AND target.id::text=mapping.source_record_id
  AND target.currency_id=mapping.entity_id;

UPDATE pipeline_card target SET service_offering_id=NULL FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid AND mapping.source_table='pipeline_card'
  AND mapping.source_column='service_kind' AND target.id::text=mapping.source_record_id
  AND target.service_offering_id=mapping.entity_id;

UPDATE catalog_backfill_run
SET status='rolled-back', completed_at=now(),
    report=(COALESCE(NULLIF(report, ''), '{}')::jsonb || jsonb_build_object('rolledBackAt',now()))::text
WHERE id=:'backfill_run_id'::uuid;

-- Canonical security assignments are retained as historical bootstrap data.
-- The rollback restores the legacy writer revision; removing the canonical
-- mirror could otherwise delete the last emergency recovery path.
INSERT INTO security_audit_event (
  id, revision_id, entity_kind, party_id, role_id, permission_id, operation,
  previous_active, new_active, actor_id, reviewer_id, approver_id, occurred_at,
  source_platform, reason, correlation_id, approval_mode, result
)
SELECT gen_random_uuid(), NULL, 'party-role', source.party_id, mapping.entity_id,
  NULL, 'backfill-rollback-writer-selection', assignment.active,
  assignment.active, NULL, NULL, NULL, now(), 'production-migration',
  'canonical bootstrap assignment retained while legacy writer selection is restored',
  :'run_code' || ':rollback:party_role:' || mapping.source_record_id,
  assignment.approval_mode, 'retained'
FROM catalog_migration_mapping mapping
JOIN catalog_legacy_party_role_source source ON source.id::text=mapping.source_record_id
JOIN party_security_role assignment
  ON assignment.party_id=source.party_id AND assignment.role_id=mapping.entity_id
WHERE mapping.run_id=:'backfill_run_id'::uuid
  AND mapping.source_table='party_role' AND mapping.source_column='role'
  AND mapping.status='mapped' AND mapping.entity_id IS NOT NULL
  AND NOT EXISTS (
    SELECT 1 FROM security_audit_event audit
    WHERE audit.correlation_id=:'run_code' || ':rollback:party_role:' || mapping.source_record_id
  );

INSERT INTO catalog_audit_event (
  id, catalog_id, entity_id, operation, occurred_at, source_platform,
  reason, correlation_id, result, affected_relationships
)
SELECT gen_random_uuid(), mapping.catalog_id, mapping.entity_id, 'backfill-rollback',
  now(), 'production-migration', 'writer selection restored to pre-cutover columns',
  :'run_code' || ':rollback:' || mapping.source_table || ':' || mapping.source_column || ':' || mapping.source_record_id,
  'success', jsonb_build_object('sourceTable',mapping.source_table,'sourceRecordId',mapping.source_record_id)
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'backfill_run_id'::uuid AND mapping.entity_id IS NOT NULL
  AND mapping.source_table<>'party_role'
  AND NOT EXISTS (
    SELECT 1 FROM catalog_audit_event audit
    WHERE audit.correlation_id=:'run_code' || ':rollback:' || mapping.source_table || ':' || mapping.source_column || ':' || mapping.source_record_id
  );

SELECT jsonb_build_object('runId',id,'runCode',run_code,'status',status,'report',report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
