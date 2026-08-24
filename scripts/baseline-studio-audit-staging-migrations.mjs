#!/usr/bin/env node

import { createHash } from 'node:crypto';
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

import {
  expandMigrationIncludes,
  normalizeFullSha,
  validateMigrationRelativePath,
} from './lib/production-release.mjs';

const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const targetMigrationId = '2026-08-21_studio_internship_audit';
const expectedConfirmation = 'BASELINE_CURRENT_SYNTHETIC_SCHEMA_WITHOUT_PRODUCTION_DATA';
const expectedRuntimeConfirmation = 'REPLAY_CANONICAL_RUNTIME_MIGRATIONS_IN_SYNTHETIC_STAGING';
const runtimeMigrationStartId = '2026-08-13_unified_checkout_core';

function sqlLiteral(value) {
  return `'${String(value).replaceAll("'", "''")}'`;
}

export function buildStudioAuditStagingRuntimeSql(entries) {
  const startIndex = entries.findIndex(({ id }) => id === runtimeMigrationStartId);
  if (startIndex < 0) {
    throw new Error(`The staging runtime migration ${runtimeMigrationStartId} is missing.`);
  }
  const runtimeEntries = entries.slice(startIndex);
  for (const entry of runtimeEntries) {
    if (typeof entry.content !== 'string' || entry.content.trim() === '') {
      throw new Error(`Expanded SQL is missing for staging runtime migration ${entry.id}.`);
    }
  }

  const runtimeSql = runtimeEntries
    .map(({ id, content }) => {
      const beginCount = content.match(/^BEGIN;\s*$/gmu)?.length ?? 0;
      const commitCount = content.match(/^COMMIT;\s*$/gmu)?.length ?? 0;
      if (beginCount !== 1 || commitCount !== 1) {
        throw new Error(
          `Staging runtime migration ${id} must contain exactly one transaction wrapper.`,
        );
      }
      const body = content
        .replace(/^\\set ON_ERROR_STOP on\s*$/gmu, '')
        .replace(/^BEGIN;\s*$/mu, '')
        .replace(/^COMMIT;\s*$/mu, '')
        .trim();
      return `-- BEGIN CANONICAL STAGING RUNTIME MIGRATION ${id}\n${body}\n-- END CANONICAL STAGING RUNTIME MIGRATION ${id}`;
    })
    .join('\n\n');

  return `\\set ON_ERROR_STOP on
BEGIN;
DO $runtime_preflight$
BEGIN
  IF current_database() <> 'tdf_studio_audit_staging' THEN
    RAISE EXCEPTION 'Refusing runtime migration replay outside the isolated studio-audit staging database';
  END IF;
  IF EXISTS (
    SELECT 1 FROM public.party
    WHERE primary_email IS NOT NULL
      AND lower(primary_email) NOT LIKE '%@persona.test'
  ) THEN
    RAISE EXCEPTION 'Refusing runtime migration replay against non-synthetic party email addresses';
  END IF;
  IF (
    SELECT count(*) FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name IN (
        'intern_audit_plan', 'intern_test_case', 'intern_test_execution',
        'internal_feedback_report', 'internal_feedback_evidence', 'internal_feedback_history'
      )
  ) <> 6 THEN
    RAISE EXCEPTION 'The runtime migration target is not the expected current synthetic application schema';
  END IF;
END
$runtime_preflight$;

-- BEGIN CANONICAL STAGING RUNTIME MIGRATION REPLAY
${runtimeSql}
-- END CANONICAL STAGING RUNTIME MIGRATION REPLAY
COMMIT;
`;
}

export function buildStudioAuditStagingBaselineSql(entries, sourceCommit) {
  const normalizedSourceCommit = normalizeFullSha(sourceCommit);
  if (!Array.isArray(entries) || entries.length === 0) {
    throw new Error('At least one migration is required for the staging baseline.');
  }
  for (const entry of entries) {
    if (!/^[a-zA-Z0-9][a-zA-Z0-9_-]*$/u.test(entry.id)) {
      throw new Error(`Unsafe migration id: ${entry.id}`);
    }
    if (!/^[0-9a-f]{64}$/u.test(entry.checksum)) {
      throw new Error(`Invalid checksum for migration ${entry.id}.`);
    }
  }

  const values = entries
    .map(({ id, checksum }) => `(${sqlLiteral(id)}, ${sqlLiteral(checksum)}, ${sqlLiteral(normalizedSourceCommit)})`)
    .join(',\n  ');
  const ids = entries.map(({ id }) => sqlLiteral(id)).join(', ');
  const cutoverCodes = [
    'catalog-cutover-2026-08-07',
    'records-cms-cutover-2026-08-07',
    'instrument-input-cutover-2026-08-11',
    'feedback-catalog-cutover-2026-08-11',
    'pipeline-workflow-cutover-2026-08-11',
    'social-event-type-cutover-2026-08-11',
    'social-event-workflow-cutover-2026-08-11',
    'event-moment-reaction-cutover-2026-08-12',
    'content-reaction-cutover-2026-08-12',
    'creator-badge-cutover-2026-08-12',
    'ddex-reference-cutover-2026-08-12',
    'ddex-validation-reference-cutover-2026-08-12',
    'ddex-operational-cutover-2026-08-12',
  ];
  const cutoverValues = cutoverCodes.map((code) => `(${sqlLiteral(code)})`).join(',\n    ');

  const runtimeSql = buildStudioAuditStagingRuntimeSql(entries);

  return `${runtimeSql}
BEGIN;

CREATE TABLE IF NOT EXISTS public.tdf_schema_migration (
  migration_id TEXT PRIMARY KEY,
  checksum TEXT NOT NULL CHECK (checksum ~ '^[0-9a-f]{64}$'),
  source_commit TEXT NOT NULL CHECK (source_commit ~ '^[0-9a-f]{40}$'),
  applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

DO $preflight$
BEGIN
  IF current_database() <> 'tdf_studio_audit_staging' THEN
    RAISE EXCEPTION 'Refusing to baseline a database other than the isolated studio-audit staging database';
  END IF;
  IF EXISTS (
    SELECT 1 FROM public.party
    WHERE primary_email IS NOT NULL
      AND lower(primary_email) NOT LIKE '%@persona.test'
  ) THEN
    RAISE EXCEPTION 'Refusing to baseline a database containing non-synthetic party email addresses';
  END IF;
  IF EXISTS (
    SELECT 1 FROM public.tdf_schema_migration
    WHERE migration_id = ${sqlLiteral(targetMigrationId)}
  ) THEN
    RAISE EXCEPTION 'The studio-audit migration must remain pending during baseline creation';
  END IF;
  IF (
    SELECT count(*) FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_name IN (
        'intern_audit_plan', 'intern_test_case', 'intern_test_execution',
        'internal_feedback_report', 'internal_feedback_evidence', 'internal_feedback_history'
      )
  ) <> 6 THEN
    RAISE EXCEPTION 'The restored database is not the expected current synthetic application schema';
  END IF;
END
$preflight$;

-- Persistent's current-schema bootstrap uses a legacy acronym spelling for the
-- ticket QR uniqueness constraint and explicit RESTRICT actions for foreign
-- keys. Normalize the isolated synthetic schema to the reviewed production
-- contract before marking historical migrations as represented.
DO $ticket_qr_compatibility$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_catalog.pg_constraint
    WHERE conrelid = 'public.ticket_qr_code'::regclass
      AND conname = 'unique_ticket_qr_code' AND contype = 'u'
  ) AND EXISTS (
    SELECT 1 FROM pg_catalog.pg_constraint
    WHERE conrelid = 'public.ticket_qr_code'::regclass
      AND conname = 'unique_ticket_q_r_code' AND contype = 'u'
  ) THEN
    ALTER TABLE public.ticket_qr_code
      RENAME CONSTRAINT unique_ticket_q_r_code TO unique_ticket_qr_code;
  END IF;
END
$ticket_qr_compatibility$;

ALTER TABLE public.promo_code
  DROP CONSTRAINT IF EXISTS promo_code_event_id_fkey,
  ADD CONSTRAINT promo_code_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id);
ALTER TABLE public.promo_code_redemption
  DROP CONSTRAINT IF EXISTS promo_code_redemption_promo_code_id_fkey,
  DROP CONSTRAINT IF EXISTS promo_code_redemption_order_id_fkey,
  ADD CONSTRAINT promo_code_redemption_promo_code_id_fkey FOREIGN KEY (promo_code_id) REFERENCES public.promo_code(id),
  ADD CONSTRAINT promo_code_redemption_order_id_fkey FOREIGN KEY (order_id) REFERENCES public.event_ticket_order(id);
ALTER TABLE public.ticket_refund_request
  DROP CONSTRAINT IF EXISTS ticket_refund_request_order_id_fkey,
  ADD CONSTRAINT ticket_refund_request_order_id_fkey FOREIGN KEY (order_id) REFERENCES public.event_ticket_order(id);
ALTER TABLE public.ticket_transfer
  DROP CONSTRAINT IF EXISTS ticket_transfer_ticket_id_fkey,
  ADD CONSTRAINT ticket_transfer_ticket_id_fkey FOREIGN KEY (ticket_id) REFERENCES public.event_ticket(id);
ALTER TABLE public.event_waitlist
  DROP CONSTRAINT IF EXISTS event_waitlist_event_id_fkey,
  DROP CONSTRAINT IF EXISTS event_waitlist_tier_id_fkey,
  DROP CONSTRAINT IF EXISTS event_waitlist_converted_order_id_fkey,
  ADD CONSTRAINT event_waitlist_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id),
  ADD CONSTRAINT event_waitlist_tier_id_fkey FOREIGN KEY (tier_id) REFERENCES public.event_ticket_tier(id),
  ADD CONSTRAINT event_waitlist_converted_order_id_fkey FOREIGN KEY (converted_order_id) REFERENCES public.event_ticket_order(id);
ALTER TABLE public.stripe_payment_intent
  DROP CONSTRAINT IF EXISTS stripe_payment_intent_order_id_fkey,
  ADD CONSTRAINT stripe_payment_intent_order_id_fkey FOREIGN KEY (order_id) REFERENCES public.event_ticket_order(id);
ALTER TABLE public.ticket_qr_code
  DROP CONSTRAINT IF EXISTS ticket_qr_code_ticket_id_fkey,
  ADD CONSTRAINT ticket_qr_code_ticket_id_fkey FOREIGN KEY (ticket_id) REFERENCES public.event_ticket(id);
ALTER TABLE public.event_ticket_order
  DROP CONSTRAINT IF EXISTS event_ticket_order_promo_code_id_fkey,
  ADD CONSTRAINT event_ticket_order_promo_code_id_fkey FOREIGN KEY (promo_code_id) REFERENCES public.promo_code(id);
CREATE UNIQUE INDEX IF NOT EXISTS uq_event_ticket_order_stripe_payment_intent
  ON public.event_ticket_order(stripe_payment_intent_id)
  WHERE stripe_payment_intent_id IS NOT NULL;

-- SQL migrations intentionally use PostgreSQL text/integer types for several
-- runtime contracts, while Persistent's current-schema bootstrap emits
-- varchar/bigint for the corresponding Haskell Text/Int fields. Normalize the
-- synthetic bootstrap without changing application values.
DO $runtime_type_compatibility$
DECLARE
  item RECORD;
  actual_type TEXT;
BEGIN
  FOR item IN
    SELECT * FROM (VALUES
      ('external_venue_ref', 'provider', 'text'),
      ('external_venue_ref', 'external_id', 'text'),
      ('external_artist_ref', 'provider', 'text'),
      ('external_artist_ref', 'external_id', 'text'),
      ('external_event_ref', 'provider', 'text'),
      ('external_event_ref', 'external_id', 'text'),
      ('external_event_ref', 'city', 'text'),
      ('external_event_ref', 'country_code', 'text'),
      ('external_event_ref', 'source_url', 'text'),
      ('external_event_ref', 'price_cents', 'integer'),
      ('external_event_ref', 'currency', 'text'),
      ('external_event_ref', 'missing_runs', 'integer'),
      ('external_event_ref', 'source_status', 'text'),
      ('external_event_discovery_run', 'provider', 'text'),
      ('external_event_discovery_run', 'status', 'text'),
      ('external_event_discovery_run', 'cities_count', 'integer'),
      ('external_event_discovery_run', 'events_seen', 'integer'),
      ('external_event_discovery_run', 'events_created', 'integer'),
      ('external_event_discovery_run', 'events_updated', 'integer'),
      ('external_event_discovery_run', 'venues_created', 'integer'),
      ('external_event_discovery_run', 'artists_created', 'integer'),
      ('external_event_discovery_run', 'error_message', 'text'),
      ('event_city', 'name', 'text'),
      ('event_city', 'normalized_name', 'text'),
      ('event_city', 'country_code', 'text'),
      ('event_city', 'time_zone', 'text'),
      ('event_city_subscription', 'party_id', 'text'),
      ('event_discovery_source', 'source_key', 'text'),
      ('event_discovery_source', 'name', 'text'),
      ('event_discovery_source', 'source_type', 'text'),
      ('event_discovery_source', 'feed_url', 'text'),
      ('event_discovery_source', 'priority', 'integer'),
      ('event_discovery_source', 'configuration', 'text'),
      ('event_discovery_source', 'etag', 'text'),
      ('event_discovery_source', 'last_modified', 'text'),
      ('event_discovery_source', 'consecutive_failures', 'integer'),
      ('event_discovery_source', 'last_error', 'text'),
      ('social_discovery_review', 'status', 'text'),
      ('artist_research_source', 'supported_fields', 'text'),
      ('artist_research_source', 'content_hash', 'text'),
      ('artist_enrichment_suggestion', 'decision_note', 'text'),
      ('artist_identity_candidate', 'decision_note', 'text'),
      ('artist_media_asset', 'source_content_hash', 'text'),
      ('artist_media_asset', 'source_attribution', 'text'),
      ('artist_media_asset', 'source_width', 'integer'),
      ('artist_media_asset', 'source_height', 'integer'),
      ('artist_media_asset', 'source_mime_type', 'text'),
      ('artist_media_asset', 'drive_file_id', 'text'),
      ('feature_navigation_preferences', 'feature_id', 'text'),
      ('feature_navigation_preferences', 'pin_order', 'integer'),
      ('feature_navigation_preferences', 'use_count', 'integer'),
      ('ddex_document', 'file_name', 'text'),
      ('ddex_document', 'private_uri', 'text'),
      ('ddex_document', 'sha256', 'text'),
      ('ddex_document', 'family', 'text'),
      ('ddex_document', 'version', 'text'),
      ('ddex_document', 'namespace', 'text'),
      ('ddex_document', 'message_type', 'text'),
      ('ddex_document', 'status', 'text'),
      ('ddex_document', 'message_id', 'text'),
      ('ddex_document', 'sender_id', 'text'),
      ('ddex_document', 'recipient_id', 'text')
    ) expected(table_name, column_name, target_type)
  LOOP
    SELECT data_type INTO actual_type
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = item.table_name
      AND column_name = item.column_name;
    IF actual_type IS NULL THEN
      RAISE EXCEPTION 'Missing runtime bootstrap column %.%', item.table_name, item.column_name;
    END IF;
    IF actual_type <> item.target_type THEN
      EXECUTE format(
        'ALTER TABLE public.%I ALTER COLUMN %I TYPE %s USING %I::%s',
        item.table_name, item.column_name, item.target_type,
        item.column_name, item.target_type
      );
    END IF;
  END LOOP;
END
$runtime_type_compatibility$;

-- DDEX SQL migrations predate the Persistent models and use SERIAL/integer
-- identifiers. Convert the empty or synthetic bootstrap only after proving all
-- values fit; dropping the dependent FKs is required by PostgreSQL for the
-- referenced key type change.
DO $ddex_integer_range_guard$
BEGIN
  IF EXISTS (
    SELECT 1 FROM public.ddex_document
    WHERE id > 2147483647 OR uploaded_by > 2147483647
  ) OR EXISTS (
    SELECT 1 FROM public.ddex_message_header WHERE document_id > 2147483647
  ) OR EXISTS (
    SELECT 1 FROM public.ddex_validation_run WHERE document_id > 2147483647
  ) OR EXISTS (
    SELECT 1 FROM public.ddex_import_plan WHERE document_id > 2147483647
  ) THEN
    RAISE EXCEPTION 'DDEX synthetic bootstrap identifiers exceed the canonical integer range';
  END IF;
END
$ddex_integer_range_guard$;

ALTER TABLE public.ddex_message_header
  DROP CONSTRAINT IF EXISTS ddex_message_header_document_id_fkey;
ALTER TABLE public.ddex_validation_run
  DROP CONSTRAINT IF EXISTS ddex_validation_run_document_id_fkey;
ALTER TABLE public.ddex_import_plan
  DROP CONSTRAINT IF EXISTS ddex_import_plan_document_id_fkey;
ALTER TABLE public.ddex_document
  ALTER COLUMN id TYPE integer USING id::integer,
  ALTER COLUMN uploaded_by TYPE integer USING uploaded_by::integer;
ALTER TABLE public.ddex_message_header
  ALTER COLUMN document_id TYPE integer USING document_id::integer,
  ADD CONSTRAINT ddex_message_header_document_id_fkey
    FOREIGN KEY (document_id) REFERENCES public.ddex_document(id) ON DELETE CASCADE;
ALTER TABLE public.ddex_validation_run
  ALTER COLUMN document_id TYPE integer USING document_id::integer,
  ADD CONSTRAINT ddex_validation_run_document_id_fkey
    FOREIGN KEY (document_id) REFERENCES public.ddex_document(id) ON DELETE CASCADE;
ALTER TABLE public.ddex_import_plan
  ALTER COLUMN document_id TYPE integer USING document_id::integer,
  ADD CONSTRAINT ddex_import_plan_document_id_fkey
    FOREIGN KEY (document_id) REFERENCES public.ddex_document(id) ON DELETE CASCADE;

ALTER TABLE public.external_venue_ref
  DROP CONSTRAINT IF EXISTS external_venue_ref_venue_id_fkey,
  ADD CONSTRAINT external_venue_ref_venue_id_fkey FOREIGN KEY (venue_id) REFERENCES public.venue(id);
ALTER TABLE public.external_artist_ref
  DROP CONSTRAINT IF EXISTS external_artist_ref_artist_id_fkey,
  ADD CONSTRAINT external_artist_ref_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES public.social_artist_profile(id);
ALTER TABLE public.external_event_ref
  DROP CONSTRAINT IF EXISTS external_event_ref_event_id_fkey,
  ADD CONSTRAINT external_event_ref_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id);
CREATE INDEX IF NOT EXISTS idx_external_event_ref_city
  ON public.external_event_ref(lower(city));
CREATE INDEX IF NOT EXISTS idx_external_event_ref_event_id
  ON public.external_event_ref(event_id);
ALTER TABLE public.event_city_subscription
  DROP CONSTRAINT IF EXISTS event_city_subscription_city_id_fkey,
  ADD CONSTRAINT event_city_subscription_city_id_fkey FOREIGN KEY (city_id) REFERENCES public.event_city(id) ON DELETE CASCADE;
ALTER TABLE public.event_discovery_source
  DROP CONSTRAINT IF EXISTS event_discovery_source_city_id_fkey,
  ADD CONSTRAINT event_discovery_source_city_id_fkey FOREIGN KEY (city_id) REFERENCES public.event_city(id);
ALTER TABLE public.external_event_discovery_run
  DROP CONSTRAINT IF EXISTS unique_external_event_discovery_slot;
DROP INDEX IF EXISTS public.unique_external_event_discovery_slot;
CREATE UNIQUE INDEX unique_external_event_discovery_slot
  ON public.external_event_discovery_run(provider, scheduled_for)
  WHERE scheduled_for IS NOT NULL;
CREATE UNIQUE INDEX IF NOT EXISTS uq_artist_profile_slug_ci
  ON public.artist_profile(lower(slug))
  WHERE slug IS NOT NULL AND btrim(slug) <> '';
CREATE UNIQUE INDEX IF NOT EXISTS uq_artist_enrichment_active_full_run
  ON public.artist_enrichment_run((scope))
  WHERE status = 'running' AND scope = 'full';
CREATE INDEX IF NOT EXISTS idx_artist_suggestion_queue
  ON public.artist_enrichment_suggestion(status, confidence DESC, updated_at DESC);
CREATE INDEX IF NOT EXISTS idx_artist_field_change_history
  ON public.artist_field_change(artist_party_id, changed_at DESC);
CREATE INDEX IF NOT EXISTS idx_artist_media_asset_hash
  ON public.artist_media_asset(content_hash);
ALTER TABLE public.artist_inventory_reference
  DROP CONSTRAINT IF EXISTS fk_artist_inventory_social_artist,
  ADD CONSTRAINT fk_artist_inventory_social_artist
    FOREIGN KEY (social_artist_id) REFERENCES public.social_artist_profile(id) ON DELETE SET NULL;

ALTER TABLE public.social_event
  ALTER COLUMN end_time DROP NOT NULL;
DO $social_event_compatibility$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_catalog.pg_constraint
    WHERE conrelid = 'public.social_event'::regclass
      AND conname = 'social_event_time_order' AND contype = 'c'
  ) THEN
    ALTER TABLE public.social_event
      ADD CONSTRAINT social_event_time_order
      CHECK (end_time IS NULL OR start_time < end_time) NOT VALID;
  END IF;
END
$social_event_compatibility$;
ALTER TABLE public.social_event VALIDATE CONSTRAINT social_event_time_order;

DO $cutover_guard$
BEGIN
  IF EXISTS (SELECT 1 FROM public.catalog_backfill_run WHERE NOT dry_run) THEN
    RAISE EXCEPTION 'Refusing synthetic cutover markers when non-dry-run history exists';
  END IF;
END
$cutover_guard$;

INSERT INTO public.catalog_backfill_run (
  run_code, candidate_revision, dry_run, status, safety_threshold,
  scanned_rows, mapped_rows, ambiguous_rows, rejected_rows,
  started_at, completed_at, report, correlation_id
)
SELECT
  required.code, ${sqlLiteral(normalizedSourceCommit)}, FALSE, 'completed', 0,
  0, 0, 0, 0, NOW(), NOW(),
  '{"baseline":"current synthetic schema","productionData":false}',
  required.code || ':synthetic-staging-baseline'
FROM (VALUES
    ${cutoverValues}
) required(code);

DO $checksums$
BEGIN
  IF EXISTS (
    SELECT 1
    FROM public.tdf_schema_migration applied
    JOIN (VALUES
      ${entries.map(({ id, checksum }) => `(${sqlLiteral(id)}, ${sqlLiteral(checksum)})`).join(',\n      ')}
    ) expected(migration_id, checksum)
      ON expected.migration_id = applied.migration_id
    WHERE applied.checksum <> expected.checksum
  ) THEN
    RAISE EXCEPTION 'An existing staging migration checksum does not match the current manifest';
  END IF;
END
$checksums$;

INSERT INTO public.tdf_schema_migration (migration_id, checksum, source_commit)
VALUES
  ${values}
ON CONFLICT (migration_id) DO NOTHING;

DO $verification$
BEGIN
  IF (
    SELECT count(*) FROM public.tdf_schema_migration
    WHERE migration_id IN (${ids})
  ) <> ${entries.length} THEN
    RAISE EXCEPTION 'The synthetic current-schema migration baseline is incomplete';
  END IF;
  IF (
    SELECT count(*) FROM public.catalog_backfill_run
    WHERE NOT dry_run AND status = 'completed'
      AND run_code IN (${cutoverCodes.map(sqlLiteral).join(', ')})
  ) <> ${cutoverCodes.length} THEN
    RAISE EXCEPTION 'The synthetic canonical-cutover baseline is incomplete';
  END IF;
END
$verification$;

COMMIT;
`;
}

export async function loadStudioAuditStagingBaselineEntries() {
  const manifest = JSON.parse(await fs.readFile(
    path.join(rootDir, 'scripts', 'production-migrations.json'),
    'utf8',
  ));
  const targetIndex = manifest.migrations.findIndex(({ id }) => id === targetMigrationId);
  if (targetIndex < 1) {
    throw new Error('The studio-audit migration must exist after at least one baseline migration.');
  }

  return Promise.all(manifest.migrations.slice(0, targetIndex).map(async (entry) => {
    const relativePath = validateMigrationRelativePath(entry.path);
    const content = await expandMigrationIncludes(
      {
        path: relativePath,
        content: await fs.readFile(path.join(rootDir, relativePath), 'utf8'),
      },
      (includedPath) => fs.readFile(path.join(rootDir, includedPath), 'utf8'),
    );
    return {
      id: entry.id,
      checksum: createHash('sha256').update(content).digest('hex'),
      content,
    };
  }));
}

async function main() {
  const runtimeOnly = process.argv.includes('--runtime-only');
  const expected = runtimeOnly ? expectedRuntimeConfirmation : expectedConfirmation;
  if (process.env.TDF_STUDIO_AUDIT_STAGING_BASELINE_CONFIRM !== expected) {
    throw new Error(
      `Set TDF_STUDIO_AUDIT_STAGING_BASELINE_CONFIRM=${expected} to render the ${runtimeOnly ? 'runtime migration replay' : 'baseline'} SQL.`,
    );
  }
  const entries = await loadStudioAuditStagingBaselineEntries();
  process.stdout.write(runtimeOnly
    ? buildStudioAuditStagingRuntimeSql(entries)
    : buildStudioAuditStagingBaselineSql(entries, process.env.SOURCE_COMMIT));
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  await main();
}
