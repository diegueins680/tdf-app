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

function sqlLiteral(value) {
  return `'${String(value).replaceAll("'", "''")}'`;
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
  const cutoverValues = cutoverCodes.map((code) => `(${sqlLiteral(code)})`).join(',\n+    ');

  return `\\set ON_ERROR_STOP on
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
    };
  }));
}

async function main() {
  if (process.env.TDF_STUDIO_AUDIT_STAGING_BASELINE_CONFIRM !== expectedConfirmation) {
    throw new Error(
      `Set TDF_STUDIO_AUDIT_STAGING_BASELINE_CONFIRM=${expectedConfirmation} to render the baseline SQL.`,
    );
  }
  const entries = await loadStudioAuditStagingBaselineEntries();
  process.stdout.write(buildStudioAuditStagingBaselineSql(
    entries,
    process.env.SOURCE_COMMIT,
  ));
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  await main();
}
