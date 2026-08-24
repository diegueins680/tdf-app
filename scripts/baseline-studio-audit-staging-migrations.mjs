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

  return `\\set ON_ERROR_STOP on
BEGIN;

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
  if (targetIndex < 1 || targetIndex !== manifest.migrations.length - 1) {
    throw new Error('The studio-audit migration must exist and remain the final registered migration.');
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
