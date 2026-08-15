#!/usr/bin/env node

import fs from 'node:fs/promises';

const schemaPath = process.argv[2];
if (!schemaPath) {
  throw new Error('Usage: node scripts/wrap-catalog-canonical-schema.mjs SCHEMA.sql');
}

const source = await fs.readFile(schemaPath, 'utf8');
if (source.startsWith('\\set ON_ERROR_STOP on\n')) process.exit(0);

const tableNames = [...source.matchAll(/^CREATe TABLE "([a-z0-9_]+)"/gmu)]
  .map((match) => match[1]);
if (tableNames.length < 80 || new Set(tableNames).size !== tableNames.length) {
  throw new Error(`Expected at least 80 unique catalog tables, found ${tableNames.length}`);
}

const expectedValues = tableNames
  .map((table) => `      ('${table}'::text)`)
  .join(',\n');

const output = [
  '\\set ON_ERROR_STOP on',
  '',
  'BEGIN;',
  "SET LOCAL statement_timeout = '15min';",
  "SET LOCAL lock_timeout = '2s';",
  "SELECT pg_advisory_xact_lock(hashtextextended('tdf-catalog-canonical-schema-v1', 0));",
  "SELECT to_regclass('public.catalog_definition') IS NULL AS apply_catalog_canonical_schema \\gset",
  '\\if :apply_catalog_canonical_schema',
  '',
  source.trimEnd(),
  '',
  '\\else',
  'DO $catalog_canonical_schema_gate$',
  'BEGIN',
  '  IF EXISTS (',
  '    SELECT 1',
  '    FROM (VALUES',
  expectedValues,
  '    ) AS expected(table_name)',
  "    WHERE to_regclass('public.' || expected.table_name) IS NULL",
  '  ) THEN',
  "    RAISE EXCEPTION 'canonical catalog schema is partial; refusing idempotent skip' USING ERRCODE='55000';",
  '  END IF;',
  'END',
  '$catalog_canonical_schema_gate$;',
  '\\endif',
  '',
  'COMMIT;',
  '',
].join('\n');

await fs.writeFile(schemaPath, output, 'utf8');
