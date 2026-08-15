#!/usr/bin/env node

import fs from 'node:fs/promises';
import path from 'node:path';

const inputPath = process.argv[2];
const outputPath = process.argv[3] ?? inputPath;

if (!inputPath) {
  throw new Error('Usage: node scripts/order-catalog-foundation-seed.mjs INPUT.sql [OUTPUT.sql]');
}

const tableOrder = [
  'workflow_definition',
  'workflow_state',
  'security_module',
  'security_action',
  'security_permission',
  'security_role',
  'security_role_assignment_policy',
  'role_permission',
  'workflow_transition',
  'workflow_default_state',
  'workflow_state_capability',
  'catalog_definition',
  'country_reference',
  'currency_reference',
  'language_reference',
  'locale_reference',
  'deployment_currency_enablement',
  'deployment_locale_enablement',
  'external_provider',
  'ddex_standard_version',
  'ddex_standard_support',
  'ddex_message_type',
  'ddex_job_operation',
  'ddex_import_operation',
  'ddex_validation_result',
  'ddex_validation_severity',
  'ddex_validation_layer',
  'release_type_reference',
  'recording_type_reference',
  'recording_session_type',
  'service_category',
  'service_pricing_model',
  'service_resource_selection_mode',
  'service_offering',
  'pipeline_workflow_binding',
  'radio_auto_stop_option',
  'appearance_mode_option',
  'genre',
  'instrument',
  'event_type',
  'booking_type',
  'feedback_category',
  'feedback_severity',
  'reaction_type',
  'content_reaction_type',
  'creator_badge_type',
  'content_type',
  'authored_content',
  'catalog_slug_alias',
  'catalog_scoped_default',
];

const source = await fs.readFile(inputPath, 'utf8');
const grouped = new Map(tableOrder.map((table) => [table, []]));

for (const line of source.split(/\r?\n/u)) {
  const match = line.match(/^INSERT INTO public\.([a-z0-9_]+)\b/u);
  if (!match) continue;
  const rows = grouped.get(match[1]);
  if (!rows) throw new Error(`Unexpected seed table: ${match[1]}`);
  rows.push(line);
}

const missing = tableOrder.filter((table) => grouped.get(table).length === 0);
if (missing.length > 0) {
  throw new Error(`Seed dump is missing expected tables: ${missing.join(', ')}`);
}

const body = tableOrder.flatMap((table) => [
  `-- ${table}`,
  ...grouped.get(table),
  '',
]);

const output = [
  '\\set ON_ERROR_STOP on',
  '',
  'BEGIN;',
  "SET LOCAL statement_timeout = '15min';",
  "SET LOCAL lock_timeout = '2s';",
  "SELECT pg_advisory_xact_lock(hashtextextended('tdf-catalog-foundation-seed-v1', 0));",
  '',
  ...body,
  'DO $catalog_foundation_seed_gate$',
  'BEGIN',
  "  IF (SELECT count(*) FROM workflow_definition WHERE active) < 16",
  "     OR (SELECT count(*) FROM workflow_state WHERE active) < 97",
  "     OR (SELECT count(*) FROM workflow_transition WHERE active) < 295",
  "     OR (SELECT count(*) FROM catalog_definition WHERE active) < 47",
  "     OR (SELECT count(*) FROM country_reference WHERE active) < 249",
  "     OR (SELECT count(*) FROM security_module WHERE active) < 8",
  "     OR (SELECT count(*) FROM security_action WHERE active) < 16",
  "     OR (SELECT count(*) FROM security_permission WHERE active) < 30",
  "     OR (SELECT count(*) FROM security_role WHERE active) < 31",
  "     OR (SELECT count(*) FROM role_permission WHERE active) < 116 THEN",
  "    RAISE EXCEPTION 'canonical catalog foundation seed postcondition failed' USING ERRCODE='23514';",
  '  END IF;',
  'END',
  '$catalog_foundation_seed_gate$;',
  '',
  'COMMIT;',
  '',
].join('\n');

await fs.mkdir(path.dirname(outputPath), { recursive: true });
await fs.writeFile(outputPath, output, 'utf8');
