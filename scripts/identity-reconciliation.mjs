#!/usr/bin/env node
import { readFileSync, writeFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import { parseArgs } from 'node:util';
import { candidateGroups, inventorySummary, sqlText, assertUuid } from './lib/identity-reconciliation.mjs';
import { buildDatabaseSqlInvocation } from './lib/production-release.mjs';

const { values } = parseArgs({ options: {
  command: { type: 'string', default: 'summary' }, inventory: { type: 'string' },
  'db-app': { type: 'string' }, database: { type: 'string' },
  case: { type: 'string' }, operation: { type: 'string' }, fingerprint: { type: 'string' },
  output: { type: 'string' },
} });
let sql;
if (values.command === 'summary') {
  console.log(JSON.stringify(inventorySummary(JSON.parse(readFileSync(values.inventory, 'utf8'))), null, 2));
  process.exit(0);
}
if (!values.output) throw new Error('--output is required; private results are never printed');
if (values.command === 'queue') {
  const inventory = JSON.parse(readFileSync(values.inventory, 'utf8'));
  const groups = candidateGroups(inventory);
  sql = groups.map(group => {
    const ids = `ARRAY[${group.member_ids.join(',')}]::bigint[]`;
    const evidence = { hints: group.hints, member_ids: group.member_ids, inventory_sha256: inventorySummary(inventory).inventory_sha256 };
    // Read current snapshots inside the transaction. The imported file supplies
    // hints only and cannot authorize execution.
    return `INSERT INTO identity_reconciliation_case(member_ids,evidence,before_parties,reason)
      SELECT ${ids},${sqlText(JSON.stringify(evidence))}::jsonb,
        (SELECT jsonb_agg(to_jsonb(p) ORDER BY p.id) FROM party p WHERE id=ANY(${ids})),${sqlText(group.reason)}
      WHERE NOT EXISTS(SELECT 1 FROM identity_reconciliation_case WHERE member_ids=${ids} AND status<>'reverted');`;
  }).join('\n') + '\nSELECT jsonb_build_object(\'awaiting_review\',count(*)) FROM identity_reconciliation_case WHERE status=\'review\';';
} else if (values.command === 'dry-run') {
  sql = `SELECT identity_merge_plan(${sqlText(assertUuid(values.case))}::uuid);`;
} else if (values.command === 'execute') {
  if (!/^[a-f0-9]{64}$/.test(values.fingerprint ?? '')) throw new Error('Exact dry-run fingerprint is required');
  sql = `SELECT identity_execute_merge(${sqlText(assertUuid(values.operation))}::uuid,${sqlText(assertUuid(values.case))}::uuid,${sqlText(values.fingerprint)});`;
} else if (values.command === 'rollback') {
  sql = `SELECT identity_rollback_merge(${sqlText(assertUuid(values.operation))}::uuid);`;
} else if (values.command === 'list') {
  sql = 'SELECT jsonb_agg(jsonb_build_object(\'id\',id,\'members\',member_ids,\'status\',status,\'reason\',reason)) FROM identity_reconciliation_case;';
} else throw new Error('Unknown command');
const readOnly = ['dry-run', 'list'].includes(values.command);
const invocation = buildDatabaseSqlInvocation({ dbApp: values['db-app'], database: values.database },
  `BEGIN${readOnly ? ' READ ONLY' : ''};\nSET LOCAL standard_conforming_strings=on;\nSET LOCAL lock_timeout='5s';\nSET LOCAL statement_timeout='60s';\n${sql}\nCOMMIT;`, { tuplesOnly: true });
writeFileSync(values.output, '', { mode: 0o600, flag: 'wx' });
const result = spawnSync(invocation.argv[0], invocation.argv.slice(1), { input: invocation.input, encoding: 'utf8', timeout: 90000, maxBuffer: 16 * 1024 * 1024 });
writeFileSync(values.output, result.stdout ?? '', { mode: 0o600 });
if (result.status !== 0) {
  // PostgreSQL errors can contain contact values. Keep diagnostics private too.
  writeFileSync(`${values.output}.error`, result.stderr ?? String(result.error), { mode: 0o600, flag: 'wx' });
  throw new Error('Database operation failed; inspect the private error artifact');
}
console.log(JSON.stringify({ command: values.command, result: values.output, verifiedExitCode: 0 }));
