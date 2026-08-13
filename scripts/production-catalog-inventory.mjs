#!/usr/bin/env node

import { createHash } from 'node:crypto';
import { spawnSync } from 'node:child_process';
import { writeFileSync } from 'node:fs';
import { resolve } from 'node:path';
import process from 'node:process';

const PRODUCTION_APP = 'tdf-hq';
const PRODUCTION_DATABASE_APP = 'tdf-hq-db';
const PRODUCTION_DATABASE = 'tdf_hq';

const inventorySql = String.raw`
\set ON_ERROR_STOP on
BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '120s';
SET LOCAL lock_timeout = '2s';
SET LOCAL idle_in_transaction_session_timeout = '150s';

SELECT json_build_object(
  'kind', 'metadata',
  'database', current_database(),
  'serverVersion', current_setting('server_version'),
  'transactionReadOnly', current_setting('transaction_read_only'),
  'tableCount', (
    SELECT count(*)::int
    FROM information_schema.tables
    WHERE table_schema = 'public' AND table_type = 'BASE TABLE'
  ),
  'capturedAt', now()
)::text;

WITH candidate_columns AS (
  SELECT
    c.table_schema,
    c.table_name,
    c.column_name,
    c.data_type
  FROM information_schema.columns AS c
  JOIN information_schema.tables AS t
    ON t.table_schema = c.table_schema
   AND t.table_name = c.table_name
   AND t.table_type = 'BASE TABLE'
  WHERE c.table_schema = 'public'
    AND c.data_type IN ('text', 'character varying', 'character')
    AND (
      c.column_name ~* '(^|_)(status|type|kind|role|category|genre|currency|locale|country_code|language|slug|platform|provider|tag|instrument|service_type|method|module|permission|workflow|state|source|medium|unit|policy|stage|action|scheme|territory|disposition|condition|quality|priority|scope|mode)s?$'
      OR (c.table_name, c.column_name) IN (
        ('service_catalog', 'name'),
        ('country', 'name'),
        ('event_city', 'city_name'),
        ('venue', 'city'),
        ('artist_profile', 'genres'),
        ('fan_profile', 'favorite_genres'),
        ('input_row', 'instrument')
      )
    )
    AND c.column_name !~* '(token|secret|password|credential|email|phone|address|notes?|reason|body|payload|metadata|url|uri|hash|external_id|reference)'
)
SELECT format($generated$
SELECT json_build_object(
  'kind', 'column',
  'table', %L,
  'column', %L,
  'dataType', %L,
  'rowCount', (SELECT count(*)::int FROM %I.%I),
  'nonEmptyCount', (
    SELECT count(*)::int
    FROM %I.%I
    WHERE NULLIF(btrim(%I::text), '') IS NOT NULL
  ),
  'distinctCount', (
    SELECT count(DISTINCT %I::text)::int
    FROM %I.%I
    WHERE NULLIF(btrim(%I::text), '') IS NOT NULL
  ),
  'truncated', (
    SELECT count(DISTINCT %I::text) > 200
    FROM %I.%I
    WHERE NULLIF(btrim(%I::text), '') IS NOT NULL
  ),
  'values', COALESCE((
    SELECT json_agg(
      json_build_object('value', values_by_count.value, 'count', values_by_count.row_count)
      ORDER BY values_by_count.row_count DESC, values_by_count.value
    )
    FROM (
      SELECT %I::text AS value, count(*)::int AS row_count
      FROM %I.%I
      WHERE NULLIF(btrim(%I::text), '') IS NOT NULL
        AND length(%I::text) <= 256
      GROUP BY %I::text
      ORDER BY row_count DESC, value
      LIMIT 200
    ) AS values_by_count
  ), '[]'::json)
)::text;
$generated$,
  table_name,
  column_name,
  data_type,
  table_schema, table_name,
  table_schema, table_name, column_name,
  column_name, table_schema, table_name, column_name,
  column_name, table_schema, table_name, column_name,
  column_name, table_schema, table_name, column_name, column_name, column_name
)
FROM candidate_columns
ORDER BY table_name, column_name
\gexec

SELECT json_build_object(
  'kind', 'role-assignment-summary',
  'rows', COALESCE(json_agg(row_to_json(role_counts) ORDER BY role_counts.role), '[]'::json)
)::text
FROM (
  SELECT role::text AS role, active, count(*)::int AS assignment_count
  FROM party_role
  GROUP BY role, active
) AS role_counts;

SELECT json_build_object(
  'kind', 'table-estimates',
  'rows', COALESCE(json_agg(row_to_json(table_counts) ORDER BY table_counts.table_name), '[]'::json)
)::text
FROM (
  SELECT relname AS table_name, n_live_tup::bigint AS estimated_rows
  FROM pg_stat_user_tables
  ORDER BY relname
) AS table_counts;

ROLLBACK;
`;

function parseArgs(argv) {
  const options = { output: null, dryRun: false };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === '--output') options.output = argv[++index];
    else if (arg === '--dry-run') options.dryRun = true;
    else if (arg === '--help') {
      process.stdout.write(
        'Usage: node scripts/production-catalog-inventory.mjs [--output PATH] [--dry-run]\n',
      );
      process.exit(0);
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }
  return options;
}

function run(command, args) {
  const result = spawnSync(command, args, {
    cwd: process.cwd(),
    encoding: 'utf8',
    maxBuffer: 32 * 1024 * 1024,
  });
  if (result.error) throw result.error;
  if (result.status !== 0) {
    throw new Error(
      `${command} exited with ${result.status}: ${(result.stderr || result.stdout).trim()}`,
    );
  }
  return result.stdout;
}

function remotePsqlCommand(sql) {
  const encoded = Buffer.from(sql).toString('base64');
  return [
    'sh -lc',
    `'printf %s ${encoded} | base64 -d | su postgres -c "psql -X -v ON_ERROR_STOP=1 -qAt -p 5433 -d ${PRODUCTION_DATABASE}"'`,
  ].join(' ');
}

async function fetchJson(url) {
  const response = await fetch(url, {
    headers: { accept: 'application/json' },
    signal: AbortSignal.timeout(15_000),
  });
  if (!response.ok) throw new Error(`${url} returned HTTP ${response.status}`);
  return response.json();
}

async function main() {
  const options = parseArgs(process.argv.slice(2));
  if (options.dryRun) {
    process.stdout.write(`${inventorySql}\n`);
    return;
  }

  const [health, version] = await Promise.all([
    fetchJson(`https://${PRODUCTION_APP}.fly.dev/health`),
    fetchJson(`https://${PRODUCTION_APP}.fly.dev/version`),
  ]);
  const machines = JSON.parse(
    run('flyctl', ['machine', 'list', '--app', PRODUCTION_APP, '--json']),
  ).map((machine) => ({
    id: machine.id,
    region: machine.region,
    state: machine.state,
    version: machine.config?.metadata?.fly_release_version ?? null,
    imageDigest: machine.image_ref?.digest ?? null,
  }));

  const stdout = run('flyctl', [
    'ssh',
    'console',
    '--app',
    PRODUCTION_DATABASE_APP,
    '--command',
    remotePsqlCommand(inventorySql),
  ]);
  const records = stdout
    .split('\n')
    .map((line) => line.trim())
    .filter((line) => line.startsWith('{'))
    .map((line) => JSON.parse(line));
  if (!records.some(({ kind }) => kind === 'metadata')) {
    throw new Error('Production inventory returned no metadata record.');
  }
  const metadata = records.find(({ kind }) => kind === 'metadata');
  if (metadata.transactionReadOnly !== 'on') {
    throw new Error('Production inventory transaction was not read-only.');
  }

  const report = {
    schemaVersion: 1,
    capturedAt: new Date().toISOString(),
    source: {
      app: PRODUCTION_APP,
      databaseApp: PRODUCTION_DATABASE_APP,
      database: PRODUCTION_DATABASE,
      querySha256: createHash('sha256').update(inventorySql).digest('hex'),
      readOnly: true,
    },
    health,
    version,
    machines,
    records,
  };
  const rendered = `${JSON.stringify(report, null, 2)}\n`;
  if (options.output) writeFileSync(resolve(options.output), rendered);
  else process.stdout.write(rendered);
}

main().catch((error) => {
  process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n`);
  process.exitCode = 1;
});
