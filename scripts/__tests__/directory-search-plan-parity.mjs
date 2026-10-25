// Compare shipped public SQL with the immutable pre-repair SQL using one
// deterministic fixture/snapshot. Timings describe this fixture only.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';

const database = process.env.TDF_DIRECTORY_ALERT_PARITY_DATABASE_URL;
disposablePostgresUrl(database, { ci: process.env.CI === 'true' });
const reference = '512d590e902760130f54ca2927620c001a754b85';
const exported = (revision) => JSON.parse(execFileSync('python3', ['scripts/test-community-search.py', '--emit-sql', ...(revision ? [revision] : [])], { encoding: 'utf8' }));
const before = exported(reference);
const after = exported();
const quote = (value) => value == null ? 'NULL' : typeof value === 'number' ? String(value) : typeof value === 'boolean' ? String(value) : "'" + String(value).replaceAll("'", "''") + "'";
function render(template, params) {
  const chunks = template.split('?');
  assert.equal(chunks.length, params.length + 1);
  return chunks[0] + params.map((value, i) => quote(value) + chunks[i + 1]).join('');
}
const q = (query = '', city = null, genre = null, latitude = null, longitude = null, radius = null) => [query, 'classified', city, latitude, longitude, radius, null, null, null, genre, null, null, null, null, null, 'suppressed', null, 50];
const cases = [
  ['empty', q()],
  ['selective-city', q('', '24000000-0000-4000-8000-000000000002')],
  ['selective-text', q('uniqueraretoken')],
  ['substring', q('ordinary')],
  ['catalog-alias', q('syntheticplanalias')],
  ['genre', q('', null, '2109e4ab-c2b5-493a-aa5e-97c00dd6fa9c')],
  ['geo-unchanged', q('', null, null, -0.180653, -78.467834, 1)],
];
let fixture = `BEGIN;
 SET LOCAL standard_conforming_strings=on;
 SET LOCAL jit=off;
 INSERT INTO catalog_search_alias(catalog_id,entity_kind,entity_id,locale_id,term,normalized_term,source)
 SELECT catalog_id,entity_kind,entity_id,locale_id,'syntheticplanalias','syntheticplanalias','manual'
 FROM catalog_search_alias ORDER BY id LIMIT 1;
 INSERT INTO directory_search_document(entity_kind,entity_id,slug,title,city_id,
  public_latitude,public_longitude,profession_ids,genre_ids,search_text,search_vector,
  availability_score,remote,source_status,visibility,moderation_status,effective_at,source_updated_at)
 SELECT 'classified','plan-parity-'||i,'plan-parity-'||i,'Plan parity '||i,
  CASE WHEN i%100=0 THEN '24000000-0000-4000-8000-000000000002'::uuid
    ELSE (SELECT id FROM city_reference WHERE active AND id<>'24000000-0000-4000-8000-000000000002' ORDER BY id LIMIT 1) END,
  CASE WHEN i%100=0 THEN -0.180653 ELSE -2.17 END,
  CASE WHEN i%100=0 THEN -78.467834 ELSE -79.92 END,
  CASE WHEN i%97=0 THEN ARRAY[(SELECT entity_id FROM catalog_search_alias WHERE normalized_term='syntheticplanalias' LIMIT 1)] ELSE '{}'::uuid[] END,
  CASE WHEN i%11=0 THEN ARRAY['2109e4ab-c2b5-493a-aa5e-97c00dd6fa9c'::uuid] ELSE '{}'::uuid[] END,
  CASE WHEN i=1 THEN 'uniqueraretoken' ELSE 'ordinary synthetic searchable listing '||i END,
  to_tsvector('simple',CASE WHEN i=1 THEN 'uniqueraretoken' ELSE 'ordinary synthetic searchable listing '||i END),
  CASE WHEN i%5=0 THEN 1 ELSE 0 END,i%2=0,'published','public','allowed',now()-interval '1 hour',now()
 FROM generate_series(1,4000) i;
 ANALYZE directory_search_document;
 CREATE FUNCTION pg_temp.explain_query(statement text) RETURNS jsonb LANGUAGE plpgsql AS $explain$
 DECLARE plan jsonb; BEGIN EXECUTE 'EXPLAIN (ANALYZE,BUFFERS,FORMAT JSON) '||statement INTO plan; RETURN plan; END
 $explain$;
`;
for (const [name, params] of cases) {
  const oldQuery = render(before.search, params), newQuery = render(after.search, params);
  const oldFacets = render(before.facets, params.slice(0, -2)), newFacets = render(after.facets, params.slice(0, -2));
  fixture += `SELECT jsonb_build_object('name',${quote(name)},
    'beforeRows',coalesce((SELECT jsonb_agg(to_jsonb(value)) FROM (${oldQuery}) value),'[]'::jsonb),
    'afterRows',coalesce((SELECT jsonb_agg(to_jsonb(value)) FROM (${newQuery}) value),'[]'::jsonb),
    'beforeFacets',(SELECT to_jsonb(value) FROM (${oldFacets}) value),
    'afterFacets',(SELECT to_jsonb(value) FROM (${newFacets}) value),
    'beforePlan',pg_temp.explain_query(${quote(oldQuery)}),
    'afterPlan',pg_temp.explain_query(${quote(newQuery)}));\n`;
}
fixture += 'ROLLBACK;';
const output = execFileSync('psql', ['-XqAt', '-v', 'ON_ERROR_STOP=1', '-d', database], {
  input: fixture, encoding: 'utf8', maxBuffer: 64 * 1024 * 1024,
});
const rows = output.split('\n').filter((line) => line.startsWith('{')).map((line) => JSON.parse(line));
assert.equal(rows.length, cases.length);
function nodes(plan) { return [plan, ...(plan.Plans ?? []).flatMap(nodes)]; }
for (const row of rows) {
  assert.deepEqual(row.afterRows, row.beforeRows, `${row.name}: result IDs, values, scores and order changed`);
  assert.deepEqual(row.afterFacets, row.beforeFacets, `${row.name}: facet counts changed`);
  assert.ok(!JSON.stringify(row.afterPlan).includes('directory_matches_non_geo_filters'), `${row.name}: filter helper was not inlined`);
  assert.ok(!JSON.stringify(row.afterPlan).includes('directory_matches_entity_and_derivation'), `${row.name}: entity helper was not inlined`);
  const oldNodes = nodes(row.beforePlan[0].Plan), newNodes = nodes(row.afterPlan[0].Plan);
  const oldIndexes = oldNodes.map((node) => node['Index Name']).filter(Boolean);
  const newIndexes = newNodes.map((node) => node['Index Name']).filter(Boolean);
  if (row.name === 'selective-city') {
    for (const index of oldIndexes) assert.ok(newIndexes.includes(index), `Selective indexed path lost: ${index}`);
  }
  for (const node of newNodes.filter((node) => node['CTE Name'] === 'directory_query_aliases')) {
    assert.ok(node['Actual Loops'] <= 1, `${row.name}: alias lookup repeated per document`);
  }
  console.log(JSON.stringify({ name: row.name, rows: row.afterRows.length,
    beforeMs: row.beforePlan[0]['Execution Time'], afterMs: row.afterPlan[0]['Execution Time'],
    beforeHitBlocks: row.beforePlan[0].Plan['Shared Hit Blocks'], afterHitBlocks: row.afterPlan[0].Plan['Shared Hit Blocks'],
    beforeIndexes: oldIndexes, afterIndexes: newIndexes,
    scope: '4000 synthetic documents; identical snapshot and source-bound queries; timings are descriptive, not a production guarantee' }));
}
console.log('PASS: IDs/scores/order/facets, scalar-helper inlining, observed indexed paths and once-per-query alias evaluation');
