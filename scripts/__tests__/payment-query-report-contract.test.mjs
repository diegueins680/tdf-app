import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';
import YAML from 'yaml';

const read = (name) => readFileSync(new URL(`../../${name}`, import.meta.url), 'utf8');
const api = YAML.parse(read('tdf-hq/docs/openapi/api.yaml'));
const route = api.paths['/admin/commerce/provider-queries'];
const schemas = api.components.schemas;

test('query report is a protected, bounded, non-cacheable GET-only contract', () => {
  assert.deepEqual(Object.keys(route), ['get']);
  assert.deepEqual(route.get.security, [{ bearerAuth: [] }]);
  const params = Object.fromEntries(route.get.parameters.map((param) => [param.name, param.schema]));
  assert.equal(params.environment.default, 'sandbox');
  assert.deepEqual(params.environment.enum, ['sandbox', 'production']);
  assert.equal(params.limit.maximum, 100);
  assert.equal(params.offset.maximum, 10000);
  assert.equal(schemas.CommerceProviderQueries.properties.cpqsJobs.maxItems, 100);
  assert.deepEqual(route.get.responses['200'].headers['Cache-Control'].schema.enum, ['no-store']);
  for (const code of ['400', '401', '403', '503']) assert.ok(route.get.responses[code]);
});

test('diagnostic contract exactly matches the fixed Haskell redaction projection', () => {
  const source = read('tdf-hq/src/TDF/Server/CommerceOperations.hs');
  const projection = source.split('providerQueryOutcome code = case code of')[1].split('loadProviderQueries ::')[0];
  const cases = [...projection.matchAll(/"([a-z_]+)" -> "([a-z_]+)"/g)];
  for (const [, input, output] of cases) assert.equal(input, output);
  const values = [...cases.map((match) => match[2]), 'unrecognized'].sort();
  assert.ok(projection.includes('_ -> "unrecognized"'));
  assert.deepEqual(schemas.CommerceProviderQuery.properties.cpqLastOutcome.enum.slice().sort(), values);
});

test('report schemas expose required explicit availability and no raw payload or lease token', () => {
  for (const name of ['CommerceProviderQueries', 'CommerceProviderQuery', 'CommerceProviderQueryBudget']) {
    const schema = schemas[name];
    assert.equal(schema.additionalProperties, false);
    assert.deepEqual(schema.required.slice().sort(), Object.keys(schema.properties).sort());
    for (const field of Object.keys(schema.properties)) {
      assert.doesNotMatch(field, /payload|token|merchant|secret|email|phoneNumber|providerResource|redirect/i);
    }
  }
  assert.equal(schemas.CommerceProviderQueries.properties.cpqsSchemaReady.type, 'boolean');
  assert.ok(schemas.CommerceProviderQuery.properties.cpqOperationStatus);
  assert.ok(schemas.CommerceProviderQuery.properties.cpqOutcomeCertainty);
});
