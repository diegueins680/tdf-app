import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';
import YAML from 'yaml';

const read = (name) => readFileSync(new URL(`../../${name}`, import.meta.url), 'utf8');
const api = YAML.parse(read('tdf-hq/docs/openapi/api.yaml'));
const route = api.paths['/admin/commerce/provider-queries'];
const schemas = api.components.schemas;

test('held-refund readiness and explicit query commands require auth and forbid cached evidence', () => {
  const refund = api.paths['/admin/services/storefront/refunds/{refundId}/reconcile'];
  assert.equal(refund.parameters[0].schema.format, 'uuid');
  for (const method of ['get', 'post']) {
    assert.deepEqual(refund[method].security, [{ bearerAuth: [] }]);
    assert.deepEqual(refund[method].responses['200'].headers['Cache-Control'].schema.enum, ['no-store']);
    for (const code of ['400', '401', '403', '404', '409', '503']) assert.ok(refund[method].responses[code]);
    assert.equal(refund[method].requestBody, undefined);
  }
  assert.ok(refund.post.responses['429'].headers['Retry-After']);
  assert.ok(refund.post.responses['502']);
});

test('held-refund projection carries exact money and no external IDs or provider payload', () => {
  const refund = schemas.ServiceStorefrontRefundRecovery;
  assert.deepEqual(refund.required.slice().sort(), Object.keys(refund.properties).sort());
  assert.equal(refund.properties.ssrrAmountMinor.type, 'string');
  const pattern = new RegExp(refund.properties.ssrrAmountMinor.pattern);
  assert.ok(pattern.test('9223372036854775807'));
  assert.ok(!pattern.test('1.5'));
  assert.ok(!pattern.test('0'));
  for (const field of Object.keys(refund.properties)) {
    assert.doesNotMatch(field, /payload|token|merchant|secret|email|providerResource|redirect|capture/i);
  }
});

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

test('reconciliation evidence is a bounded, protected GET with explicit filter echoes', () => {
  const evidence = api.paths['/admin/commerce/reconciliation-exceptions'];
  assert.deepEqual(Object.keys(evidence), ['get']);
  assert.deepEqual(evidence.get.security, [{ bearerAuth: [] }]);
  const params = Object.fromEntries(evidence.get.parameters.map((param) => [param.name, param.schema]));
  assert.equal(params.environment.default, 'sandbox');
  assert.deepEqual(params.environment.enum, ['sandbox', 'production']);
  assert.deepEqual(params.status.enum, ['open', 'assigned', 'resolved', 'ignored']);
  assert.equal(params.checkoutId.format, 'uuid');
  assert.equal(params.limit.maximum, 100);
  assert.equal(params.offset.maximum, 10000);
  assert.equal(schemas.CommerceReconciliationReport.properties.crrEntries.maxItems, 100);
  for (const field of ['crrStatus', 'crrCheckoutId', 'crrEnvironment', 'crrOffset', 'crrLimit']) {
    assert.ok(schemas.CommerceReconciliationReport.properties[field]);
  }
  assert.deepEqual(evidence.get.responses['200'].headers['Cache-Control'].schema.enum, ['no-store']);
  for (const code of ['400', '401', '403', '503']) assert.ok(evidence.get.responses[code]);
});

test('reconciliation projection matches safe Haskell classifications, never private stored values', () => {
  const source = read('tdf-hq/src/TDF/Server/CommerceOperations.hs');
  const projections = [
    ['safeProvider provider = case provider of', 'safeStatus state =', 'creProvider'],
    ['safeStatus state = case state of', 'safeReason reason =', 'creStatus'],
    ['safeReason reason = case reason of', '-- The gate above', 'creReason'],
  ];
  for (const [start, end, field] of projections) {
    const projection = source.split(start)[1].split(end)[0];
    const outputs = [...projection.matchAll(/"[a-z_]+" -> "([a-z_]+)"/g)].map((match) => match[1]);
    assert.ok(projection.includes('_ -> "unrecognized"'));
    assert.deepEqual(schemas.CommerceReconciliationEntry.properties[field].enum.slice().sort(), [...outputs, 'unrecognized'].sort());
  }
});

test('reconciliation money stays nullable exact strings and sensitive evidence stays outside the contract', () => {
  for (const name of ['CommerceReconciliationEntry', 'CommerceReconciliationReport']) {
    const schema = schemas[name];
    assert.equal(schema.additionalProperties, false);
    assert.deepEqual(schema.required.slice().sort(), Object.keys(schema.properties).sort());
    for (const field of Object.keys(schema.properties)) {
      assert.doesNotMatch(field, /payload|token|merchant|secret|email|phoneNumber|providerResource|redirect|notes|assignedParty/i);
    }
  }
  for (const field of ['creExpectedMinor', 'creActualMinor']) {
    const money = schemas.CommerceReconciliationEntry.properties[field];
    assert.equal(money.type, 'string');
    assert.equal(money.nullable, true);
    assert.ok(new RegExp(money.pattern).test('9223372036854775807'));
    assert.ok(new RegExp(money.pattern).test('-9223372036854775808'));
    assert.ok(!new RegExp(money.pattern).test('1.5'));
  }
  for (const field of ['creCheckoutId', 'crePaymentAttemptId']) {
    assert.equal(schemas.CommerceReconciliationEntry.properties[field].format, 'uuid');
    assert.equal(schemas.CommerceReconciliationEntry.properties[field].nullable, true);
  }
});
