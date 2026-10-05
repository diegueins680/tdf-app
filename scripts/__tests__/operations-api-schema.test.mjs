import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';
import Ajv from 'ajv';
import { parse } from 'yaml';

const contract = parse(readFileSync(new URL('../../tdf-hq/docs/openapi/api.yaml', import.meta.url), 'utf8'));
const metadata = { expectedVersion: 1, requestId: 'synthetic', sourceClient: 'conformance' };
function validator(name, document = contract) {
  // JSON numbers in this JS harness cannot represent every int64 exactly.
  // These fixtures exercise structure and representable boundary values only.
  const ajv = new Ajv({ allErrors: true, nullable: true });
  ajv.addFormat('int64', { type: 'number', validate: value => Number.isInteger(value)
    && BigInt(value) >= -(1n << 63n) && BigInt(value) < (1n << 63n) });
  return ajv.compile({
    components: document.components, $ref: `#/components/schemas/${name}`,
  });
}

for (const [name, body] of [
  ['OperationsVersionedCommand', metadata],
  ['OperationsTransitionCommand', { ...metadata, targetStatus: 'resolved', reason: 'Synthetic' }],
  ['OperationsAssignmentCommand', { ...metadata, assigneePartyId: 12, responsibleTeam: null }],
  ['OperationsPriorityCommand', { ...metadata, priority: 'high', reason: 'Synthetic' }],
]) {
  test(`${name} accepts its mounted HTTP payload and forward-compatible fields`, () => {
    const validate = validator(name);
    assert.equal(validate(body), true, JSON.stringify(validate.errors));
    assert.equal(validate({ ...body, clientDiagnostic: 'extra' }), true, JSON.stringify(validate.errors));
    for (const invalid of [{ expectedVersion: 0 }, { expectedVersion: '1' }, { requestId: '' }, { sourceClient: '' }]) {
      assert.equal(validate({ ...body, ...invalid }), false);
    }
    const missing = { ...body }; delete missing.expectedVersion;
    assert.equal(validate(missing), false);
  });
}

test('a closed shared base detects the original impossible derived-schema defect', () => {
  const mutated = structuredClone(contract);
  mutated.components.schemas.OperationsVersionedCommand.additionalProperties = false;
  for (const [name, extra] of [['OperationsTransitionCommand', { targetStatus: 'resolved' }],
                              ['OperationsAssignmentCommand', { assigneePartyId: 12 }]]) {
    const validate = validator(name, mutated);
    assert.equal(validate({ ...metadata, ...extra }), false);
    assert.ok(validate.errors.some(error => error.keyword === 'additionalProperties'));
  }
});

test('approval requests and decisions enforce declared representation constraints', () => {
  const validate = validator('OperationsApprovalCreate');
  const body = { organizationId: '00000000-0000-0000-0000-000000000001', actionType: 'refund',
    targetEntityType: 'payment', targetEntityId: 'synthetic', reason: 'Synthetic', idempotencyKey: 'synthetic',
    requestId: 'synthetic', sourceClient: 'conformance', amountMinor: 10, currency: 'USD' };
  assert.equal(validate(body), true, JSON.stringify(validate.errors));
  for (const invalid of [{ amountMinor: -1 }, { currency: 'usd' }, { requestId: '' }, { sourceClient: '' }]) {
    assert.equal(validate({ ...body, ...invalid }), false);
  }
  const decide = validator('OperationsApprovalDecision');
  const decision = { requestId: 'synthetic', sourceClient: 'conformance', decision: 'approved',
    expectedDecision: 'pending', reason: 'Synthetic' };
  assert.equal(decide(decision), true);
  assert.equal(decide({ ...decision, expectedDecision: 'approved' }), false);
});
