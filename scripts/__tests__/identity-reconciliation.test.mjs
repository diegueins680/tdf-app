import { test } from 'node:test';
import assert from 'node:assert/strict';
import { candidateGroups, inventorySummary, sqlText, assertUuid } from '../lib/identity-reconciliation.mjs';
const p = (id, values = {}) => ({ id, is_org: false, ...values });
test('shared attributes never confirm identity, even when all contact fields agree', () => {
  const parties = [1, 2, 3].map(id => p(id, { display_name: 'Same', primary_email: 'shared@example.test', primary_phone: '+12345' }));
  const inventory = { parties, credentials: [{ party_id: 1 }, { party_id: 2 }] };
  assert.equal(inventorySummary(inventory).confirmed_groups, 0);
  assert.equal(candidateGroups(inventory)[0].reason, 'multiple-credentials-require-ownership-review');
});
test('pairwise chains do not create a whole-group proof', () => {
  const groups = candidateGroups({ parties: [p(1, { primary_email: 'a' }), p(2, { primary_email: 'a', primary_phone: 'b' }), p(3, { primary_phone: 'b' })] });
  assert.deepEqual(groups.map(g => g.member_ids), [[1, 2], [2, 3]]);
});
test('preserves local email case, plus tags, dots, phone prefixes and names with accents', () => {
  const values = ['A@x', 'a@x', 'a+b@x', 'ab@x', 'a.b@x', '+123', '00123', 'Jose', 'José'];
  assert.deepEqual(candidateGroups({ parties: values.map((v, i) => p(i + 1, { display_name: v })) }), []);
});
test('organizations and people remain review candidates, never automatic matches', () => {
  assert.equal(candidateGroups({ parties: [p(1, { display_name: 'A' }), p(2, { display_name: 'A', is_org: true })] })[0].reason, 'person-and-organization-boundary');
});
test('permutation invariance over bounded generated inventories', () => {
  let state = 42;
  const next = () => (state = (Math.imul(state, 1664525) + 1013904223) >>> 0);
  for (let iteration = 0; iteration < 300; iteration += 1) {
    const parties = Array.from({ length: next() % 30 }, (_, i) => p(i + 1, { display_name: `n${next() % 10}`, primary_email: `e${next() % 8}` }));
    assert.deepEqual(candidateGroups({ parties }), candidateGroups({ parties: [...parties].reverse() }));
    assert.ok(candidateGroups({ parties }).every(g => g.classification === 'review' && new Set(g.member_ids).size === g.member_ids.length));
  }
});
test('SQL literal and identifier inputs are bounded without shell interpolation', () => {
  assert.equal(sqlText("a'b\\c"), "'a''b\\c'");
  assert.throws(() => sqlText('a\0b'));
  assert.throws(() => assertUuid("'; DELETE FROM party; --"));
});
