import assert from 'node:assert/strict';
import test from 'node:test';
import { readFileSync } from 'node:fs';
import { alloyControls, alloyCommands, mutateAlloy, validateAlloyCoverage, admitAlloyResult } from '../lib/alloy-controls.mjs';

test('every checked assertion has a mutation that preserves assertions and command scopes', () => {
  let count = 0;
  for (const { model, controls } of alloyControls) {
    const source = readFileSync(new URL(`../../formal/event-operations/${model}`, import.meta.url), 'utf8');
    const commands = validateAlloyCoverage(source, controls);
    for (const control of controls) {
      const changed = mutateAlloy(source, control);
      assert.notEqual(changed, source);
      assert.deepEqual(alloyCommands(changed), commands);
      assert.equal(changed.slice(changed.indexOf('\nassert ')), source.slice(source.indexOf('\nassert ')),
        'All assertions and scopes must remain byte-identical');
      count++;
    }
    assert.throws(() => validateAlloyCoverage(source + '\ncheck AddedAssertion for 4\n', controls), /coverage drift/);
    assert.throws(() => validateAlloyCoverage('assert AddedAssertion { some none }\n' + source, controls), /coverage drift/);
    assert.throws(() => validateAlloyCoverage(source, controls.slice(1)), /coverage drift/);
    assert.throws(() => validateAlloyCoverage(source, [...controls, controls[0]]), /coverage drift/);
  }
  assert.equal(count, 15);
});

test('mutations reject missing, duplicate, no-op, assertion, comment-only and unclosed targets', () => {
  const source = 'pred guard { x = y }\nassert A { x = y }\ncheck A for 4\n';
  const control = ['test', 'A', 'pred guard', 'x = y', 'no none'];
  for (const changed of [source.replace('x = y', 'x = z'), source.replace('pred guard { x = y }', 'pred guard { x = y and x = y }'), source + '\npred guard { x = y }', source.replace('pred guard { x = y }', 'pred guard { // x = y\n no none }'), 'pred guard { x = y']) {
    assert.throws(() => mutateAlloy(changed, control));
  }
  assert.throws(() => mutateAlloy(source, ['test', 'A', 'assert A', 'x = y', 'no none']));
  assert.throws(() => mutateAlloy(source, ['test', 'A', 'pred guard', 'x = y', 'x = y']));
  assert.throws(() => alloyCommands('label: check A for 4'));
  assert.throws(() => alloyCommands('check { no none } for 4'));
  assert.throws(() => alloyCommands('check A for 4\ncheck A for 4'));
});

function evidence(expected = 'SAT') {
  return { status: 0, signal: null, log: `00. check A       ${expected === 'SAT' ? '0\b\b\b\b\b    1/1' : '0'}       ${expected}\n`,
    receipt: { solver: 'sat4j', commands: { A: { name: 'A', type: 'check', source: 'check A for 4', overall: 4, bitwidth: 4,
      ...(expected === 'SAT' ? { solution: [{ instances: [{ values: { 'Party$0': {} } }] }] } : {}) } } },
    command: { type: 'check', name: 'A', source: 'check A for 4', overall: 4, bitwidth: 4 }, expected };
}
test('named completed results require matching solver receipts and SAT witnesses', () => {
  for (const expected of ['SAT', 'UNSAT']) {
    assert.doesNotThrow(() => admitAlloyResult(evidence(expected)));
    for (const change of [{ status: 1 }, { signal: 'SIGTERM' }, { error: new Error('timeout') }, { log: '' },
      { log: 'Error: SAT solver failed' }, { log: `00. check Wrong 0 ${expected}\n` },
      { log: `00. check A 0 SAT\n01. check A 0 UNSAT\n` }, { receipt: {} },
      { expected: expected === 'SAT' ? 'UNSAT' : 'SAT' }]) {
      assert.throws(() => admitAlloyResult({ ...evidence(expected), ...change }));
    }
  }
  for (const modify of [x => { x.receipt.commands.A.solution = []; },
    x => { x.receipt.commands.A.solution = [{}]; },
    x => { x.receipt.commands.A.solution = [{ instances: [] }]; },
    x => { x.receipt.commands.A.solution = [{ instances: [{}] }]; },
    x => { x.receipt.commands.A.solution = [{ instances: [{ values: {} }] }]; },
    x => { delete x.receipt.commands.A.source; }, x => { delete x.receipt.commands.A.overall; },
    x => { x.receipt.commands.A.source = 'check A for 5'; },
    x => { x.receipt.commands.A.bitwidth = 5; },
    x => { x.receipt.commands.A.type = 'run'; }, x => { x.receipt.commands.B = {}; },
    x => { x.receipt.solver = 'other'; }]) {
    const x = evidence(); modify(x); assert.throws(() => admitAlloyResult(x));
  }
});
