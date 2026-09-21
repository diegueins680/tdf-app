import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
import { pinnedToolchain, verifyDirectory, verifyTranslation } from '../verify-event-operations-pluscal.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const modelPath = path.join(root, 'formal/event-operations/ReservationRace.tla');
const source = readFileSync(modelPath, 'utf8');
// Intentionally mandatory: missing/unpinned Java tools are a failure, not a skipped check.
const toolchain = pinnedToolchain();

function fixture(t, text) {
  const dir = mkdtempSync(path.join(tmpdir(), 'tdf-pluscal-test-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const file = path.join(dir, 'ReservationRace.tla');
  writeFileSync(file, text);
  return { dir, file };
}

test('pinned regeneration matches the committed model without editing its source or siblings', t => {
  const { dir, file } = fixture(t, source);
  assert.equal(verifyTranslation(file, toolchain), 'ReservationRace.tla');
  assert.equal(readFileSync(file, 'utf8'), source);
  assert.deepEqual(readdirSync(dir), ['ReservationRace.tla']);
});

const [prefix, generated] = source.split('\\* BEGIN TRANSLATION');
const mutations = [
  ['algorithm change without regeneration', prefix.replace('if self \\notin seenCommands then', 'if TRUE then') + '\\* BEGIN TRANSLATION' + generated],
  ['translation-only semantic change', prefix + '\\* BEGIN TRANSLATION' + generated.replace('IF self \\notin seenCommands', 'IF TRUE')],
  ['forged TLA checksum', source.replace('chksum(tla) = "aaece465"', 'chksum(tla) = "00000000"')],
  ['forged PlusCal checksum', source.replace('chksum(pcal) = "14601121"', 'chksum(pcal) = "00000000"')],
  ['translation whitespace change', source.replace('VARIABLES booking,', 'VARIABLES  booking,')],
  // Exact previous committed formatting: the default translator emitted two
  // trailing spaces, subsequently trimmed without updating its checksum.
  ['original stale default-width translation', source.replace('aaece465', '6057351e')
    .replace('UNCHANGED << booking, overrideUsed >>', 'UNCHANGED << booking,\n                                                                  overrideUsed >>')
    .replace('UNCHANGED << booking, seenCommands, overrideUsed, audit >>', 'UNCHANGED << booking, seenCommands,\n                                            overrideUsed, audit >>')],
];

for (const [name, mutated] of mutations) {
  test(`rejects ${name} and preserves the rejected input`, t => {
    assert.notEqual(mutated, source, 'mutation must change the actual fixture');
    const { dir, file } = fixture(t, mutated);
    assert.throws(() => verifyTranslation(file, toolchain), /PlusCal translation drift/);
    assert.equal(readFileSync(file, 'utf8'), mutated);
    assert.deepEqual(readdirSync(dir), ['ReservationRace.tla']);
  });
}

test('missing, duplicate and orphaned translation blocks fail closed', t => {
  for (const mutated of [prefix, source.replace('--algorithm', '--removed'),
    source + '\n\\* BEGIN TRANSLATION extra\n', source.replace('\\* END TRANSLATION', '')]) {
    const { file } = fixture(t, mutated);
    assert.throws(() => verifyTranslation(file, toolchain), /Missing or ambiguous/);
  }
});

test('syntax errors and missing Java are failures, not successful no-change translations', t => {
  const broken = source.replace('seenCommands := seenCommands', 'seenCommands :@= seenCommands');
  const { file } = fixture(t, broken);
  assert.throws(() => verifyTranslation(file, toolchain), /PlusCal translation failed/);
  assert.throws(() => verifyTranslation(modelPath, { ...toolchain, javaBin: '/nonexistent/tdf-java' }),
    /PlusCal translation failed/);
  assert.throws(() => verifyTranslation(modelPath, { ...toolchain, javaBin: 'true' }),
    /PlusCal translation failed/);
  assert.equal(readFileSync(file, 'utf8'), broken);
});

test('directory scan includes new PlusCal files and cannot silently lose ReservationRace', t => {
  const { dir, file } = fixture(t, source);
  assert.deepEqual(verifyDirectory(dir, toolchain), ['ReservationRace.tla']);
  const second = source.replaceAll('ReservationRace', 'AnotherRace');
  writeFileSync(path.join(dir, 'AnotherRace.tla'), second.replace('IF self \\notin seenCommands', 'IF TRUE'));
  assert.throws(() => verifyDirectory(dir, toolchain), /AnotherRace/);
  writeFileSync(path.join(dir, 'AnotherRace.tla'), '---- MODULE AnotherRace ----\n====\n');
  writeFileSync(file, '---- MODULE ReservationRace ----\n====\n');
  assert.throws(() => verifyDirectory(dir, toolchain), /Missing or ambiguous/);
});

test('missing and unpinned toolchains fail before running a translator', t => {
  assert.throws(() => pinnedToolchain({}), /TLA2TOOLS_JAR must point/);
  const { file } = fixture(t, 'not a jar');
  assert.throws(() => pinnedToolchain({ TLA2TOOLS_JAR: file }), /Unexpected TLA\+ tools checksum/);
});

test('CLI rejects unsupported options without modifying the model', () => {
  const result = spawnSync(process.execPath, [path.join(root, 'scripts/verify-event-operations-pluscal.mjs'), '--update'],
    { cwd: root, encoding: 'utf8', timeout: 5000 });
  assert.ifError(result.error);
  assert.equal(result.status, 1);
  assert.match(result.stderr, /accepts no command-line arguments/);
  assert.equal(readFileSync(modelPath, 'utf8'), source);
});

test('formal runner and both hosted triggers retain the fail-closed integrity gate', () => {
  const runner = readFileSync(path.join(root, 'scripts/verify-event-operations-formal.sh'), 'utf8');
  const workflow = readFileSync(path.join(root, '.github/workflows/event-operations-formal.yml'), 'utf8');
  const gate = runner.indexOf('node "${SCRIPT_DIR}/verify-event-operations-pluscal.mjs"');
  assert.ok(gate >= 0 && gate < runner.indexOf('run_tlc EventLifecycle.tla'));
  assert.match(runner, /node --test "\$\{SCRIPT_DIR\}\/__tests__\/event-operations-pluscal.test.mjs"/);
  // Every implementation/configuration change invalidates the consolidated run.
  assert.doesNotMatch(workflow, /^\s+paths(?:-ignore)?:/m);
  assert.match(workflow, /^  pull_request:/m);
  assert.match(workflow, /^  push:/m);
  assert.doesNotMatch(workflow, /continue-on-error/);
});
