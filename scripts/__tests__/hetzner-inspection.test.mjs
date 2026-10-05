import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, copyFileSync, writeFileSync, readFileSync, existsSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync, execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
const root = fileURLToPath(new URL('../../', import.meta.url));

test('retired Fly preflight and execution stop before any remote tool', t => {
  const dir = mkdtempSync(path.join(tmpdir(), 'tdf-retired-deploy-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const marker = path.join(dir, 'called');
  for (const tool of ['flyctl', 'docker', 'ssh', 'git']) {
    writeFileSync(path.join(dir, tool), '#!/bin/sh\n: > "$MARKER"\nexit 97\n', { mode: 0o700 });
  }
  for (const mode of ['preflight', 'release']) {
    const result = spawnSync(process.execPath, [path.join(root, 'scripts/production-release.mjs'), mode,
      '--sha', 'a'.repeat(40), '--execute', '--confirm', 'a'.repeat(40)], {
      env: { PATH: `${dir}:${process.env.PATH}`, MARKER: marker }, encoding: 'utf8',
    });
    assert.equal(result.status, 1);
    assert.match(result.stderr, /Fly production preflight\/release is retired/);
    assert.equal(existsSync(marker), false);
  }
});

test('inspector pins SSH target and suppresses raw failed-command output', t => {
  const dir = mkdtempSync(path.join(tmpdir(), 'tdf-hetzner-inspect-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const args = path.join(dir, 'args');
  const input = path.join(dir, 'input');
  const output = path.join(dir, 'receipt.json');
  const secret = 'SYNTHETIC_SECRET_MUST_NOT_LEAK';
  writeFileSync(path.join(dir, 'ssh'), '#!/bin/sh\nprintf "%s\\n" "$@" > "$ARGS"\ncat > "$INPUT"\nprintf "%s" "$SECRET" >&2\nexit 97\n', { mode: 0o700 });
  const result = spawnSync(process.execPath, [path.join(root, 'scripts/inspect-hetzner-production.mjs'),
    '--identity-file', path.join(dir, 'synthetic-key'), '--output', output], {
    encoding: 'utf8', env: { PATH: `${dir}:${process.env.PATH}`, ARGS: args, INPUT: input, SECRET: secret },
  });
  assert.equal(result.status, 1);
  assert.doesNotMatch(result.stdout + result.stderr, new RegExp(secret));
  assert.equal(existsSync(output), false);
  const actual = readFileSync(args, 'utf8').split('\n');
  for (const required of ['StrictHostKeyChecking=yes', 'IdentitiesOnly=yes', 'BatchMode=yes',
    '/dev/null', 'root@178.105.93.101', 'python3', '-']) assert.ok(actual.includes(required));
  assert.equal(readFileSync(input, 'utf8'), readFileSync(path.join(root, 'ops/hetzner/inspect-runtime.py'), 'utf8'));
});

for (const changeDuringRead of [false, true]) {
  test(`inspection ${changeDuringRead ? 'rejects changing' : 'records stable'} source provenance`, t => {
    const dir = mkdtempSync(path.join(tmpdir(), 'tdf-inspection-provenance-'));
    t.after(() => rmSync(dir, { recursive: true, force: true }));
    for (const name of ['scripts', 'ops/hetzner', 'bin']) mkdirSync(path.join(dir, name), { recursive: true });
    const inspector = path.join(dir, 'ops/hetzner/inspect-runtime.py');
    const launcher = path.join(dir, 'scripts/inspect-hetzner-production.mjs');
    copyFileSync(path.join(root, 'scripts/inspect-hetzner-production.mjs'), launcher);
    copyFileSync(path.join(root, 'ops/hetzner/inspect-runtime.py'), inspector);
    writeFileSync(path.join(dir, 'bin/ssh'), '#!/bin/sh\ncat >/dev/null\nif [ "$CHANGE" = true ]; then printf "\\n# changed during remote read\\n" >> "$INSPECTOR"; fi\nprintf "%s" "$RESULT"\n', { mode: 0o700 });
    for (const args of [['init', '-q'], ['add', '.'], ['-c', 'user.name=Test', '-c', 'user.email=test@example.invalid', '-c', 'commit.gpgsign=false', 'commit', '-qm', 'fixture']]) {
      execFileSync('git', args, { cwd: dir, stdio: 'ignore' });
    }
    const output = path.join(dir, 'receipt.json');
    const result = spawnSync(process.execPath, [launcher, '--identity-file', path.join(dir, 'synthetic-key'), '--output', output], {
      encoding: 'utf8', env: { PATH: `${dir}/bin:${process.env.PATH}`, CHANGE: String(changeDuringRead),
        INSPECTOR: inspector, RESULT: JSON.stringify({ publicBackend: { commit: 'a'.repeat(40) }, database: { migrations: [] } }) },
    });
    assert.equal(result.status, changeDuringRead ? 1 : 0, result.stderr);
    assert.equal(existsSync(output), !changeDuringRead);
    if (!changeDuringRead) {
      const receipt = JSON.parse(readFileSync(output, 'utf8'));
      assert.equal(receipt.sourceWorktreeDirty, false);
      assert.equal(receipt.sourceRevision, execFileSync('git', ['rev-parse', 'HEAD'], { cwd: dir, encoding: 'utf8' }).trim());
      assert.match(receipt.inspectorSha256, /^[a-f0-9]{64}$/);
      assert.match(receipt.launcherSha256, /^[a-f0-9]{64}$/);
    }
  });
}
