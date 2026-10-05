import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, copyFileSync, writeFileSync, readFileSync, existsSync, rmSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync, execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
const root = fileURLToPath(new URL('../../', import.meta.url));
const files = ['scripts/rehearse-hetzner-restore.mjs', 'ops/hetzner/rehearse-postgres-restore.py', 'ops/hetzner/inspect-runtime.py'];
const passing = { status: 'isolated-database-restore-passed', isolateRemoved: true,
  productionDatabaseWritten: false, tableCounts: { 'public.synthetic': 1 }, migrationCount: 159 };
for (const mode of ['pass', 'dirty', 'existing', 'remote-error', 'changed-source', 'incomplete', 'not-removed', 'production-write']) {
  test(`restore launcher: ${mode}`, t => {
    const dir = mkdtempSync(path.join(tmpdir(), 'tdf-restore-launcher-'));
    t.after(() => rmSync(dir, { recursive: true, force: true }));
    for (const name of ['scripts', 'ops/hetzner', 'bin']) mkdirSync(path.join(dir, name), { recursive: true });
    for (const file of files) copyFileSync(path.join(root, file), path.join(dir, file));
    const marker = path.join(dir, 'called'), output = path.join(dir, 'receipt.json');
    const secret = 'SYNTHETIC_RESTORE_SECRET_MUST_NOT_LEAK';
    writeFileSync(path.join(dir, 'bin/ssh'), '#!/bin/sh\nprintf "%s\\n" "$@" > "$MARKER"\ncat > "$INPUT"\nif [ "$MODE" = remote-error ]; then printf "%s" "$SECRET" >&2; exit 97; fi\nif [ "$MODE" = changed-source ]; then printf "\\n# concurrent change\\n" >> "$SOURCE"; fi\nprintf "%s" "$RESULT"\n', { mode: 0o700 });
    writeFileSync(path.join(dir, '.gitignore'), '/called\n/input\n/receipt.json\n');
    for (const args of [['init', '-q'], ['add', '.'], ['-c', 'user.name=Test', '-c', 'user.email=test@example.invalid', '-c', 'commit.gpgsign=false', 'commit', '-qm', 'fixture']]) execFileSync('git', args, { cwd: dir, stdio: 'ignore' });
    if (mode === 'dirty') writeFileSync(path.join(dir, 'unreviewed'), 'dirty');
    if (mode === 'existing') writeFileSync(output, 'preserve');
    const snapshot = { ...passing };
    if (mode === 'incomplete') snapshot.status = 'started';
    if (mode === 'not-removed') snapshot.isolateRemoved = false;
    if (mode === 'production-write') snapshot.productionDatabaseWritten = true;
    const result = spawnSync(process.execPath, [path.join(dir, files[0]), '--identity-file', path.join(dir, 'key'), '--output', output], {
      encoding: 'utf8', env: { PATH: `${dir}/bin:${process.env.PATH}`, MODE: mode, MARKER: marker,
        INPUT: path.join(dir, 'input'), SOURCE: path.join(dir, files[1]), SECRET: secret, RESULT: JSON.stringify(snapshot) },
    });
    assert.equal(result.status, mode === 'pass' ? 0 : 1, result.stderr);
    assert.doesNotMatch(result.stdout + result.stderr, new RegExp(secret));
    assert.equal(existsSync(marker), !['dirty', 'existing'].includes(mode));
    assert.equal(existsSync(output), ['pass', 'existing'].includes(mode));
    if (mode === 'existing') assert.equal(readFileSync(output, 'utf8'), 'preserve');
    if (mode === 'pass') {
      const receipt = JSON.parse(readFileSync(output, 'utf8'));
      assert.equal(receipt.dirty, false);
      assert.match(receipt.revision, /^[a-f0-9]{40}$/);
      for (const file of files) assert.match(receipt.sources[file], /^[a-f0-9]{64}$/);
      assert.match(receipt.transmittedSha256, /^[a-f0-9]{64}$/);
      assert.equal(statSync(output).mode & 0o777, 0o600);
      const args = readFileSync(marker, 'utf8').split('\n');
      for (const value of ['root@178.105.93.101', 'StrictHostKeyChecking=yes', 'IdentitiesOnly=yes', 'BatchMode=yes', '/dev/null', 'timeout', '540', 'python3', '-']) assert.ok(args.includes(value));
      const program = readFileSync(path.join(dir, 'input'), 'utf8');
      for (const file of files.slice(1)) assert.ok(program.includes(readFileSync(path.join(dir, file)).toString('base64')));
    }
  });
}
