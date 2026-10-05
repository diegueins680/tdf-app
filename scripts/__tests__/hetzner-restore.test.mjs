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

for (const mode of ['pass', 'wrong-sql', 'wrong-manifest', 'incomplete-ledger']) {
  test(`candidate migration launcher: ${mode}`, t => {
    const dir = mkdtempSync(path.join(tmpdir(), 'tdf-migration-launcher-'));
    t.after(() => rmSync(dir, { recursive: true, force: true }));
    const helpers = ['scripts/lib/migration-contract.mjs', 'scripts/lib/production-release.mjs', 'scripts/lib/hetzner-migration-rehearsal.mjs'];
    for (const name of ['scripts/lib', 'ops/hetzner', 'tdf-hq/sql', 'bin']) mkdirSync(path.join(dir, name), { recursive: true });
    for (const file of [...files, ...helpers]) copyFileSync(path.join(root, file), path.join(dir, file));
    writeFileSync(path.join(dir, '.gitignore'), '/receipt.json\n');
    writeFileSync(path.join(dir, 'tdf-hq/sql/synthetic.sql'), 'SELECT 1;');
    // The stand-in decodes only the data bundle, never executes transmitted Python.
    writeFileSync(path.join(dir, 'bin/ssh'), `#!${process.execPath}\n` + `
import fs from 'node:fs';
const program = fs.readFileSync(0, 'utf8');
const encoded = program.match(/candidate = json.loads\\(base64.b64decode\\('([^']+)'\\)\\)/)[1];
const candidate = JSON.parse(Buffer.from(encoded, 'base64').toString('utf8'));
const proof = { sourceRevision: candidate.sourceRevision, sqlSha256: candidate.sqlSha256,
 manifestSha256: candidate.manifestSha256, applications: 2, migrationCount: candidate.migrations.length };
if (process.env.MODE === 'wrong-sql') proof.sqlSha256 = '0'.repeat(64);
if (process.env.MODE === 'wrong-manifest') proof.manifestSha256 = '0'.repeat(64);
if (process.env.MODE === 'incomplete-ledger') proof.migrationCount = 0;
console.log(JSON.stringify({ ...${JSON.stringify(passing)}, candidateMigrations: proof }));
`, { mode: 0o700 });
    const git = (...args) => execFileSync('git', args, { cwd: dir, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
    const commit = message => git('-c', 'user.name=Test', '-c', 'user.email=test@example.invalid', '-c', 'commit.gpgsign=false', 'commit', '-qm', message);
    git('init', '-q'); git('add', '.'); commit('introduce source');
    const introducedBy = git('rev-parse', 'HEAD');
    writeFileSync(path.join(dir, 'scripts/production-migrations.json'), JSON.stringify({ schemaVersion: 1,
      migrations: [{ id: 'synthetic', path: 'tdf-hq/sql/synthetic.sql', introducedBy }] }));
    git('add', '.'); commit('register source');
    const output = path.join(dir, 'receipt.json');
    const result = spawnSync(process.execPath, [path.join(dir, files[0]), '--identity-file', path.join(dir, 'key'), '--output', output, '--with-candidate-migrations'], {
      encoding: 'utf8', env: { PATH: `${dir}/bin:${process.env.PATH}`, MODE: mode },
    });
    assert.equal(result.status, mode === 'pass' ? 0 : 1, result.stderr);
    assert.equal(existsSync(output), mode === 'pass');
    if (mode === 'pass') {
      const receipt = JSON.parse(readFileSync(output));
      assert.equal(receipt.snapshot.candidateMigrations.migrationCount, 1);
      for (const file of helpers) assert.match(receipt.sources[file], /^[a-f0-9]{64}$/);
    }
  });
}
