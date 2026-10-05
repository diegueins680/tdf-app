import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

// Intentionally read-only: this is one prerequisite for a future guarded release,
// not a replacement deployment executor or authorization to change runtime flags.
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
if (process.argv.length !== 6 || process.argv[2] !== '--identity-file' || process.argv[4] !== '--output') {
  throw new Error('Usage: node scripts/inspect-hetzner-production.mjs --identity-file PATH --output NEW_JSON');
}
const identity = path.resolve(process.argv[3]);
const output = path.resolve(process.argv[5]);
const inspector = readFileSync(path.join(root, 'ops/hetzner/inspect-runtime.py'));
const digest = bytes => createHash('sha256').update(bytes).digest('hex');
function provenance() {
  return {
    sourceRevision: execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' }).trim(),
    sourceWorktreeDirty: execFileSync('git', ['status', '--porcelain'], { cwd: root, encoding: 'utf8' }).trim() !== '',
    inspectorSha256: digest(readFileSync(path.join(root, 'ops/hetzner/inspect-runtime.py'))),
    launcherSha256: digest(readFileSync(fileURLToPath(import.meta.url))),
  };
}
try {
  const before = provenance();
  if (before.inspectorSha256 !== digest(inspector)) throw new Error('Inspection source changed before transmission');
  const startedAt = new Date().toISOString();
  const raw = execFileSync('ssh', ['-F', '/dev/null', '-o', 'BatchMode=yes', '-o', 'IdentitiesOnly=yes',
    '-o', 'StrictHostKeyChecking=yes', '-o', 'ConnectTimeout=10', '-i', identity,
    'root@178.105.93.101', 'python3', '-'], {
    input: inspector, timeout: 180_000, maxBuffer: 4 * 1024 * 1024, stdio: ['pipe', 'pipe', 'pipe'],
  });
  const snapshot = JSON.parse(raw);
  if (JSON.stringify(before) !== JSON.stringify(provenance())) throw new Error('Inspection source changed during execution');
  const receipt = { startedAt, observedAt: new Date().toISOString(), ...before, snapshot };
  writeFileSync(output, JSON.stringify(receipt, null, 2) + '\n', { flag: 'wx', mode: 0o600 });
  console.log(JSON.stringify({ receipt: output, backend: snapshot.publicBackend.commit,
    migrationCount: snapshot.database.migrations.length, readOnly: true }));
} catch {
  console.error('Read-only Hetzner inspection failed or output already exists. No raw runtime output printed.');
  process.exitCode = 1;
}
