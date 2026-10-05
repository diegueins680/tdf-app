import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync, existsSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
if (process.argv.length !== 6 || process.argv[2] !== '--identity-file' || process.argv[4] !== '--output') {
  throw new Error('Usage: rehearse-hetzner-restore.mjs --identity-file PATH --output NEW_JSON');
}
const identity = path.resolve(process.argv[3]), output = path.resolve(process.argv[5]);
const files = ['ops/hetzner/inspect-runtime.py', 'ops/hetzner/rehearse-postgres-restore.py', 'scripts/rehearse-hetzner-restore.mjs'];
const digest = value => createHash('sha256').update(value).digest('hex');
const provenance = () => ({
  revision: execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' }).trim(),
  dirty: execFileSync('git', ['status', '--porcelain'], { cwd: root, encoding: 'utf8' }).trim() !== '',
  sources: Object.fromEntries(files.map(file => [file, digest(readFileSync(path.join(root, file)))])),
});
try {
  if (existsSync(output)) throw new Error('Output exists');
  const before = provenance();
  if (before.dirty) throw new Error('Rehearsal requires a clean source revision');
  const load = (name, file) => {
    const bytes = readFileSync(path.join(root, file));
    if (digest(bytes) !== before.sources[file]) throw new Error('Source changed');
    return `${name} = types.ModuleType('${name}')\nexec(compile(base64.b64decode('${bytes.toString('base64')}'), '${file}', 'exec'), ${name}.__dict__)\n`;
  };
  const program = 'import base64, types, json, sys, os, signal\nos.umask(0o077)\n' +
    'def interrupted(*_):\n    raise RuntimeError("Rehearsal interrupted")\nsignal.signal(signal.SIGTERM, interrupted)\n' +
    load('runtime', files[0]) + load('restore', files[1]) +
    'try:\n    print(json.dumps(restore.rehearse(runtime)))\nexcept Exception:\n    print("Isolated restore rehearsal failed; inspect root-private host evidence. No runtime data emitted.", file=sys.stderr)\n    sys.exit(1)\n';
  const startedAt = new Date().toISOString();
  const raw = execFileSync('ssh', ['-F', '/dev/null', '-o', 'BatchMode=yes', '-o', 'IdentitiesOnly=yes',
    '-o', 'StrictHostKeyChecking=yes', '-o', 'ConnectTimeout=10', '-i', identity,
    'root@178.105.93.101', 'timeout', '--signal=TERM', '--kill-after=90', '540', 'python3', '-'], {
    input: program, timeout: 660_000, maxBuffer: 4 * 1024 * 1024, stdio: ['pipe', 'pipe', 'pipe'],
  });
  if (JSON.stringify(before) !== JSON.stringify(provenance())) throw new Error('Source changed during rehearsal');
  const snapshot = JSON.parse(raw);
  if (snapshot.status !== 'isolated-database-restore-passed' || snapshot.isolateRemoved !== true || snapshot.productionDatabaseWritten !== false) throw new Error('Incomplete rehearsal');
  const receipt = { startedAt, completedAt: new Date().toISOString(), ...before, transmittedSha256: digest(program), snapshot };
  writeFileSync(output, JSON.stringify(receipt, null, 2) + '\n', { flag: 'wx', mode: 0o600 });
  console.log(JSON.stringify({ receipt: output, status: snapshot.status, tables: Object.keys(snapshot.tableCounts).length,
    migrations: snapshot.migrationCount, isolateRemoved: true, productionDatabaseWritten: false }));
} catch {
  console.error('Hetzner restore rehearsal failed or source/output boundary rejected. No raw remote output emitted.');
  process.exitCode = 1;
}
