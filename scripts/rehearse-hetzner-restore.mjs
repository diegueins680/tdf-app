import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync, existsSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const withMigrations = process.argv.length === 7 && process.argv[6] === '--with-candidate-migrations';
if ((!withMigrations && process.argv.length !== 6) || process.argv[2] !== '--identity-file' || process.argv[4] !== '--output') {
  throw new Error('Usage: rehearse-hetzner-restore.mjs --identity-file PATH --output NEW_JSON [--with-candidate-migrations]');
}
const identity = path.resolve(process.argv[3]), output = path.resolve(process.argv[5]);
const files = ['ops/hetzner/inspect-runtime.py', 'ops/hetzner/rehearse-postgres-restore.py', 'scripts/rehearse-hetzner-restore.mjs'];
if (withMigrations) files.push('scripts/lib/migration-contract.mjs', 'scripts/lib/production-release.mjs',
  'scripts/lib/hetzner-migration-rehearsal.mjs');
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
  let candidate = null;
  if (withMigrations) {
    const { loadMigrationContract } = await import('./lib/migration-contract.mjs');
    const { migrationRehearsalBundle } = await import('./lib/hetzner-migration-rehearsal.mjs');
    const git = (...args) => execFileSync('git', args, { cwd: root, encoding: 'utf8', maxBuffer: 16 * 1024 * 1024, stdio: ['ignore', 'pipe', 'pipe'] });
    const contract = await loadMigrationContract(before.revision, (sha, file) => git('show', `${sha}:${file}`),
      (ancestor, head) => { try { git('merge-base', '--is-ancestor', ancestor, head); return true; } catch (error) { if (error.status === 1) return false; throw error; } });
    candidate = migrationRehearsalBundle(contract);
  }
  const bundle = Buffer.from(JSON.stringify(candidate)).toString('base64');
  const program = 'import base64, types, json, sys, os, signal\nos.umask(0o077)\n' +
    'def interrupted(*_):\n    raise RuntimeError("Rehearsal interrupted")\nsignal.signal(signal.SIGTERM, interrupted)\n' +
    load('runtime', files[0]) + load('restore', files[1]) +
    `candidate = json.loads(base64.b64decode('${bundle}'))\n` +
    'try:\n    print(json.dumps(restore.rehearse(runtime, candidate)))\nexcept Exception:\n    print("Isolated restore rehearsal failed; inspect root-private host evidence. No runtime data emitted.", file=sys.stderr)\n    sys.exit(1)\n';
  const startedAt = new Date().toISOString();
  const raw = execFileSync('ssh', ['-F', '/dev/null', '-o', 'BatchMode=yes', '-o', 'IdentitiesOnly=yes',
    '-o', 'StrictHostKeyChecking=yes', '-o', 'ConnectTimeout=10', '-i', identity,
    'root@178.105.93.101', 'timeout', '--signal=TERM', '--kill-after=90', '540', 'python3', '-'], {
    input: program, timeout: 660_000, maxBuffer: 4 * 1024 * 1024, stdio: ['pipe', 'pipe', 'pipe'],
  });
  if (JSON.stringify(before) !== JSON.stringify(provenance())) throw new Error('Source changed during rehearsal');
  const snapshot = JSON.parse(raw);
  if (snapshot.status !== 'isolated-database-restore-passed' || snapshot.isolateRemoved !== true || snapshot.productionDatabaseWritten !== false) throw new Error('Incomplete rehearsal');
  if (withMigrations && (snapshot.candidateMigrations?.sourceRevision !== before.revision || snapshot.candidateMigrations?.sqlSha256 !== candidate.sqlSha256 || snapshot.candidateMigrations?.applications !== 2 || snapshot.candidateMigrations?.manifestSha256 !== candidate.manifestSha256 || snapshot.candidateMigrations?.migrationCount !== candidate.migrations.length)) throw new Error('Incomplete migration rehearsal');
  const receipt = { startedAt, completedAt: new Date().toISOString(), ...before, transmittedSha256: digest(program), snapshot };
  writeFileSync(output, JSON.stringify(receipt, null, 2) + '\n', { flag: 'wx', mode: 0o600 });
  console.log(JSON.stringify({ receipt: output, status: snapshot.status, tables: Object.keys(snapshot.tableCounts).length,
    migrations: snapshot.migrationCount, isolateRemoved: true, productionDatabaseWritten: false }));
} catch {
  console.error('Hetzner restore rehearsal failed or source/output boundary rejected. No raw remote output emitted.');
  process.exitCode = 1;
}
