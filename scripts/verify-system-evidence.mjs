import { execFileSync, spawn, spawnSync } from 'node:child_process';
import { mkdirSync, openSync, closeSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { sourceManifest, sha256, classifyExecution } from './lib/verification-evidence.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
if (process.argv.length !== 4 || process.argv[2] !== '--output') {
  throw new Error('Usage: node scripts/verify-system-evidence.mjs --output NEW_DIRECTORY');
}
const out = path.resolve(process.argv[3]);
// Refuse reuse: failed runs must not pick up evidence from an older invocation.
mkdirSync(out);
const before = sourceManifest(root);
const report = {
  schemaVersion: 1, revision: execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' }).trim(),
  startedAt: new Date().toISOString(), node: process.version,
  command: ['bash', 'scripts/verify-event-operations-formal.sh'],
  sourceDigest: before.digest, status: 'running', tools: {},
  limitations: 'Bounded model analysis and translator-integrity tests, not whole-program refinement; mobile identity recorded but mobile source not analyzed by this runner.',
};
writeFileSync(path.join(out, 'sources.json'), JSON.stringify(before, null, 2) + '\n');
const save = () => writeFileSync(path.join(out, 'result.json'), JSON.stringify(report, null, 2) + '\n');
save();
try {
  for (const name of ['TLA2TOOLS_JAR', 'ALLOY_JAR']) {
    if (!process.env[name]) throw new Error(`${name} is required`);
    report.tools[name] = sha256(readFileSync(process.env[name]));
  }
  const java = spawnSync(process.env.JAVA_BIN || 'java', ['-version'], { encoding: 'utf8' });
  if (java.error || java.status !== 0) throw new Error('Java version probe failed');
  report.tools.java = `${java.stdout}${java.stderr}`.trim();
  const fd = openSync(path.join(out, 'models.log'), 'wx');
  let outcome;
  try {
    outcome = await new Promise(resolve => {
      const child = spawn('bash', ['scripts/verify-event-operations-formal.sh'],
        { cwd: root, stdio: ['ignore', fd, fd], timeout: 18 * 60_000 });
      child.on('error', error => resolve({ status: null, error: error.message }));
      child.on('close', (status, signal) => resolve({ status, signal }));
    });
  } finally { closeSync(fd); }
  const after = sourceManifest(root);
  const log = readFileSync(path.join(out, 'models.log'), 'utf8');
  report.execution = outcome;
  report.logSha256 = sha256(log);
  report.finishedAt = new Date().toISOString();
  report.finalSourceDigest = after.digest;
  report.status = classifyExecution({ ...outcome, log, before: before.digest, after: after.digest });
  if (report.status !== 'bounded-model-checks-passed') process.exitCode = 1;
} catch (error) {
  report.status = 'execution-failed'; report.error = error.message; process.exitCode = 1;
} finally { save(); }
console.log(JSON.stringify(report, null, 2));
