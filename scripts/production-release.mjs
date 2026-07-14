#!/usr/bin/env node
import { createHash, randomUUID } from 'node:crypto';
import { execFile, spawn } from 'node:child_process';
import fs from 'node:fs/promises';
import net from 'node:net';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { promisify } from 'node:util';

import {
  buildDeployPlan,
  buildMigrationBatchSql,
  buildSchemaPreflightSql,
  buildSchemaVerificationSql,
  normalizeFullSha,
  validateFlyConfig,
  validateMigrationRelativePath,
  validateSafeName,
} from './lib/production-release.mjs';

const execFileAsync = promisify(execFile);
const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const manifestPath = path.join(rootDir, 'scripts', 'production-migrations.json');
const flyConfigPath = path.join(rootDir, 'fly.toml');
const defaultImageRepo = 'diegueins680/tdf-hq';
const productionProfile = Object.freeze({
  app: 'tdf-hq',
  dbApp: 'tdf-hq-db',
  database: 'tdf_hq',
  imageRepo: defaultImageRepo,
});
const stagedRuntimeEnv = Object.freeze({
  RUN_MIGRATIONS: 'false',
  EVENT_DISCOVERY_ENABLED: 'false',
});
const readRuntimeEnvCommand = [
  "sh -lc '",
  'printf "RUN_MIGRATIONS=%s\\nEVENT_DISCOVERY_ENABLED=%s\\n" ',
  '"${RUN_MIGRATIONS-__UNSET__}" ',
  '"${EVENT_DISCOVERY_ENABLED-__UNSET__}"',
  "'",
].join('');

function usage() {
  return `Usage:
  npm run release:backend:plan -- --sha <full-sha>
  npm run release:backend:preflight -- --sha <full-sha>
  npm run release:backend -- --sha <full-sha> --execute --confirm <full-sha>

Options:
  --app <name>       Fly API app (default: tdf-hq)
  --db-app <name>    Fly PostgreSQL app (default: tdf-hq-db)
  --database <name>  PostgreSQL database (default: tdf_hq)
  --image <ref>      Immutable image; defaults to diegueins680/tdf-hq:<sha>
`;
}

export function parseArgs(argv) {
  const args = [...argv];
  const mode = args[0] && !args[0].startsWith('--') ? args.shift() : 'plan';
  if (!['plan', 'preflight', 'release'].includes(mode)) {
    throw new Error(`Unknown release mode: ${mode}`);
  }
  const options = {
    mode,
    app: 'tdf-hq',
    dbApp: 'tdf-hq-db',
    database: 'tdf_hq',
    execute: false,
  };
  while (args.length > 0) {
    const flag = args.shift();
    if (flag === '--execute') {
      options.execute = true;
      continue;
    }
    if (flag === '--help' || flag === '-h') {
      options.help = true;
      continue;
    }
    const key = {
      '--sha': 'sha',
      '--confirm': 'confirm',
      '--image': 'image',
      '--app': 'app',
      '--db-app': 'dbApp',
      '--database': 'database',
    }[flag];
    if (!key || args.length === 0) throw new Error(`Unknown or incomplete option: ${flag}`);
    options[key] = args.shift();
  }
  return options;
}

async function run(argv, options = {}) {
  const [command, ...args] = argv;
  if (options.log !== false) console.error(`$ ${argv.join(' ')}`);
  try {
    const result = await execFileAsync(command, args, {
      cwd: rootDir,
      encoding: 'utf8',
      maxBuffer: 30 * 1024 * 1024,
      ...options,
    });
    return { stdout: result.stdout ?? '', stderr: result.stderr ?? '' };
  } catch (error) {
    const detail = [error.stdout, error.stderr].filter(Boolean).join('\n').trim();
    throw new Error(`${argv.join(' ')} failed${detail ? `:\n${detail}` : ''}`, { cause: error });
  }
}

async function commandExists(command) {
  try {
    await run(['sh', '-lc', `command -v ${command}`], { log: false });
    return true;
  } catch {
    return false;
  }
}

async function readGitBlob(sha, relativePath) {
  const object = `${sha}:${relativePath}`;
  const { stdout: type } = await run(['git', 'cat-file', '-t', object], { log: false });
  if (type.trim() !== 'blob') throw new Error(`${relativePath} is not a regular file in ${sha}.`);
  const { stdout } = await run(['git', 'show', object], { log: false });
  return stdout;
}

async function resolveReleaseContext(options) {
  const sha = normalizeFullSha(options.sha);
  const app = validateSafeName(options.app, 'Fly app');
  const dbApp = validateSafeName(options.dbApp, 'Fly database app');
  const database = validateSafeName(options.database, 'PostgreSQL database');
  const image = String(options.image ?? `${defaultImageRepo}:${sha}`).trim();
  if (!image.endsWith(`:${sha}`)) {
    throw new Error('Release image must use the exact full commit SHA tag.');
  }
  if (options.mode === 'release' && (
    app !== productionProfile.app
      || dbApp !== productionProfile.dbApp
      || database !== productionProfile.database
      || !image.startsWith(`${productionProfile.imageRepo}:`)
  )) {
    throw new Error('Production execution does not allow app, database, or image-repository overrides.');
  }

  await run(['git', 'cat-file', '-e', `${sha}^{commit}`], { log: false });
  const manifestRelativePath = path.relative(rootDir, manifestPath);
  const manifest = JSON.parse(await readGitBlob(sha, manifestRelativePath));
  if (manifest.schemaVersion !== 1 || !Array.isArray(manifest.migrations)) {
    throw new Error('Unsupported production migration manifest.');
  }

  const migrations = [];
  const migrationIds = new Set();
  const migrationPaths = new Set();
  for (const entry of manifest.migrations) {
    const id = validateSafeName(entry.id, 'Migration id');
    const relativePath = validateMigrationRelativePath(entry.path);
    if (migrationIds.has(id)) throw new Error(`Duplicate production migration id: ${id}`);
    if (migrationPaths.has(relativePath)) throw new Error(`Duplicate production migration path: ${relativePath}`);
    migrationIds.add(id);
    migrationPaths.add(relativePath);
    const introducedBy = normalizeFullSha(entry.introducedBy);
    let included = true;
    try {
      await run(['git', 'merge-base', '--is-ancestor', introducedBy, sha], { log: false });
    } catch (error) {
      if (error.cause?.code === 1) included = false;
      else throw error;
    }
    if (!included) continue;
    const content = await readGitBlob(sha, relativePath);
    migrations.push({
      ...entry,
      id,
      path: relativePath,
      content,
      checksum: createHash('sha256').update(content).digest('hex'),
    });
  }
  if (migrations.length === 0) {
    throw new Error('The target commit has no registered production migrations.');
  }

  const flyConfig = await readGitBlob(sha, path.relative(rootDir, flyConfigPath));
  validateFlyConfig(flyConfig);
  return { ...options, sha, app, dbApp, database, image, flyConfig, migrations };
}

async function readMachines(app) {
  const { stdout } = await run(['flyctl', 'machines', 'list', '--app', app, '--json']);
  const machines = JSON.parse(stdout);
  if (machines.length < 2) {
    throw new Error('Production canary requires at least two started API Machines.');
  }
  const unavailable = machines.filter((machine) => machine.state !== 'started');
  if (unavailable.length > 0) {
    throw new Error(`Every production Machine must be started before release: ${unavailable.map(({ id, state }) => `${id}=${state}`).join(', ')}`);
  }
  for (const machine of machines) validateSafeName(machine.id, 'Fly Machine id');
  return machines.sort((left, right) => left.id.localeCompare(right.id));
}

async function readEffectiveRuntimeEnv(app, machines) {
  return Promise.all(machines.map(async (machine) => {
    const { stdout } = await run([
      'flyctl', 'machine', 'exec', machine.id,
      readRuntimeEnvCommand,
      '--app', app,
    ], { log: false });
    const values = Object.fromEntries(stdout.trim().split('\n').map((line) => {
      const separator = line.indexOf('=');
      if (separator < 1) throw new Error(`Machine ${machine.id} returned an invalid runtime environment payload.`);
      return [line.slice(0, separator), line.slice(separator + 1)];
    }));
    return { machineId: machine.id, values };
  }));
}

function runtimeEnvBlockers(rows) {
  return rows.flatMap(({ machineId, values }) => Object.entries(stagedRuntimeEnv)
    .filter(([name, expected]) => values[name] !== expected)
    .map(([name, expected]) => {
      const actual = values[name] === '__UNSET__' || values[name] === undefined
        ? 'unset'
        : JSON.stringify(values[name]);
      return `Machine ${machineId} effective ${name} is ${actual}; expected ${expected}.`;
    }));
}

async function readSecretNames(app) {
  const { stdout } = await run(['flyctl', 'secrets', 'list', '--app', app, '--json']);
  const rows = JSON.parse(stdout);
  return new Set(rows.map((row) => row.Name ?? row.name).filter(Boolean));
}

function remotePsqlCommand(sql, database, tuplesOnly = false) {
  const encoded = Buffer.from(sql).toString('base64');
  const psqlFlags = tuplesOnly ? '-qAt' : '';
  return [
    'sh -lc',
    `'printf %s ${encoded} | base64 -d | su postgres -c "psql -X -v ON_ERROR_STOP=1 ${psqlFlags} -p 5433 -d ${database}"'`,
  ].join(' ');
}

async function runDatabaseSql(context, sql, options = {}) {
  const remoteCommand = remotePsqlCommand(sql, context.database, options.tuplesOnly);
  return run([
    'flyctl', 'ssh', 'console',
    '--app', context.dbApp,
    '--command', remoteCommand,
  ]);
}

async function verifyImageExists(image, sha) {
  if (!(await commandExists('docker'))) throw new Error('docker CLI is required to inspect the release image.');
  const { stdout } = await run([
    'docker', 'buildx', 'imagetools', 'inspect', image,
    '--format', '{{json .}}',
  ]);
  const metadata = JSON.parse(stdout);
  const digest = metadata.manifest?.digest;
  if (!/^sha256:[0-9a-f]{64}$/i.test(digest ?? '')) {
    throw new Error(`Could not resolve an immutable digest for ${image}.`);
  }
  const imageEnv = Object.fromEntries((metadata.image?.config?.Env ?? []).map((entry) => {
    const separator = entry.indexOf('=');
    return separator < 1 ? [entry, ''] : [entry.slice(0, separator), entry.slice(separator + 1)];
  }));
  for (const name of ['SOURCE_COMMIT', 'GIT_SHA']) {
    if (imageEnv[name] !== sha) {
      throw new Error(`Image ${image} has ${name}=${JSON.stringify(imageEnv[name])}, expected ${sha}.`);
    }
  }
  if (imageEnv.RUN_MIGRATIONS !== 'false') {
    throw new Error(`Image ${image} does not default RUN_MIGRATIONS=false.`);
  }
  const acceptableDigests = new Set([digest]);
  for (const manifest of metadata.manifest?.manifests ?? []) {
    if (manifest.platform?.os === 'linux'
        && manifest.platform?.architecture === 'amd64'
        && /^sha256:[0-9a-f]{64}$/i.test(manifest.digest ?? '')) {
      acceptableDigests.add(manifest.digest);
    }
  }
  if (acceptableDigests.size < 2) {
    throw new Error(`Image ${image} has no linux/amd64 platform manifest.`);
  }
  return {
    resolvedImage: `${image.slice(0, image.lastIndexOf(':'))}@${digest}`,
    acceptableDigests: [...acceptableDigests],
  };
}

async function remotePreflight(context) {
  if (!(await commandExists('flyctl'))) throw new Error('flyctl is required for production preflight.');
  await run(['flyctl', 'auth', 'whoami']);
  const machines = await readMachines(context.app);
  const runtimeEnv = await readEffectiveRuntimeEnv(context.app, machines);
  const secrets = await readSecretNames(context.app);
  const blockers = runtimeEnvBlockers(runtimeEnv);
  for (const machine of machines) {
    const check = await smokeMachine(context, machine.id, null);
    machine.releaseSnapshot = {
      image: previousImage(machine),
      imageDigest: machine.image_ref?.digest,
      instanceId: machine.instance_id,
      sha: check.version.commit,
      runtimeEnv: runtimeEnv.find(({ machineId }) => machineId === machine.id)?.values,
    };
    if (!machine.releaseSnapshot.image) blockers.push(`Machine ${machine.id} has no immutable rollback image reference.`);
    if (!machine.releaseSnapshot.imageDigest) blockers.push(`Machine ${machine.id} has no rollback image digest.`);
    if (!machine.releaseSnapshot.instanceId) blockers.push(`Machine ${machine.id} has no instance snapshot.`);
    if (!previousSha(machine)) blockers.push(`Machine ${machine.id} does not report a full rollback source commit.`);
  }
  for (const required of ['STRIPE_SECRET_KEY', 'STRIPE_WEBHOOK_SECRET']) {
    if (!secrets.has(required)) blockers.push(`Required Fly secret is missing: ${required}`);
  }
  const { stdout } = await runDatabaseSql(context, buildSchemaPreflightSql());
  const health = await waitForJson(`https://${context.app}.fly.dev/health`);
  const version = await waitForJson(`https://${context.app}.fly.dev/version`);
  if (health.status !== 'ok' || health.db !== 'ok') {
    throw new Error('Current production health payload is not ready.');
  }
  try {
    const image = await verifyImageExists(context.image, context.sha);
    context.resolvedImage = image.resolvedImage;
    context.acceptableImageDigests = new Set(image.acceptableDigests);
  } catch (error) {
    blockers.push(error.message.split('\n')[0]);
  }
  if (blockers.length > 0) {
    throw new Error(`Production preflight is blocked:\n- ${blockers.join('\n- ')}`);
  }
  return {
    machines,
    runtimeEnv,
    ticketmasterConfigured: secrets.has('TICKETMASTER_API_KEY'),
    databasePreflight: stdout.trim().split('\n').slice(-3),
    currentVersion: version.commit,
    resolvedImage: context.resolvedImage,
    acceptableImageDigests: [...context.acceptableImageDigests],
  };
}

async function reservePort() {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.once('error', reject);
    server.listen(0, '127.0.0.1', () => {
      const { port } = server.address();
      server.close((error) => error ? reject(error) : resolve(port));
    });
  });
}

async function waitForJson(url, timeoutMs = 60_000) {
  const deadline = Date.now() + timeoutMs;
  let lastError;
  while (Date.now() < deadline) {
    try {
      const response = await fetch(url, { signal: AbortSignal.timeout(3_000) });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      return await response.json();
    } catch (error) {
      lastError = error;
      await new Promise((resolve) => setTimeout(resolve, 1_000));
    }
  }
  throw new Error(`Timed out waiting for ${url}: ${lastError?.message ?? 'unknown error'}`);
}

async function smokeMachine(context, machineId, expectedSha = context.sha) {
  const port = await reservePort();
  const proxy = spawn('flyctl', [
    'proxy', `${port}:8080`, `${machineId}.vm.${context.app}.internal`,
    '--app', context.app,
    '--quiet',
  ], { cwd: rootDir, stdio: ['ignore', 'pipe', 'pipe'] });
  let proxyError = '';
  proxy.stderr.on('data', (chunk) => { proxyError += chunk.toString(); });
  try {
    const health = await waitForJson(`http://127.0.0.1:${port}/health`);
    if (health.status !== 'ok' || health.db !== 'ok') {
      throw new Error(`Machine ${machineId} health payload is not ready.`);
    }
    const version = await waitForJson(`http://127.0.0.1:${port}/version`);
    let runningSha;
    try {
      runningSha = normalizeFullSha(version.commit);
    } catch {
      throw new Error(`Machine ${machineId} reports an invalid commit: ${JSON.stringify(version.commit)}.`);
    }
    if (expectedSha && runningSha !== expectedSha) {
      throw new Error(`Machine ${machineId} reports ${runningSha}, expected ${expectedSha}.`);
    }
    version.commit = runningSha;
    return { machineId, health, version };
  } finally {
    proxy.kill('SIGTERM');
    await new Promise((resolve) => {
      const timer = setTimeout(resolve, 2_000);
      proxy.once('exit', () => { clearTimeout(timer); resolve(); });
    });
    if (proxy.exitCode && proxy.exitCode !== 143 && proxyError) console.error(proxyError.trim());
  }
}

function deployArgs(context, selector, machineId, image = context.image, sha = context.sha) {
  return [
    'flyctl', 'deploy', '.',
    '--app', context.app,
    '--config', 'fly.toml',
    '--image', image,
    '--env', `SOURCE_COMMIT=${sha}`,
    '--env', `GIT_SHA=${sha}`,
    '--env', 'RUN_MIGRATIONS=false',
    '--env', 'EVENT_DISCOVERY_ENABLED=false',
    '--strategy', 'rolling',
    '--max-unavailable', '1',
    '--wait-timeout', '10m',
    '--update-only',
    '--yes',
    selector, machineId,
  ];
}

function previousImage(machine) {
  if (typeof machine.image_ref === 'string') return machine.image_ref;
  const registry = machine.image_ref?.registry;
  const repository = machine.image_ref?.repository;
  if (registry && repository) {
    const base = `${registry}/${repository}`;
    if (/^sha256:[0-9a-f]{64}$/i.test(machine.image_ref?.digest ?? '')) {
      return `${base}@${machine.image_ref.digest}`;
    }
    if (machine.image_ref?.tag) return `${base}:${machine.image_ref.tag}`;
  }
  return machine.config?.image;
}

function previousSha(machine) {
  const value = machine.releaseSnapshot?.sha
    ?? machine.config?.env?.GIT_SHA
    ?? machine.config?.env?.SOURCE_COMMIT;
  try {
    return normalizeFullSha(value);
  } catch {
    return undefined;
  }
}

async function rollbackMachine(context, machine) {
  const image = machine.releaseSnapshot?.image ?? previousImage(machine);
  const sha = previousSha(machine);
  if (!image || !sha) throw new Error(`Cannot construct rollback for Machine ${machine.id}.`);
  await run([
    'flyctl', 'machine', 'update', machine.id,
    '--app', context.app,
    '--image', image,
    '--env', `SOURCE_COMMIT=${sha}`,
    '--env', `GIT_SHA=${sha}`,
    '--env', 'RUN_MIGRATIONS=false',
    '--env', 'EVENT_DISCOVERY_ENABLED=false',
    '--yes',
  ]);
  const restored = (await readMachines(context.app)).find(({ id }) => id === machine.id);
  if (!restored) throw new Error(`Rolled-back Machine ${machine.id} disappeared.`);
  if (restored.image_ref?.digest !== machine.releaseSnapshot?.imageDigest) {
    throw new Error(`Machine ${machine.id} rollback digest does not match its snapshot.`);
  }
  const envBlockers = runtimeEnvBlockers(await readEffectiveRuntimeEnv(context.app, [restored]));
  if (envBlockers.length > 0) {
    throw new Error(`Machine ${machine.id} rollback environment is unsafe: ${envBlockers.join(' ')}`);
  }
  const smoke = await smokeMachine(context, machine.id, sha);
  return { machineId: machine.id, image, imageDigest: restored.image_ref?.digest, version: smoke.version };
}

function assertExactMachineSet(current, originalMachines) {
  const expectedIds = new Set(originalMachines.map((machine) => machine.id));
  const currentIds = new Set(current.map((machine) => machine.id));
  const missing = [...expectedIds].filter((id) => !currentIds.has(id));
  const unexpected = [...currentIds].filter((id) => !expectedIds.has(id));
  if (missing.length > 0 || unexpected.length > 0) {
    throw new Error(`Production Machine set changed (missing: ${missing.join(', ') || 'none'}; unexpected: ${unexpected.join(', ') || 'none'}).`);
  }
}

async function assertUntouchedSnapshots(context, originalMachines, touchedMachines) {
  const current = await readMachines(context.app);
  assertExactMachineSet(current, originalMachines);
  for (const original of originalMachines) {
    if (touchedMachines.has(original.id)) continue;
    const latest = current.find(({ id }) => id === original.id);
    if (latest.instance_id !== original.releaseSnapshot?.instanceId
        || latest.image_ref?.digest !== original.releaseSnapshot?.imageDigest) {
      throw new Error(`Untouched Machine ${original.id} changed after preflight; refusing concurrent rollout.`);
    }
  }
  return current;
}

async function verifyTargetMachine(context, machineId) {
  const machine = (await readMachines(context.app)).find(({ id }) => id === machineId);
  if (!machine) throw new Error(`Target Machine ${machineId} disappeared.`);
  if (!context.acceptableImageDigests.has(machine.image_ref?.digest)) {
    throw new Error(`Machine ${machineId} is running unexpected image digest ${machine.image_ref?.digest}.`);
  }
  const envBlockers = runtimeEnvBlockers(await readEffectiveRuntimeEnv(context.app, [machine]));
  if (envBlockers.length > 0) {
    throw new Error(`Machine ${machineId} runtime environment is unsafe: ${envBlockers.join(' ')}`);
  }
  return smokeMachine(context, machineId);
}

async function verifyFleet(context, originalMachines) {
  const current = await readMachines(context.app);
  assertExactMachineSet(current, originalMachines);
  const envBlockers = runtimeEnvBlockers(await readEffectiveRuntimeEnv(context.app, current));
  if (envBlockers.length > 0) {
    throw new Error(`Fleet runtime environment is unsafe:\n- ${envBlockers.join('\n- ')}`);
  }
  for (const machine of current) {
    if (!context.acceptableImageDigests.has(machine.image_ref?.digest)) {
      throw new Error(`Machine ${machine.id} has unexpected image digest ${machine.image_ref?.digest}.`);
    }
    await smokeMachine(context, machine.id);
  }
  const health = await waitForJson(`https://${context.app}.fly.dev/health`);
  const version = await waitForJson(`https://${context.app}.fly.dev/version`);
  if (health.status !== 'ok' || health.db !== 'ok' || version.commit !== context.sha) {
    throw new Error('Public fleet verification did not match the target release.');
  }
  return { machines: current.map((machine) => machine.id), health, version };
}

function sqlLiteral(value) {
  return `'${String(value).replaceAll("'", "''")}'`;
}

async function acquireReleaseLease(context, token) {
  const sql = `
\\set ON_ERROR_STOP on
CREATE TABLE IF NOT EXISTS public.tdf_release_lease (
  singleton BOOLEAN PRIMARY KEY DEFAULT TRUE CHECK (singleton),
  source_commit TEXT NOT NULL CHECK (source_commit ~ '^[0-9a-f]{40}$'),
  owner_token UUID NOT NULL,
  acquired_at TIMESTAMPTZ NOT NULL,
  heartbeat_at TIMESTAMPTZ NOT NULL
);
WITH acquired AS (
  INSERT INTO public.tdf_release_lease (
    singleton, source_commit, owner_token, acquired_at, heartbeat_at
  ) VALUES (
    TRUE, ${sqlLiteral(context.sha)}, ${sqlLiteral(token)}::UUID, NOW(), NOW()
  )
  ON CONFLICT (singleton) DO UPDATE
    SET source_commit = EXCLUDED.source_commit,
        owner_token = EXCLUDED.owner_token,
        acquired_at = EXCLUDED.acquired_at,
        heartbeat_at = EXCLUDED.heartbeat_at
    WHERE public.tdf_release_lease.heartbeat_at < NOW() - INTERVAL '2 hours'
  RETURNING owner_token
)
SELECT owner_token FROM acquired;
`.trim();
  const { stdout } = await runDatabaseSql(context, sql, { tuplesOnly: true });
  if (stdout.trim() !== token) {
    throw new Error('Another production release holds the database release lease.');
  }
}

async function heartbeatReleaseLease(context, token) {
  const sql = `
WITH refreshed AS (
  UPDATE public.tdf_release_lease
  SET heartbeat_at = NOW()
  WHERE singleton AND owner_token = ${sqlLiteral(token)}::UUID
  RETURNING 1
)
SELECT COUNT(*) FROM refreshed;
`.trim();
  const { stdout } = await runDatabaseSql(context, sql, { tuplesOnly: true });
  if (stdout.trim() !== '1') throw new Error('Production release lease was lost.');
}

async function releaseReleaseLease(context, token) {
  const sql = `
WITH released AS (
  DELETE FROM public.tdf_release_lease
  WHERE singleton AND owner_token = ${sqlLiteral(token)}::UUID
  RETURNING 1
)
SELECT COUNT(*) FROM released;
`.trim();
  const { stdout } = await runDatabaseSql(context, sql, { tuplesOnly: true });
  if (stdout.trim() !== '1') throw new Error('Production release lease could not be released cleanly.');
}

async function ensurePlainDirectory(directory) {
  try {
    const current = await fs.lstat(directory);
    if (!current.isDirectory() || current.isSymbolicLink()) {
      throw new Error(`Release report directory is not a plain directory: ${directory}`);
    }
  } catch (error) {
    if (error.code !== 'ENOENT') throw error;
    await fs.mkdir(directory);
    const created = await fs.lstat(directory);
    if (!created.isDirectory() || created.isSymbolicLink()) {
      throw new Error(`Release report directory is not a plain directory: ${directory}`);
    }
  }
}

async function writeReport(context, report) {
  const artifacts = path.join(rootDir, 'artifacts');
  const releases = path.join(artifacts, 'releases');
  await ensurePlainDirectory(artifacts);
  await ensurePlainDirectory(releases);
  const timestamp = new Date().toISOString().replaceAll(':', '-');
  const relative = path.join(
    'artifacts', 'releases',
    `production-release-${context.sha}-${timestamp}-${randomUUID()}.json`,
  );
  const absolute = path.join(rootDir, relative);
  await fs.writeFile(absolute, `${JSON.stringify(report, null, 2)}\n`, { flag: 'wx', mode: 0o600 });
  return path.relative(rootDir, absolute);
}

async function executeRelease(context) {
  if (!context.execute || normalizeFullSha(context.confirm) !== context.sha) {
    throw new Error('Mutating release requires --execute and --confirm <exact-full-sha>.');
  }
  const { stdout: head } = await run(['git', 'rev-parse', 'HEAD'], { log: false });
  if (normalizeFullSha(head) !== context.sha) throw new Error('Release SHA must equal the checked-out HEAD.');
  const { stdout: status } = await run(['git', 'status', '--porcelain'], { log: false });
  if (status.trim()) throw new Error('Release execution requires a clean worktree.');

  const startedAt = new Date().toISOString();
  const preflight = await remotePreflight(context);
  const originalMachines = preflight.machines;
  const canary = originalMachines[0];
  const remaining = originalMachines.slice(1);
  const report = {
    status: 'started',
    startedAt,
    sha: context.sha,
    image: context.image,
    migrations: context.migrations.map(({ id, path: migrationPath, checksum }) => ({ id, path: migrationPath, checksum })),
    canaryMachine: canary.id,
    remainingMachines: remaining.map((machine) => machine.id),
    rollbacks: [],
    rollout: [],
  };
  const touchedMachines = new Set();
  const leaseToken = randomUUID();
  let leaseHeld = false;
  let deploymentComplete = false;

  try {
    report.resolvedImage = context.resolvedImage;
    report.acceptableImageDigests = [...context.acceptableImageDigests];
    await acquireReleaseLease(context, leaseToken);
    leaseHeld = true;
    await runDatabaseSql(
      context,
      buildMigrationBatchSql(context.migrations, { sourceCommit: context.sha }),
    );
    await runDatabaseSql(context, buildSchemaVerificationSql());
    await heartbeatReleaseLease(context, leaseToken);

    await assertUntouchedSnapshots(context, originalMachines, touchedMachines);
    touchedMachines.add(canary.id);
    await run(deployArgs(context, '--only-machines', canary.id, context.resolvedImage));
    try {
      report.canary = await verifyTargetMachine(context, canary.id);
    } catch (error) {
      report.rollbacks.push(await rollbackMachine(context, canary));
      touchedMachines.delete(canary.id);
      throw new Error(`Canary verification failed and was rolled back: ${error.message}`, { cause: error });
    }

    for (const machine of remaining) {
      await heartbeatReleaseLease(context, leaseToken);
      await assertUntouchedSnapshots(context, originalMachines, touchedMachines);
      touchedMachines.add(machine.id);
      await run(deployArgs(context, '--only-machines', machine.id, context.resolvedImage));
      report.rollout.push(await verifyTargetMachine(context, machine.id));
    }
    report.fleet = await verifyFleet(context, originalMachines);
    deploymentComplete = true;
    report.status = 'complete';
    report.finishedAt = new Date().toISOString();
    try {
      await releaseReleaseLease(context, leaseToken);
      leaseHeld = false;
    } catch (error) {
      report.status = 'complete_with_warning';
      report.leaseReleaseError = error.message;
    }
    report.reportPath = await writeReport(context, report);
    return report;
  } catch (error) {
    report.status = 'failed';
    report.error = error.message;
    report.finishedAt = new Date().toISOString();
    if (deploymentComplete) {
      report.status = 'complete_with_warning';
      report.reportPath = await writeReport(context, report);
      return report;
    }
    const rollbackErrors = [];
    for (const machine of [...originalMachines].reverse().filter(({ id }) => touchedMachines.has(id))) {
      try {
        report.rollbacks.push(await rollbackMachine(context, machine));
      } catch (rollbackError) {
        rollbackErrors.push({ machineId: machine.id, error: rollbackError.message });
      }
    }
    report.rollbackErrors = rollbackErrors;
    if (leaseHeld) {
      try {
        await releaseReleaseLease(context, leaseToken);
        leaseHeld = false;
      } catch (leaseError) {
        report.leaseReleaseError = leaseError.message;
      }
    }
    report.reportPath = await writeReport(context, report);
    throw error;
  }
}

async function main() {
  const options = parseArgs(process.argv.slice(2));
  if (options.help) {
    console.log(usage());
    return;
  }
  if (!options.sha) throw new Error(`--sha is required.\n\n${usage()}`);
  const context = await resolveReleaseContext(options);

  if (context.mode === 'plan') {
    const plan = buildDeployPlan({
      app: context.app,
      sha: context.sha,
      image: context.image,
      migrations: context.migrations.map((migration) => migration.path),
      flyConfig: context.flyConfig,
      dryRun: true,
    });
    console.log(JSON.stringify(plan, null, 2));
    return;
  }

  if (context.mode === 'preflight') {
    const preflight = await remotePreflight(context);
    console.log(JSON.stringify({
      status: 'ready',
      sha: context.sha,
      image: context.image,
      migrations: context.migrations.map(({ id, path: migrationPath, checksum }) => ({ id, path: migrationPath, checksum })),
      machines: preflight.machines.map(({ id, region, state }) => ({ id, region, state })),
      runtimeEnv: preflight.runtimeEnv,
      ticketmasterConfigured: preflight.ticketmasterConfigured,
      currentVersion: preflight.currentVersion,
      resolvedImage: preflight.resolvedImage,
    }, null, 2));
    return;
  }

  const result = await executeRelease(context);
  console.log(JSON.stringify(result, null, 2));
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  main().catch((error) => {
    console.error(error.message);
    process.exit(1);
  });
}
