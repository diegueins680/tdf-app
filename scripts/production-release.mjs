#!/usr/bin/env node
import { createHash } from 'node:crypto';
import { execFile } from 'node:child_process';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { promisify } from 'node:util';
import { checkDisabledEscrowWrites } from './lib/legacy-escrow-contract.mjs';
import { loadMigrationContract } from './lib/migration-contract.mjs';

import {
  expandMigrationIncludes,
  normalizeFullSha,
  validateProductionCompose,
} from './lib/production-release.mjs';

const execFileAsync = promisify(execFile);
const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const manifestPath = path.join(rootDir, 'scripts', 'production-migrations.json');
const productionComposePath = path.join(rootDir, 'ops', 'hetzner', 'compose.production.yaml');
const securityEmergencyPreflightPath = path.join(
  rootDir,
  'tdf-hq',
  'sql',
  'preflight_security_emergency_readiness.sql',
);
const defaultImageRepo = 'diegueins680/tdf-hq';
const stagedRuntimeEnv = Object.freeze({
  RUN_MIGRATIONS: 'false',
  AUTO_APPLY_PRODUCTION_MIGRATIONS: 'true',
  CONTEXTUAL_REPUTATION_ENABLED: 'false',
  REPUTATION_AGGREGATION_WORKER_ENABLED: 'false',
  REPUTATION_AGGREGATION_ENVIRONMENT: 'production',
  REPUTATION_AGGREGATION_MODE: 'simulation',
  EVENT_DISCOVERY_ENABLED: 'false',
  EVENT_DISCOVERY_AUTO_PUBLISH: 'false',
  SINGLE_FEATURE_ONBOARDING_EXPERIMENT_ENABLED: 'false',
  DEFAULT_LOCALE: 'es',
});
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

export async function resolveReleaseContext(options) {
  const sha = normalizeFullSha(options.sha);
  const image = String(options.image ?? `${defaultImageRepo}:${sha}`).trim();
  if (!image.endsWith(`:${sha}`)) {
    throw new Error('Release image must use the exact full commit SHA tag.');
  }
  await run(['git', 'cat-file', '-e', `${sha}^{commit}`], { log: false });
  const manifestRelativePath = path.relative(rootDir, manifestPath);
  const manifest = JSON.parse(await readGitBlob(sha, manifestRelativePath));
  const { migrations } = await loadMigrationContract(sha, readGitBlob, gitIsAncestor);

  if (!await disabledEscrowWritesAt(sha)) {
    throw new Error('Release source must preserve the disabled legacy escrow write contract.');
  }

  let recoverySha;
  if (options.recoverySha) {
    recoverySha = normalizeFullSha(options.recoverySha);
    if (recoverySha === sha || !await gitIsAncestor(recoverySha, sha)) {
      throw new Error('Recovery must be a distinct reviewed ancestor of the release.');
    }
    const recoveryManifest = JSON.parse(await readGitBlob(recoverySha, manifestRelativePath));
    if (JSON.stringify(recoveryManifest) !== JSON.stringify(manifest)) {
      throw new Error('Recovery and release must have identical migration manifests.');
    }
    for (const migration of migrations) {
      const content = await expandMigrationIncludes(
        { path: migration.path, content: await readGitBlob(recoverySha, migration.path) },
        (includedPath) => readGitBlob(recoverySha, includedPath),
      );
      if (createHash('sha256').update(content).digest('hex') !== migration.checksum) {
        throw new Error(`Recovery migration differs: ${migration.id}`);
      }
    }
    if (!(await rollbackCompatibility({ sha, migrations }, recoverySha)).compatible) {
      throw new Error('Recovery source does not preserve the required authentication and financial-write contracts.');
    }
  }

  const productionCompose = await readGitBlob(sha, path.relative(rootDir, productionComposePath));
  const securityEmergencyPreflightSql = await readGitBlob(
    sha,
    path.relative(rootDir, securityEmergencyPreflightPath),
  );
  validateProductionCompose(productionCompose);
  return {
    ...options,
    sha,
    image,
    migrations,
    recoverySha,
    securityEmergencyPreflightSql,
  };
}

export function runtimeEnvBlockers(rows, options = {}) {
  const contextualReputationEnabled = options.contextualReputationEnabled;
  if (contextualReputationEnabled !== undefined
      && typeof contextualReputationEnabled !== 'boolean') {
    throw new Error('contextualReputationEnabled must be a boolean.');
  }
  const { eventDiscoveryEnabled, eventDiscoveryAutoPublish } = options;
  if (eventDiscoveryEnabled !== undefined && typeof eventDiscoveryEnabled !== 'boolean') {
    throw new Error('eventDiscoveryEnabled must be a boolean.');
  }
  if (eventDiscoveryAutoPublish !== undefined && typeof eventDiscoveryAutoPublish !== 'boolean') {
    throw new Error('eventDiscoveryAutoPublish must be a boolean.');
  }
  const expectedRuntimeEnv = {
    ...stagedRuntimeEnv,
    ...(eventDiscoveryEnabled === undefined ? {} : { EVENT_DISCOVERY_ENABLED: String(eventDiscoveryEnabled) }),
    ...(eventDiscoveryAutoPublish === undefined ? {} : { EVENT_DISCOVERY_AUTO_PUBLISH: String(eventDiscoveryAutoPublish) }),
    ...(contextualReputationEnabled === undefined
      ? {}
      : { CONTEXTUAL_REPUTATION_ENABLED: String(contextualReputationEnabled) }),
  };
  return rows.flatMap(({ machineId, values }) => Object.entries(expectedRuntimeEnv)
    .filter(([name]) => !(
      options.allowUnavailableAutomaticRunner === true
      && name === 'AUTO_APPLY_PRODUCTION_MIGRATIONS'
      && [undefined, '__UNSET__', 'false'].includes(values[name])
    ) && !(
      options.allowUnavailableReputationWorker === true
      && name.startsWith('REPUTATION_AGGREGATION_')
      && [undefined, '__UNSET__'].includes(values[name])
    ))
    .filter(([name, expected]) => values[name] !== expected)
    .map(([name, expected]) => {
      const actual = values[name] === '__UNSET__' || values[name] === undefined
        ? 'unset'
        : JSON.stringify(values[name]);
      return `Machine ${machineId} effective ${name} is ${actual}; expected ${expected}.`;
    }));
}

export function captureEventDiscoveryGates(rows) {
  const capture = (name) => {
    const values = new Set(rows.map(({ values: runtime }) => runtime[name]));
    if (values.size !== 1 || !['true', 'false'].includes([...values][0])) {
      throw new Error(`Every production Machine must report the same boolean ${name} value.`);
    }
    return [...values][0] === 'true';
  };
  return {
    eventDiscoveryEnabled: capture('EVENT_DISCOVERY_ENABLED'),
    eventDiscoveryAutoPublish: capture('EVENT_DISCOVERY_AUTO_PUBLISH'),
  };
}

export function captureContextualReputationGate(rows) {
  const values = new Set(rows.map(({ values: runtime }) => runtime.CONTEXTUAL_REPUTATION_ENABLED));
  if (values.size !== 1 || !['true', 'false'].includes([...values][0])) {
    throw new Error(
      'Every production Machine must report the same boolean CONTEXTUAL_REPUTATION_ENABLED value.',
    );
  }
  return [...values][0] === 'true';
}

export async function verifyImageExists(image, sha) {
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
  if (imageEnv.AUTO_APPLY_PRODUCTION_MIGRATIONS !== 'true') {
    throw new Error(`Image ${image} does not enable the reviewed production migration runner.`);
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

// This reviewed commit binds Google authentication to issuer/subject and requires
// deliberate new-account intent. Restoring an earlier binary would re-enable
// email-only authority even if the additive table itself remains intact.
const providerIdentityFloor = 'c53b33e7ef868fb7b64f876199ed66be0f617efc';
// Older binaries ignore these receipts and can repeat contact creation or grant
// access through shared email. Recovery must retain the writer contract even
// when the receipt tables happened to be empty during preflight.
const intakeIdentityFloor = '02115f7d1b0786f3cdd4287a9466dd22682f603b';
const sourceRequestIdentityFloor = '6eab8592744015124b0162ce9e9361f51a04f538';
// SYS-ESCROW-003: inspect the exact candidate blob, not a commit marker that
// can disappear after a squash/cherry-pick or survive a later reintroduction.
export async function disabledEscrowWritesAt(sha, readBlob = readGitBlob) {
  const candidate = normalizeFullSha(sha);
  const source = await readBlob(candidate, 'tdf-hq/src/TDF/Server.hs');
  const apiSource = await readBlob(candidate, 'tdf-hq/src/TDF/API.hs');
  try {
    checkDisabledEscrowWrites(source, apiSource);
    return true;
  } catch {
    return false; // Source disagrees. Git/tool failures above must still propagate.
  }
}

// ID-SESSION-003: earlier binaries ignore bound recovery expiry metadata.
const recoveryExpiryFloor = '57315f2d4b3aafac8ec7242c5913028f157dff8d';
// Session expiry: earlier binaries ignore api_token validity windows and would
// honour interactive sessions indefinitely again.
const sessionExpiryFloor = '8187065f81fad94677c8ba27760ab2cb74f58f79';
export function requiredIdentityCommit(context) {
  const ids = new Set(context.migrations.map(({ id }) => id));
  if (ids.has('2026-10-31_api_token_session_expiry')) return sessionExpiryFloor;
  if (ids.has('2026-10-04_auth_recovery_expiry')) return recoveryExpiryFloor;
  if (ids.has('2026-09-18_course_identity_requests')
      || ids.has('2026-09-18_trial_identity_requests')
      || ids.has('2026-09-18_ads_identity_requests')) return sourceRequestIdentityFloor;
  if (ids.has('2026-09-18_live_intake_idempotency')) return intakeIdentityFloor;
  return ids.has('2026-09-18_provider_subject_identity') ? providerIdentityFloor : null;
}

async function gitIsAncestor(ancestor, descendant) {
  try {
    await run(['git', 'merge-base', '--is-ancestor', ancestor, descendant], { log: false });
    return true;
  } catch (error) {
    if (error.cause?.code === 1) return false;
    throw error; // Missing history/tooling is not evidence of compatibility.
  }
}

export async function rollbackCompatibility(context, sha, isAncestor = gitIsAncestor, readBlob = readGitBlob) {
  const candidate = normalizeFullSha(sha);
  const requiredCommit = requiredIdentityCommit(context);
  const escrowWritesDisabled = context.sha ? await disabledEscrowWritesAt(candidate, readBlob) : null;
  return {
    sha: candidate,
    requiredCommit,
    escrowWritesDisabled,
    compatible: (requiredCommit === null || await isAncestor(requiredCommit, candidate))
      && escrowWritesDisabled !== false,
  };
}

export async function withCompatibleRollback(context, sha, deploy, isAncestor = gitIsAncestor, readBlob = readGitBlob) {
  const policy = await rollbackCompatibility(context, sha, isAncestor, readBlob);
  if (policy.escrowWritesDisabled === false) {
    throw new Error(`Unsafe financial-write rollback blocked: ${policy.sha} does not preserve the disabled escrow handler bodies. Recover forward with a compatible reviewed image.`);
  }
  if (!policy.compatible) {
    throw new Error(`Unsafe authentication rollback blocked: ${policy.sha} predates ${policy.requiredCommit}. Keep provider bindings and source request receipts; recover forward with a compatible reviewed image.`);
  }
  return deploy();
}

export function selectRecoveryTarget(snapshot, fallback) {
  if (snapshot.rollbackPolicy?.compatible) {
    return { sha: snapshot.sha, image: snapshot.image, acceptableImageDigests: [snapshot.imageDigest] };
  }
  if (!fallback?.sha || !fallback.image || !fallback.acceptableImageDigests?.length) {
    throw new Error('No verified compatible recovery artifact; refusing legacy rollback.');
  }
  return fallback;
}

export async function recoverReleaseMachines(originalMachines, touchedMachines, recover) {
  const rollbacks = [];
  const errors = [];
  // Before any deploy attempt there can be no new provider authority from this
  // release. Once a canary could serve traffic, every legacy replica must move
  // forward too, even if rollout never reached it. Keep attempting the rest
  // when one recovery fails; report failures instead of claiming fleet safety.
  if (touchedMachines.size === 0) return { rollbacks, errors };
  const required = [...originalMachines].reverse().filter(machine =>
    touchedMachines.has(machine.id) || machine.releaseSnapshot.rollbackPolicy?.compatible === false);
  for (const machine of required) {
    try {
      rollbacks.push(await recover(machine));
    } catch (error) {
      errors.push({ machineId: machine.id, error: error.message });
    }
  }
  return { rollbacks, errors };
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  // This module only exports the reviewed source/artifact helpers used by the
  // portable preparation lane. It has no command-line behaviour of its own.
  console.error('The release CLI is retired. Follow ops/hetzner/README.md for the current production procedure. Prepare reviewed artifacts with npm run release:backend:prepare -- FULL_RELEASE_SHA FULL_RECOVERY_SHA NEW_PRIVATE_DIRECTORY. Preparation does not deploy or authorize database changes.');
  process.exitCode = 1;
}
