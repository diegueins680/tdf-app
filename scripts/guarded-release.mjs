#!/usr/bin/env node
// Operator driver for ops/hetzner/guarded-release.py.
//
//   node scripts/guarded-release.mjs prepare --revision SHA --recipient age1... \
//        --storage-host uNNN.your-storagebox.de --storage-user uNNN-sub1 \
//        [--smoke /music/releases,/health] [--identity-file ~/.ssh/key] [--dry-run]
//   node scripts/guarded-release.mjs run backup|migrate|cutover --confirm <first 12 of SHA>
//   node scripts/guarded-release.mjs status
//   node scripts/guarded-release.mjs rollback --confirm rollback-<first 12 of SHA>
//
// Everything the host executes comes from Git blobs at one committed tool
// revision and is hash-checked on the host. Release inputs are derived, not
// typed: image digest from the registry, migrations from the candidate's
// manifest, mobile pin from its gitlink. `prepare --dry-run` derives and checks
// all of it without connecting to production.
import { createHash } from 'node:crypto';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync, chmodSync } from 'node:fs';
import { homedir, tmpdir } from 'node:os';
import path from 'node:path';

const HOST = process.env.TDF_PRODUCTION_SSH_HOST ?? 'root@178.105.93.101';
const DEFAULT_KEY = path.join(homedir(), '.ssh/tdf_hetzner_deploy_20260928');
const IMAGE_REPO = 'diegueins680/tdf-hq';
const TOOLS_ROOT = '/opt/tdf/release-tools';
export const TOOL_FILES = [
  'ops/hetzner/guarded-release.py',
  'ops/hetzner/release-journal.py',
  'ops/hetzner/recovery-files.py',
  'ops/hetzner/recovery-envelope.py',
  'ops/hetzner/physical-postgres-recovery.py',
  'ops/hetzner/rehearse-postgres-restore.py',
  'ops/hetzner/inspect-runtime.py',
  'ops/hetzner/recovery-tools.json',
];
const FULL_SHA = /^[0-9a-f]{40}$/;

export function sha256(data) {
  return createHash('sha256').update(data).digest('hex');
}

function fail(message) {
  console.error(`guarded-release: ${message}`);
  process.exit(2);
}

function run(command, args, { input, allowFailure = false, encoding = 'utf8' } = {}) {
  const result = spawnSync(command, args, { input, encoding, maxBuffer: 64 * 1024 * 1024 });
  if (result.error) fail(`${command} could not start`);
  if (result.status !== 0 && !allowFailure) {
    const detail = encoding === 'utf8' ? (result.stderr || '').trim().split('\n').slice(-1)[0] : '';
    fail(`${command} ${args[0] ?? ''} failed${detail ? `: ${detail}` : ''}`);
  }
  return result;
}

const git = (...args) => run('git', args).stdout.trim();
const gitBlob = (revision, file) => run('git', ['show', `${revision}:${file}`], { encoding: 'buffer' }).stdout;

export function parseArgs(argv) {
  const [command, ...rest] = argv;
  const options = { _: [] };
  for (let i = 0; i < rest.length; i += 1) {
    const arg = rest[i];
    if (arg === '--dry-run') options.dryRun = true;
    else if (arg.startsWith('--')) {
      const value = rest[i + 1];
      if (value === undefined || value.startsWith('--')) fail(`${arg} needs a value`);
      options[arg.slice(2)] = value;
      i += 1;
    } else options._.push(arg);
  }
  return { command, options };
}

export function migrationInputs(manifestBytes) {
  const manifest = JSON.parse(manifestBytes.toString('utf8'));
  if (manifest.schemaVersion !== 1 || !Array.isArray(manifest.migrations)) fail('unexpected migration manifest shape');
  const ids = manifest.migrations.map((entry) => entry.id);
  if (new Set(ids).size !== ids.length) fail('duplicate migration ids in manifest');
  return { manifestHash: sha256(manifestBytes), migrationIds: ids };
}

async function registryDigest(revision) {
  const auth = await fetch(`https://auth.docker.io/token?service=registry.docker.io&scope=repository:${IMAGE_REPO}:pull`);
  if (!auth.ok) fail('registry authentication failed');
  const { token } = await auth.json();
  const response = await fetch(`https://registry-1.docker.io/v2/${IMAGE_REPO}/manifests/${revision}`, {
    method: 'HEAD',
    headers: {
      Authorization: `Bearer ${token}`,
      Accept: [
        'application/vnd.oci.image.index.v1+json',
        'application/vnd.docker.distribution.manifest.list.v2+json',
        'application/vnd.oci.image.manifest.v1+json',
        'application/vnd.docker.distribution.manifest.v2+json',
      ].join(', '),
    },
  });
  if (response.status === 404) fail(`no published image tagged ${revision}; wait for the Build Image workflow`);
  const digest = response.headers.get('docker-content-digest');
  if (!response.ok || !/^sha256:[0-9a-f]{64}$/.test(digest ?? '')) fail('registry did not return an immutable digest');
  return `${IMAGE_REPO}@${digest}`;
}

async function verifiedAge(toolRevision, directory) {
  const pins = JSON.parse(gitBlob(toolRevision, 'ops/hetzner/recovery-tools.json').toString('utf8'));
  const linux = pins.platforms['linux-amd64'];
  const url = `https://github.com/FiloSottile/age/releases/download/${pins.version}/age-${pins.version}-linux-amd64.tar.gz`;
  const response = await fetch(url);
  if (!response.ok) fail('could not download the pinned age release');
  const archive = Buffer.from(await response.arrayBuffer());
  if (sha256(archive) !== linux.archiveSha256) fail('age archive checksum mismatch');
  const archivePath = path.join(directory, 'age.tar.gz');
  writeFileSync(archivePath, archive, { mode: 0o600 });
  run('tar', ['-xzf', archivePath, '-C', directory, 'age/age']);
  const binary = readFileSync(path.join(directory, 'age/age'));
  if (sha256(binary) !== linux.binaries.age) fail('age binary checksum mismatch');
  return binary;
}

function sshArgs(options) {
  const key = path.resolve((options['identity-file'] ?? DEFAULT_KEY).replace(/^~(?=\/)/, homedir()));
  return ['-i', key, '-o', 'IdentitiesOnly=yes', '-o', 'BatchMode=yes', '-o', 'StrictHostKeyChecking=yes',
    '-o', 'ConnectTimeout=15'];
}

function ssh(options, remoteCommand, input) {
  return run('ssh', [...sshArgs(options), HOST, remoteCommand], { input });
}

function toolRevision(options) {
  const revision = options['tool-revision'] ?? git('rev-parse', 'HEAD');
  if (!FULL_SHA.test(revision)) fail('tool revision must be a full commit');
  for (const file of TOOL_FILES) gitBlob(revision, file); // must exist in that commit
  return revision;
}

function confirmation(value, prefix = '') {
  if (!new RegExp(`^${prefix}[0-9a-f]{12}$`).test(value ?? '')) {
    fail(`--confirm must be ${prefix}<first 12 characters of the release revision>`);
  }
  return value;
}

function executor(options, args, input) {
  const revision = toolRevision(options);
  const result = ssh(options, ['python3', `${TOOLS_ROOT}/${revision}/guarded-release.py`, ...args].join(' '), input);
  process.stdout.write(result.stdout);
  return result.stdout;
}

async function prepare(options) {
  const revision = options.revision;
  if (!FULL_SHA.test(revision ?? '')) fail('--revision must be a full 40-character commit');
  git('cat-file', '-e', `${revision}^{commit}`);
  const tools = toolRevision(options);
  for (const required of ['recipient', 'storage-host', 'storage-user']) {
    if (!options[required]) fail(`--${required} is required`);
  }
  const { manifestHash, migrationIds } = migrationInputs(gitBlob(revision, 'scripts/production-migrations.json'));
  const mobileRevision = git('rev-parse', `${revision}:tdf-mobile`);
  const candidateImage = await registryDigest(revision);
  const params = {
    sourceRevision: revision,
    mobileRevision,
    toolRevision: tools,
    candidateImage,
    manifestHash,
    migrationIds,
    recipient: options.recipient,
    storage: {
      host: options['storage-host'],
      port: Number(options['storage-port'] ?? 23),
      user: options['storage-user'],
      directory: options['storage-directory'] ?? 'tdf-backups',
    },
    smokePaths: (options.smoke ?? '').split(',').filter(Boolean),
  };
  const staging = mkdtempSync(path.join(tmpdir(), 'tdf-release-'));
  try {
    const files = TOOL_FILES.map((file) => ({ name: path.basename(file), bytes: gitBlob(tools, file), mode: 0o600 }));
    files.push({ name: 'age', bytes: await verifiedAge(tools, staging), mode: 0o700 });
    const summary = { params, tools: Object.fromEntries(files.map((f) => [f.name, sha256(f.bytes)])) };
    console.log(JSON.stringify(summary, null, 2));
    if (options.dryRun) {
      console.log(`Dry run: derived ${migrationIds.length} migrations and image ${candidateImage}; nothing was sent.`);
      return;
    }
    const remoteDir = `${TOOLS_ROOT}/${tools}`;
    ssh(options, `install -d -m 700 ${TOOLS_ROOT} && install -d -m 700 ${remoteDir}`);
    for (const file of files) {
      const local = path.join(staging, `upload-${file.name}`);
      writeFileSync(local, file.bytes, { mode: file.mode });
      chmodSync(local, file.mode);
      run('scp', ['-q', '-p', ...sshArgs(options), local, `${HOST}:${remoteDir}/${file.name}`]);
    }
    const check = files.map((f) => `${sha256(f.bytes)}  ${remoteDir}/${f.name}`).join('\n') + '\n';
    ssh(options, `chmod 600 ${remoteDir}/*.py ${remoteDir}/*.json && chmod 700 ${remoteDir}/age && sha256sum --check --strict --quiet`, check);
    executor(options, ['plan'], JSON.stringify(params));
  } finally {
    rmSync(staging, { recursive: true, force: true });
  }
}

async function main() {
  const { command, options } = parseArgs(process.argv.slice(2));
  if (command === 'prepare') return prepare(options);
  if (command === 'status') return executor(options, ['status']);
  if (command === 'run') {
    const [phase] = options._;
    if (!['backup', 'migrate', 'cutover'].includes(phase)) fail('run needs a phase: backup, migrate or cutover');
    return executor(options, ['run', phase, '--confirm', confirmation(options.confirm)]);
  }
  if (command === 'rollback') {
    return executor(options, ['rollback', '--confirm', confirmation(options.confirm, 'rollback-')]);
  }
  fail('usage: prepare | run <phase> | status | rollback (see the header of this file)');
}

if (import.meta.url === `file://${process.argv[1]}`) {
  await main();
}
