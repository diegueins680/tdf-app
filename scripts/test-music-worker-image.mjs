#!/usr/bin/env node
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { setTimeout as pause } from 'node:timers/promises';

// This runner never builds, pulls, pushes, reads dotenv, or contacts a database.
// Resolve the local tag once; every container uses the same immutable image ID.
const reference = process.argv[2] ?? 'tdf-music-worker:local-verification';
assert(!reference.startsWith('-') && !/\s/.test(reference), 'Expected a local image reference');
const runId = randomUUID();
const names = new Set();
const label = `tdf.music-image-test=${runId}`;
const interrupted = new AbortController();
let cleaning = false;
const onInterrupt = () => interrupted.abort(new Error('Image verification interrupted'));
process.once('SIGINT', onInterrupt);
process.once('SIGTERM', onInterrupt);
const probe = fileURLToPath(new URL('./lib/music-worker-image-smoke.sh', import.meta.url));
const limits = ['--pull=never', '--network=none', '--read-only', '--cap-drop=ALL',
  '--security-opt=no-new-privileges', '--memory=1g', '--memory-swap=1g', '--cpus=2',
  '--pids-limit=128', '--tmpfs=/tmp:rw,nosuid,nodev,size=256m,mode=1777'];

async function docker(args, { timeout = 60000, accept = [0] } = {}) {
  if (!cleaning) interrupted.signal.throwIfAborted();
  return await new Promise((resolve, reject) => {
    const child = spawn('docker', args, { stdio: ['ignore', 'pipe', 'pipe'],
      signal: cleaning ? undefined : interrupted.signal });
    let output = '';
    let expired = false;
    const timer = setTimeout(() => { expired = true; child.kill('SIGKILL'); }, timeout);
    for (const stream of [child.stdout, child.stderr]) {
      stream.on('data', chunk => { output = (output + chunk).slice(-65536); });
    }
    child.on('error', error => { clearTimeout(timer); reject(error); });
    child.on('close', code => {
      clearTimeout(timer);
      if (expired || !accept.includes(code)) {
        reject(new Error(`Docker ${args[0]} ${expired ? 'timed out' : `exited ${code}`}: ${output}`));
      } else resolve(output.trim());
    });
  });
}
function containerName(suffix) {
  const name = `tdf-music-image-${runId}-${suffix}`;
  names.add(name); // also reconcile if create/run succeeds after CLI failure
  return name;
}

try {
  const metadata = JSON.parse(await docker(['image', 'inspect', reference]))[0];
  assert.match(metadata.Id, /^sha256:[0-9a-f]{64}$/);
  assert.equal(metadata.Os, 'linux');
  assert.equal(metadata.Config.User, 'worker');
  assert.deepEqual(metadata.Config.Entrypoint, ['/usr/bin/tini', '--']);
  assert.deepEqual(metadata.Config.Cmd, ['/app/scripts/run-music-release-worker.sh']);
  console.log(`Image under test: ${metadata.Id} (${metadata.Os}/${metadata.Architecture})`);

  const scriptFiles = ['process-music-release-audio.sh', 'process-music-release-preview.sh',
    'music-preview-spec.jq', 'process-music-release-artwork.sh', 'build-ddex-ern432-package.sh',
    'run-music-release-worker-once.sh', 'music-s3-upload.pl', 'run-music-release-worker.sh'];
  const hashesName = containerName('hashes');
  const actualHashes = await docker(['run', '--name', hashesName, '--label', label, ...limits,
    metadata.Id, '/bin/sh', '-c', 'sha256sum "$@" && cat /app/renderer-inputs.sha256',
    'checksum-probe', ...scriptFiles.map(file => `/app/scripts/${file}`)]);
  const inputs = scriptFiles.map(file => [`scripts/${file}`, `scripts/${file}`]);
  for (const file of ['app/MusicDdexRenderMain.hs', 'src/TDF/MusicRelease/Domain.hs',
    'src/TDF/MusicRelease/DDEX/ERN432.hs', 'music-worker/stack.yaml',
    'music-worker/stack.yaml.lock', 'music-worker/tdf-music-worker.cabal']) {
    inputs.push([`tdf-hq/${file}`, file]);
  }
  for (const [index, line] of actualHashes.split('\n').entries()) {
    assert(index < inputs.length, 'Unexpected image checksum output');
    const [source, bundled] = inputs[index];
    const expected = createHash('sha256').update(await readFile(new URL(`../${source}`, import.meta.url))).digest('hex');
    assert.equal(line, `${expected}  /app/${bundled}`, 'Image is stale or differs from this checkout');
  }
  assert.equal(actualHashes.split('\n').length, inputs.length);
  console.log('PASS bundled scripts and renderer build inputs match the current checkout SHA-256');

  const mediaName = containerName('media');
  console.log(await docker(['run', '--name', mediaName, '--label', label, ...limits,
    '--mount', `type=bind,source=${probe},target=/probe.sh,readonly`,
    metadata.Id, '/bin/sh', '/probe.sh'], { timeout: 240000 }));

  const configName = containerName('config');
  const configurationError = await docker(['run', '--name', configName, '--label', label,
    ...limits, metadata.Id, '/app/scripts/run-music-release-worker-once.sh'], { accept: [2] });
  assert.match(configurationError, /DATABASE_URL is required/);
  console.log('PASS missing configuration fails closed with exit 2 through tini');

  // Actual default entrypoint, supervisor and worker. The worker rejects absent
  // configuration, then the supervisor waits in its normal retry sleep. No stub.
  const supervisorName = containerName('signal');
  await docker(['run', '-d', '--name', supervisorName, '--label', label, ...limits,
    '-e', 'MUSIC_WORKER_POLL_SECONDS=30', metadata.Id]);
  const readyDeadline = Date.now() + 30000;
  let ready = false;
  while (Date.now() < readyDeadline) {
    if ((await docker(['logs', supervisorName])).includes('retrying after 30s')) { ready = true; break; }
    await pause(250, undefined, { signal: interrupted.signal });
  }
  assert(ready, 'Actual supervisor never reached retry wait');
  await docker(['stop', '--time=10', supervisorName], { timeout: 20000 });
  const state = JSON.parse(await docker(['inspect', '--format', '{{json .State}}', supervisorName]));
  assert.equal(state.Running, false);
  assert.equal(state.OOMKilled, false);
  assert.equal(state.ExitCode, 0, 'Supervisor must exit cleanly, not require SIGKILL');
  console.log('PASS Docker TERM traverses tini and interrupts the actual supervisor retry wait');
  console.log('Music worker Linux image: 8/8 scenarios passed (not a database/S3/CDN or DDEX conformance test).');
} finally {
  cleaning = true;
  for (const name of names) {
    const found = await docker(['ps', '-aq', '--filter', `name=^/${name}$`, '--filter', `label=${label}`]);
    if (found) {
      const owner = await docker(['inspect', '--format', '{{index .Config.Labels "tdf.music-image-test"}}', name]);
      assert.equal(owner, runId, 'Refusing to remove a container not owned by this run');
      await docker(['rm', '-f', name]);
    }
  }
  process.removeListener('SIGINT', onInterrupt);
  process.removeListener('SIGTERM', onInterrupt);
  console.log('Removed this run’s synthetic containers; built image retained.');
}
