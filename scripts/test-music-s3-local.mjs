// Real, isolated HTTPS/S3 transfers against MinIO with TDF's Haskell signer.
// No remote endpoint overrides, production credentials, database or CDN claims.
import assert from 'node:assert/strict';
import { createHash, randomBytes, randomUUID } from 'node:crypto';
import { spawn, spawnSync } from 'node:child_process';
import { copyFileSync, mkdtempSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import https from 'node:https';
import { tmpdir, userInfo } from 'node:os';
import { createServer } from 'node:net';
import { dirname, join, resolve } from 'node:path';
import { createInterface } from 'node:readline';
import { setTimeout as delay } from 'node:timers/promises';
import { fileURLToPath } from 'node:url';
import { readS3TestXmlScalar as xml } from './lib/music-s3-test-xml.mjs';
import { probeWorkerUploadFaults } from './lib/music-worker-s3-probe.mjs';
import { prepareLinuxIntegration } from './lib/music-linux-integration.mjs';

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const runtime = mkdtempSync(join(tmpdir(), 'tdf-music-s3-'));
const name = `tdf-music-s3-${randomUUID()}`;
const image = 'quay.io/minio/minio:RELEASE.2025-04-22T22-12-26Z' +
  '@sha256:a1ea29fa28355559ef137d71fc570e508a214ec84ff8083e39bc5428980b015e';
const bucket = 'music-local-private';
const key = 'quarantine/áudio + demo/master.wav';
const origin = 'http://127.0.0.1:4187';
const access = `local-${randomBytes(12).toString('hex')}`;
const secret = randomBytes(32).toString('hex');
const withLinuxWorker = process.argv.includes('--with-linux-worker');
const withBrowser = process.argv.includes('--with-browser');
const withApi = process.argv.includes('--with-api') || withLinuxWorker || withBrowser;
const schemaArgument = process.argv.find(value => value.startsWith('--ddex-schema-dir='))?.split('=').slice(1).join('=');
let ddexSchema;
// Deliberately do not inherit cloud, database, proxy or provider credentials.
const env = { PATH: process.env.PATH, HOME: process.env.HOME,
  LANG: 'C', LC_ALL: 'C', MINIO_ROOT_USER: access, MINIO_ROOT_PASSWORD: secret };
let container = false, signer, endpoint, ca, apiChild, passed = 0;
let linux;
const cancellation = new AbortController();
const interrupt = () => {
  cancellation.abort();
  void signer?.close();
  if (apiChild) process.kill(-apiChild.pid, 'SIGTERM');
};
process.once('SIGINT', interrupt);
process.once('SIGTERM', interrupt);
function run(program, args, options = {}) {
  const result = spawnSync(program, args, { env, encoding: 'utf8', timeout: 120000,
    maxBuffer: 8 * 1024 * 1024, ...options });
  if (result.error || result.status !== 0) {
    throw new Error(`${program} failed: ${String(result.error ?? result.stderr)
      .replaceAll(access, '[test access]').replaceAll(secret, '[test secret]')
      .replaceAll(options.env?.PGPASSWORD ?? options.env?.POSTGRES_PASSWORD ?? linux?.password ?? 'no-test-password', '[test DB password]')}`);
  }
  return result.stdout.trim();
}
function request(url, method = 'GET', body, headers = {}) {
  const target = new URL(url);
  assert.equal(target.origin, endpoint, 'Probe refuses any non-local storage URL');
  return new Promise((resolve, reject) => {
    // Independent operations intentionally do not share Node's idle socket pool;
    // this is a protocol test, not a keep-alive/reconnection benchmark.
    const req = https.request(target, { method, ca, agent: false, signal: cancellation.signal, headers: {
      ...headers, ...(body === undefined ? {} : { 'Content-Length': body.length }),
    }, timeout: 15000 }, (res) => {
      const chunks = [];
      res.on('data', (chunk) => chunks.push(chunk));
      res.on('error', reject);
      res.on('end', () => resolve({ status: res.statusCode, headers: res.headers,
        body: Buffer.concat(chunks) }));
    });
    req.on('timeout', () => req.destroy(new Error('Local HTTPS timeout')));
    req.on('error', reject);
    req.end(body);
  });
}
function startSigner() {
  const child = spawn('stack', ['exec', '--', 'runhaskell', '-isrc', 'test/MusicS3ProbeMain.hs'], {
    cwd: join(root, 'tdf-hq'), env: { ...env, MUSIC_S3_ENDPOINT: endpoint,
      MUSIC_S3_ACCESS_KEY_ID: access, MUSIC_S3_SECRET_ACCESS_KEY: secret },
    stdio: ['pipe', 'pipe', 'pipe'],
  });
  let pending, errors = '';
  const lines = createInterface({ input: child.stdout });
  child.stderr.on('data', (data) => { errors = (errors + data).slice(-4000); });
  child.on('error', (error) => pending?.reject(error));
  child.on('exit', () => pending?.reject(new Error(`Signer exited: ${errors}`)));
  lines.on('line', (line) => {
    try {
      const value = JSON.parse(line);
      assert.ok(value.url, 'Production signer rejected test input');
      pending?.resolve(value.url);
    } catch (error) { pending?.reject(error); }
    pending = undefined;
  });
  return {
    async sign(operation, extra = {}) {
      assert.ok(!pending, 'Only one signing request at a time');
      let timer;
      try {
        return await Promise.race([new Promise((resolve, reject) => {
          pending = { resolve, reject };
          child.stdin.write(`${JSON.stringify({ operation, bucket, key, ...extra })}\n`);
        }), new Promise((_, reject) => {
          timer = globalThis.setTimeout(() => reject(new Error(`Signer timeout: ${errors}`)), 120000);
        })]);
      } finally { clearTimeout(timer); }
    },
    async close() {
      lines.close();
      if (child.exitCode !== null || child.signalCode !== null) return;
      await new Promise((resolve) => {
        const timer = globalThis.setTimeout(() => child.kill('SIGKILL'), 5000);
        child.once('exit', () => { clearTimeout(timer); resolve(); });
        child.stdin.end();
      });
    },
  };
}
async function check(label, body) {
  cancellation.signal.throwIfAborted();
  await body(); passed += 1; console.log(`PASS ${label}`);
}
const digest = (bytes) => createHash('sha256').update(bytes).digest('hex');
try {
  if (schemaArgument !== undefined) {
    assert(withApi && schemaArgument, '--ddex-schema-dir requires API mode and an existing reviewed schema directory');
    ddexSchema = join(runtime, 'ddex-schema'); mkdirSync(ddexSchema);
    for (const [file, expected] of [
      ['release-notification.xsd', 'def25b4e72696c9bbc1fed84962acc3a9bae2bc92ef25f8393c99b362aa53a6a'],
      ['allowed-value-sets.xsd', '87e99fe74f57a640dce0d3247d16b3b52358562c1dbefc4617eb8a9b7360d943'],
    ]) {
      const path = join(resolve(schemaArgument), file);
      assert.equal(digest(readFileSync(path)), expected, 'Pinned official schema required');
      copyFileSync(path, join(ddexSchema, file));
    }
  }
  const certs = join(runtime, 'certs'); mkdirSync(certs);
  run('openssl', ['req', '-x509', '-newkey', 'rsa:2048', '-nodes', '-days', '1',
    '-subj', '/CN=127.0.0.1', '-addext', 'subjectAltName=IP:127.0.0.1,DNS:music-storage,DNS:music-db',
    '-keyout', join(certs, 'private.key'), '-out', join(certs, 'public.crt')]);
  ca = readFileSync(join(certs, 'public.crt'));
  if (withLinuxWorker) linux = await prepareLinuxIntegration({ run, root, runtime, certs, env, signal: cancellation.signal });
  // --pull=never makes network/image installation an explicit prerequisite.
  const identity = run('docker', ['image', 'inspect', image, '--format', '{{.Id}}']);
  console.log(`Local S3 image ${image} (${identity})`);
  run('docker', ['run', '-d', '--pull=never', '--name', name, '--label', 'tdf.test=music-s3',
    ...(linux ? ['--network', linux.edgeNetwork,
      '--label', `tdf.music-linux-run=${linux.network}`] : []),
    '--memory', '512m', '--cpus', '2', '--pids-limit', '128', '--cap-drop', 'ALL',
    '--security-opt', 'no-new-privileges', '-p', '127.0.0.1::9000',
    '--tmpfs', '/data:rw,size=268435456',
    '--mount', `type=bind,source=${certs},target=/certs,readonly`,
    '-e', 'MINIO_ROOT_USER', '-e', 'MINIO_ROOT_PASSWORD', '-e', 'MINIO_UPDATE=off',
    '-e', 'MINIO_BROWSER=off', '-e', `MINIO_API_CORS_ALLOW_ORIGIN=${origin}`,
    image, 'server', '/data', '--certs-dir', '/certs']);
  container = true;
  if (linux) run('docker', ['network', 'connect', '--alias', 'music-storage', linux.network, name]);
  const port = run('docker', ['port', name, '9000/tcp']);
  assert.match(port, /^127\.0\.0\.1:\d+$/);
  endpoint = `https://${port}`;
  let healthy = false, lastHealth = 'not checked';
  for (let i = 0; i < 60; i += 1) {
    cancellation.signal.throwIfAborted();
    try {
      const response = await request(`${endpoint}/minio/health/live`);
      healthy = response.status === 200; lastHealth = `HTTP ${response.status}`;
    } catch (error) { lastHealth = error.code ?? error.name; }
    if (healthy) break;
    await delay(500);
  }
  if (!healthy) {
    const state = run('docker', ['inspect', '--format', '{{.State.Status}} exit={{.State.ExitCode}} oom={{.State.OOMKilled}}', name]);
    const logs = run('docker', ['logs', '--tail', '40', name])
      .replaceAll(access, '[test access]').replaceAll(secret, '[test secret]');
    assert.fail(`Disposable MinIO did not become healthy: ${lastHealth}; ${state}\n${logs}`);
  }
  run('curl', ['--fail', '--silent', '--show-error', '--noproxy', '*',
    '--cacert', join(certs, 'public.crt'), '--aws-sigv4', 'aws:amz:us-east-1:s3',
    '--user', `${access}:${secret}`, '-X', 'PUT', `${endpoint}/${bucket}`]);
  signer = startSigner();
  let uploadId, firstEtag, secondEtag;
  const first = randomBytes(5 * 1024 * 1024);
  const second = randomBytes(128 * 1024);
  const full = Buffer.concat([first, second]);
  await check('production signer creates a real private multipart upload over verified TLS', async () => {
    const response = await request(await signer.sign('create'), 'POST');
    assert.equal(response.status, 200);
    uploadId = xml(response.body, 'UploadId');
  });
  await check('real part transfer, ETag exposure and idempotent retry of part one', async () => {
    const url = await signer.sign('part', { uploadId, part: 1 });
    const firstResponse = await request(url, 'PUT', first, { Origin: origin });
    assert.equal(firstResponse.status, 200);
    firstEtag = firstResponse.headers.etag;
    assert.ok(firstEtag);
    assert.equal(firstResponse.headers['access-control-allow-origin'], origin);
    assert.match(firstResponse.headers['access-control-expose-headers'] ?? '', /etag/i);
    const retry = await request(url, 'PUT', first);
    assert.equal(retry.status, 200); assert.equal(retry.headers.etag, firstEtag);
  });
  await check('resume after client process exits using the same upload ID and part evidence', async () => {
    const checkpoint = join(runtime, 'resume.json');
    writeFileSync(checkpoint, JSON.stringify({ uploadId, firstEtag }));
    await signer.close(); signer = startSigner();
    ({ uploadId, firstEtag } = JSON.parse(readFileSync(checkpoint, 'utf8')));
    const response = await request(await signer.sign('part', { uploadId, part: 2 }), 'PUT', second);
    assert.equal(response.status, 200); secondEtag = response.headers.etag; assert.ok(secondEtag);
  });
  await check('wrong part evidence cannot finalize the upload', async () => {
    const body = Buffer.from('<CompleteMultipartUpload><Part><PartNumber>1</PartNumber>' +
      '<ETag>"00000000000000000000000000000000"</ETag></Part></CompleteMultipartUpload>');
    const response = await request(await signer.sign('complete', { uploadId }), 'POST', body,
      { 'Content-Type': 'application/xml' });
    assert.match(response.body.toString(), /<Code>InvalidPart<\/Code>/);
    // S3 may return an embedded Error with HTTP 200; body is authoritative.
  });
  await check('multipart completion preserves the exact SHA-256 and bytes', async () => {
    const body = Buffer.from('<CompleteMultipartUpload>' + [firstEtag, secondEtag].map((etag, i) =>
      `<Part><PartNumber>${i + 1}</PartNumber><ETag>${etag}</ETag></Part>`).join('') +
      '</CompleteMultipartUpload>');
    const response = await request(await signer.sign('complete', { uploadId }), 'POST', body,
      { 'Content-Type': 'application/xml' });
    assert.equal(response.status, 200); assert.match(response.body.toString(), /<CompleteMultipartUploadResult/);
    const stored = await request(await signer.sign('get'));
    assert.equal(stored.status, 200); assert.equal(digest(stored.body), digest(full));
    assert.equal(stored.body.length, full.length);
  });
  await check('anonymous object access and bucket listing are denied', async () => {
    const signed = new URL(await signer.sign('get')); signed.search = '';
    assert.equal((await request(signed)).status, 403);
    assert.equal((await request(`${endpoint}/${bucket}?list-type=2`)).status, 403);
  });
  await check('signed GET cannot change object, method or expiration', async () => {
    const url = new URL(await signer.sign('get'));
    url.pathname += '-other'; assert.equal((await request(url)).status, 403);
    assert.equal((await request(await signer.sign('get'), 'DELETE')).status, 403);
    const expired = await signer.sign('get', { expiry: 1, offset: -120 });
    assert.equal((await request(expired)).status, 403);
    const extended = new URL(await signer.sign('get')); extended.searchParams.set('X-Amz-Expires', '900');
    assert.equal((await request(extended)).status, 403);
  });
  await check('authorized byte ranges return exact bytes and invalid ranges fail', async () => {
    const url = await signer.sign('get');
    const partial = await request(url, 'GET', undefined, { Range: 'bytes=1024-2047' });
    assert.equal(partial.status, 206);
    assert.equal(partial.headers['content-range'], `bytes 1024-2047/${full.length}`);
    assert.equal(digest(partial.body), digest(full.subarray(1024, 2048)));
    assert.equal((await request(url, 'GET', undefined, { Range: `bytes=${full.length + 1}-` })).status, 416);
  });
  await check('CORS preflight accepts only the configured test origin', async () => {
    const url = await signer.sign('get');
    const headers = { Origin: origin, 'Access-Control-Request-Method': 'GET',
      'Access-Control-Request-Headers': 'range' };
    const allowed = await request(url, 'OPTIONS', undefined, headers);
    assert.ok([200, 204].includes(allowed.status));
    assert.equal(allowed.headers['access-control-allow-origin'], origin);
    const denied = await request(url, 'OPTIONS', undefined, { ...headers, Origin: 'https://untrusted.invalid' });
    assert.equal(denied.headers['access-control-allow-origin'], undefined);
  });
  await check('abort invalidates the upload and prevents later part writes', async () => {
    const create = await request(await signer.sign('create', { key: 'quarantine/cancelled' }), 'POST');
    assert.equal(create.status, 200);
    const extra = { key: 'quarantine/cancelled', uploadId: xml(create.body, 'UploadId') };
    const part = await signer.sign('part', extra);
    assert.equal((await request(await signer.sign('abort', extra), 'DELETE')).status, 204);
    assert.equal((await request(part, 'PUT', second)).status, 404);
  });
  await check('worker single PUT preserves exact bytes with signed payload checksum', async () => {
    const source = join(runtime, 'worker-small'); writeFileSync(source, second);
    const receipt = JSON.parse(run('perl', [join(root, 'scripts/music-s3-upload.pl'),
      source, bucket, 'workers/small', 'application/octet-stream'], { env: { ...env,
      CURL_CA_BUNDLE: join(certs, 'public.crt'), MUSIC_S3_ENDPOINT: endpoint,
      MUSIC_S3_REGION: 'us-east-1', MUSIC_S3_ACCESS_KEY_ID: access, MUSIC_S3_SECRET_ACCESS_KEY: secret,
    } }));
    assert.deepEqual(receipt, { mode: 'single', bytes: second.length, sha256: digest(second), parts: 1 });
    const stored = await request(await signer.sign('get', { key: 'workers/small' }));
    assert.equal(stored.status, 200); assert.equal(digest(stored.body), digest(second));
  });
  await check('worker multipart completes and repeats without byte changes or incomplete uploads', async () => {
    const source = join(runtime, 'worker-multipart'); writeFileSync(source, full);
    const workerEnv = { ...env, CURL_CA_BUNDLE: join(certs, 'public.crt'), MUSIC_S3_ENDPOINT: endpoint,
      MUSIC_S3_REGION: 'us-east-1', MUSIC_S3_ACCESS_KEY_ID: access, MUSIC_S3_SECRET_ACCESS_KEY: secret,
      MUSIC_WORKER_MULTIPART_THRESHOLD_BYTES: String(first.length),
      MUSIC_WORKER_MULTIPART_PART_BYTES: String(first.length),
    };
    for (let attempt = 0; attempt < 2; attempt += 1) {
      const receipt = JSON.parse(run('perl', [join(root, 'scripts/music-s3-upload.pl'),
        source, bucket, 'workers/áudio + multipart', 'audio/wav'], { env: workerEnv }));
      assert.deepEqual(receipt, { mode: 'multipart', bytes: full.length, sha256: digest(full), parts: 2 });
      const stored = await request(await signer.sign('get', { key: 'workers/áudio + multipart' }));
      assert.equal(stored.status, 200); assert.equal(digest(stored.body), digest(full));
      assert.equal(stored.headers['content-type'], 'audio/wav');
    }
    const uploads = run('curl', ['-q', '--fail', '--silent', '--show-error', '--noproxy', '*',
      '--cacert', join(certs, 'public.crt'), '--aws-sigv4', 'aws:amz:us-east-1:s3',
      '--user', `${access}:${secret}`, `${endpoint}/${bucket}?uploads=`]);
    assert.doesNotMatch(uploads, /<Upload>/);
    assert.equal(digest(readFileSync(source)), digest(full));
  });
  await probeWorkerUploadFaults({ root, certs, endpoint, bucket,
    source: join(runtime, 'worker-multipart'), check, env: { ...env,
      CURL_CA_BUNDLE: join(certs, 'public.crt'), MUSIC_S3_REGION: 'us-east-1',
      MUSIC_S3_ACCESS_KEY_ID: access, MUSIC_S3_SECRET_ACCESS_KEY: secret,
      MUSIC_WORKER_MULTIPART_THRESHOLD_BYTES: String(first.length),
      MUSIC_WORKER_MULTIPART_PART_BYTES: String(first.length),
    } });
  await check('retried worker object has exact bytes; failed transfers leave no objects or multipart sessions', async () => {
    const retry = await request(await signer.sign('get', { key: 'workers/fault-retry' }));
    assert.equal(retry.status, 200); assert.equal(digest(retry.body), digest(full));
    for (const fault of ['corruption', 'embedded_error', 'cancel']) {
      assert.equal((await request(await signer.sign('get', { key: `workers/fault-${fault}` }))).status, 404);
    }
    const uploads = run('curl', ['-q', '--fail', '--silent', '--show-error', '--noproxy', '*',
      '--cacert', join(certs, 'public.crt'), '--aws-sigv4', 'aws:amz:us-east-1:s3',
      '--user', `${access}:${secret}`, `${endpoint}/${bucket}?uploads=`]);
    assert.doesNotMatch(uploads, /<Upload>/);
  });
  if (withApi) await check('real API + PostgreSQL + S3 + FFmpeg worker + editorial and entitlement flow', async () => {
    await signer.close(); signer = undefined;
    for (const apiBucket of ['music-e2e-quarantine', 'music-e2e-master', 'music-e2e-derivative', 'music-e2e-ddex']) {
      run('curl', ['--fail', '--silent', '--show-error', '--noproxy', '*',
        '--cacert', join(certs, 'public.crt'), '--aws-sigv4', 'aws:amz:us-east-1:s3',
        '--user', `${access}:${secret}`, '-X', 'PUT', `${endpoint}/${apiBucket}`]);
    }
    const install = run('stack', ['path', '--local-install-root'], { cwd: join(root, 'tdf-hq') });
    const portProbe = createServer();
    await new Promise((resolve, reject) => {
      portProbe.once('error', reject); portProbe.listen(0, '127.0.0.1', resolve);
    });
    const apiPort = portProbe.address().port;
    await new Promise((resolve) => portProbe.close(resolve));
    const password = randomBytes(24).toString('hex');
    await new Promise((resolve, reject) => {
      apiChild = spawn('sh', [join(root, 'scripts/test-music-release-api-e2e.sh')], {
        detached: true, stdio: ['ignore', 'pipe', 'pipe'], env: { ...env,
          ...(linux?.pgEnv ?? { PGHOST: '127.0.0.1', PGPORT: '5432', PGUSER: userInfo().username }),
          ...linux?.childEnv,
          TDF_MUSIC_API_E2E_BACKEND_EXE: join(install, 'bin/tdf-hq-exe'),
          TDF_MUSIC_API_E2E_PASSWORD: password, TDF_MUSIC_API_E2E_PORT: String(apiPort),
          TDF_MUSIC_API_E2E_DATABASE: `tdf_music_s3_${randomUUID().replaceAll('-', '')}`,
          TDF_MUSIC_API_E2E_S3_ENDPOINT: endpoint, TDF_MUSIC_API_E2E_S3_CA: join(certs, 'public.crt'),
          MUSIC_S3_ACCESS_KEY_ID: access, MUSIC_S3_SECRET_ACCESS_KEY: secret,
          ...(ddexSchema ? { TDF_MUSIC_API_E2E_DDEX_SCHEMA: ddexSchema } : {}),
          ...(withBrowser ? { TDF_MUSIC_API_E2E_BROWSER: '1' } : {}),
        },
      });
      const relay = (data) => process.stdout.write(String(data).replaceAll(access, '[test access]')
        .replaceAll(secret, '[test secret]').replaceAll(password, '[test password]')
        .replaceAll(linux?.password ?? 'no-test-password', '[test DB password]'));
      apiChild.stdout.on('data', relay); apiChild.stderr.on('data', relay);
      apiChild.on('error', reject);
      apiChild.on('close', (status) => {
        apiChild = undefined;
        status === 0 ? resolve() : reject(new Error(`Integrated API/S3 test failed (${status})`));
      });
    });
  });
  console.log(`PASS ${passed} local HTTPS/S3 integration scenarios; no CDN or cloud-provider validation`);
} finally {
  const cleanupErrors = [];
  try { await signer?.close(); } catch (error) { cleanupErrors.push(error); }
  try { if (container) {
    // Only this uniquely named, labeled disposable container is removed.
    assert.equal(run('docker', ['inspect', '--format', '{{index .Config.Labels "tdf.test"}}', name]), 'music-s3');
    run('docker', ['rm', '-f', name]);
  } } catch (error) { cleanupErrors.push(error); }
  try { linux?.close(); } catch (error) { cleanupErrors.push(error); }
  try { rmSync(runtime, { recursive: true, force: true }); } catch (error) { cleanupErrors.push(error); }
  process.removeListener('SIGINT', interrupt);
  process.removeListener('SIGTERM', interrupt);
  if (cleanupErrors.length) throw new AggregateError(cleanupErrors, 'Disposable resource cleanup failed');
  console.log('Removed this run’s disposable containers/network, synthetic objects, certificate and temporary credentials');
}
