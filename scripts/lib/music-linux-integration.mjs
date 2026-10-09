import assert from 'node:assert/strict';
import { randomUUID, randomBytes } from 'node:crypto';
import { copyFileSync, chmodSync, mkdirSync } from 'node:fs';
import { isAbsolute, join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';

const postgres = 'postgres@sha256:7958605b474b3d264a969cb3a123d6aa00ad1e1fe9da8a69984dabb704d93317';
const networkPattern = /^tdf-music-linux-[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/;

// Only generated local fixture settings are accepted. Never route to Fly,
// host.docker.internal, a production database or arbitrary provider endpoints.
export function linuxWorkerConfiguration(source) {
  const network = source.TDF_MUSIC_LINUX_NETWORK;
  const image = source.TDF_MUSIC_LINUX_IMAGE;
  assert.match(network ?? '', networkPattern);
  assert.match(image ?? '', /^sha256:[0-9a-f]{64}$/);
  assert.match(source.TDF_MUSIC_API_E2E_DATABASE ?? '', /^tdf_music_s3_[0-9a-f]{32}$/);
  assert.equal(source.PGUSER, 'music_probe');
  assert.match(source.PGPASSWORD ?? '', /^[0-9a-f]{64}$/);
  assert.equal(source.PGSSLMODE, 'verify-full');
  assert.match(source.MUSIC_S3_ACCESS_KEY_ID ?? '', /^local-[0-9a-f]{24}$/);
  assert.match(source.MUSIC_S3_SECRET_ACCESS_KEY ?? '', /^[0-9a-f]{64}$/);
  assert(isAbsolute(source.TDF_MUSIC_API_E2E_S3_CA ?? ''), 'Fixture CA must be an absolute path');
  assert.doesNotMatch(source.TDF_MUSIC_API_E2E_S3_CA, /[,\r\n]/);
  const schema = source.TDF_MUSIC_API_E2E_DDEX_SCHEMA;
  if (schema !== undefined) {
    assert(isAbsolute(schema), 'Fixture schema must be absolute');
    assert.doesNotMatch(schema, /[,\r\n]/);
  }
  const name = `${network}-worker-${randomUUID()}`;
  const env = { PATH: source.PATH, HOME: source.HOME, LANG: 'C.UTF-8',
    DATABASE_URL: `postgresql://music-db:5432/${source.TDF_MUSIC_API_E2E_DATABASE}?user=music_probe&sslmode=verify-full&connect_timeout=10`,
    PGPASSWORD: source.PGPASSWORD, PGSSLROOTCERT: '/fixture-ca.crt',
    MUSIC_S3_ENDPOINT: 'https://music-storage:9000', MUSIC_S3_REGION: 'us-east-1',
    MUSIC_S3_ACCESS_KEY_ID: source.MUSIC_S3_ACCESS_KEY_ID,
    MUSIC_S3_SECRET_ACCESS_KEY: source.MUSIC_S3_SECRET_ACCESS_KEY,
    MUSIC_S3_MASTER_BUCKET: 'music-e2e-master', MUSIC_S3_DERIVATIVE_BUCKET: 'music-e2e-derivative',
    MUSIC_S3_DDEX_BUCKET: 'music-e2e-ddex', CURL_CA_BUNDLE: '/fixture-ca.crt',
    MUSIC_WORKER_ID: name, MUSIC_WORKER_MULTIPART_THRESHOLD_BYTES: '5242880',
    MUSIC_WORKER_MULTIPART_PART_BYTES: '5242880' };
  if (schema) env.MUSIC_DDEX_SCHEMA_DIR = '/fixture-ddex-schema';
  const keys = Object.keys(env).filter(key => !['PATH', 'HOME', 'LANG'].includes(key));
  const args = ['run', '--pull=never', '--name', name, '--label', `tdf.music-linux-run=${network}`,
    '--network', network, '--read-only', '--cap-drop=ALL', '--security-opt=no-new-privileges',
    '--memory=1g', '--memory-swap=1g', '--cpus=2', '--pids-limit=128',
    '--tmpfs=/tmp:rw,nosuid,nodev,size=512m,mode=1777',
    '--mount', `type=bind,source=${source.TDF_MUSIC_API_E2E_S3_CA},target=/fixture-ca.crt,readonly`,
    ...(schema ? ['--mount', `type=bind,source=${schema},target=/fixture-ddex-schema,readonly`] : []),
    ...keys.flatMap(key => ['-e', key]), image, 'bash', '-c',
    'set -eu; test "$(psql "$DATABASE_URL" -XAtq -v ON_ERROR_STOP=1 -c "SELECT ssl FROM pg_stat_ssl WHERE pid=pg_backend_pid()")" = t; exec /app/scripts/run-music-release-worker-once.sh'];
  return { network, name, env, args };
}

export async function prepareLinuxIntegration({ run, root, runtime, certs, env, signal }) {
  const network = `tdf-music-linux-${randomUUID()}`;
  const edgeNetwork = `${network}-loopback`;
  let networkCreated = false;
  let edgeCreated = false;
  const password = randomBytes(32).toString('hex');
  const dbName = `${network}-db`;
  const fixtureEnv = { ...env, POSTGRES_USER: 'music_probe', POSTGRES_PASSWORD: password };
  const close = () => {
    if (!networkCreated && !edgeCreated) return;
    for (const target of [networkCreated && network, edgeCreated && edgeNetwork].filter(Boolean)) {
      assert.equal(run('docker', ['network', 'inspect', '--format', '{{index .Labels "tdf.music-linux-run"}}', target]), network);
    }
    const containers = run('docker', ['ps', '-aq', '--filter', `label=tdf.music-linux-run=${network}`]);
    for (const id of containers.split('\n').filter(Boolean)) {
      assert.match(id, /^[0-9a-f]{12,64}$/);
      assert.equal(run('docker', ['inspect', '--format', '{{index .Config.Labels "tdf.music-linux-run"}}', id]), network);
      run('docker', ['rm', '-f', id]);
    }
    if (networkCreated) { run('docker', ['network', 'rm', network]); networkCreated = false; }
    if (edgeCreated) { run('docker', ['network', 'rm', edgeNetwork]); edgeCreated = false; }
  };
  try {
    const image = run('docker', ['image', 'inspect', 'tdf-music-worker:local-verification', '--format', '{{.Id}}']);
    assert.match(image, /^sha256:[0-9a-f]{64}$/);
    // Same exact image for preflight and all subsequent jobs; never pull/build.
    console.log(run('node', [join(root, 'scripts/test-music-worker-image.mjs'), image], { timeout: 300000 }));
    console.log(`PostgreSQL fixture: ${postgres} (${run('docker', ['image', 'inspect', postgres, '--format', '{{.Id}}'])})`);
    run('docker', ['network', 'create', '--internal', '--label', `tdf.music-linux-run=${network}`, network]);
    networkCreated = true;
    // Docker does not publish host ports on internal-only networks. Only the
    // fixture servers join this second bridge; workers stay internal-only.
    run('docker', ['network', 'create', '--label', `tdf.music-linux-run=${network}`,
      '--opt', 'com.docker.network.bridge.host_binding_ipv4=127.0.0.1', edgeNetwork]);
    edgeCreated = true;
    // Public-read permission inside a protected mkdtemp directory permits the
    // non-root DB container to copy this synthetic key into its private tmpfs.
    // No private key is mounted in the worker.
    const dbCerts = join(runtime, 'db-certs'); mkdirSync(dbCerts);
    copyFileSync(join(certs, 'public.crt'), join(dbCerts, 'public.crt'));
    copyFileSync(join(certs, 'private.key'), join(dbCerts, 'private.key'));
    chmodSync(join(dbCerts, 'private.key'), 0o644);
    run('docker', ['run', '-d', '--pull=never', '--name', dbName,
      '--label', `tdf.music-linux-run=${network}`, '--network', edgeNetwork,
      '--user=postgres', '--read-only', '--cap-drop=ALL', '--security-opt=no-new-privileges',
      '--memory=1g', '--cpus=2', '--pids-limit=128', '-p', '127.0.0.1::5432',
      '--tmpfs=/var/lib/postgresql/data:rw,size=512m,uid=999,gid=999,mode=0700',
      '--tmpfs=/var/run/postgresql:rw,size=16m,uid=999,gid=999,mode=0775',
      '--tmpfs=/tmp:rw,size=16m,mode=1777',
      '--mount', `type=bind,source=${dbCerts},target=/certs,readonly`,
      '-e', 'POSTGRES_USER', '-e', 'POSTGRES_PASSWORD', '-e', 'POSTGRES_DB=postgres',
      '-e', 'POSTGRES_INITDB_ARGS=--auth-host=scram-sha-256 --auth-local=trust',
      '--entrypoint', '/bin/bash', postgres, '-c',
      'set -eu; cp /certs/public.crt /tmp/server.crt; cp /certs/private.key /tmp/server.key; chmod 0600 /tmp/server.key; exec docker-entrypoint.sh postgres -c ssl=on -c ssl_cert_file=/tmp/server.crt -c ssl_key_file=/tmp/server.key'],
    { env: fixtureEnv });
    run('docker', ['network', 'connect', '--alias', 'music-db', network, dbName]);
    const binding = run('docker', ['port', dbName, '5432/tcp']);
    assert.match(binding, /^127\.0\.0\.1:\d+$/);
    const pgEnv = { PGHOST: '127.0.0.1', PGPORT: binding.split(':')[1], PGUSER: 'music_probe',
      PGPASSWORD: password, PGSSLMODE: 'verify-full', PGSSLROOTCERT: join(certs, 'public.crt'),
      PGCONNECT_TIMEOUT: '3' };
    let ready = false;
    for (let i = 0; i < 60; i++) {
      signal.throwIfAborted();
      try {
        ready = run('psql', ['-XAtq', '-d', 'postgres', '-c', 'SELECT ssl FROM pg_stat_ssl WHERE pid=pg_backend_pid()'],
          { env: { ...env, ...pgEnv } }) === 't';
      } catch { /* expected only while the disposable server initializes */ }
      if (ready) break;
      await delay(500, undefined, { signal });
    }
    assert(ready, 'Disposable PostgreSQL did not accept a verify-full TLS connection');
    console.log('PASS isolated PostgreSQL accepts verified TLS; no host database used');
    return { network, edgeNetwork, password, close, pgEnv,
      childEnv: { TDF_MUSIC_LINUX_NETWORK: network, TDF_MUSIC_LINUX_IMAGE: image } };
  } catch (error) { close(); throw error; }
}
