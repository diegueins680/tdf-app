import assert from 'node:assert/strict';
import test from 'node:test';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { mkdtempSync, mkdirSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { linuxWorkerConfiguration, prepareLinuxIntegration } from '../lib/music-linux-integration.mjs';

const source = {
  PATH: '/usr/bin', HOME: '/fixture',
  TDF_MUSIC_LINUX_NETWORK: 'tdf-music-linux-01234567-89ab-4cde-8123-456789abcdef',
  TDF_MUSIC_LINUX_IMAGE: `sha256:${'a'.repeat(64)}`,
  TDF_MUSIC_API_E2E_DATABASE: `tdf_music_s3_${'b'.repeat(32)}`,
  PGUSER: 'music_probe', PGPASSWORD: 'c'.repeat(64), PGSSLMODE: 'verify-full',
  MUSIC_S3_ACCESS_KEY_ID: `local-${'d'.repeat(24)}`, MUSIC_S3_SECRET_ACCESS_KEY: 'e'.repeat(64),
  TDF_MUSIC_API_E2E_S3_CA: '/tmp/fixture/public.crt',
};

test('Linux worker uses only verified internal TLS and the immutable image', () => {
  const { env, args } = linuxWorkerConfiguration({ ...source, DATABASE_URL: 'production',
    MUSIC_S3_ENDPOINT: 'https://production', HTTP_PROXY: 'https://proxy', AWS_PROFILE: 'production' });
  assert.equal(env.MUSIC_S3_ENDPOINT, 'https://music-storage:9000');
  const db = new URL(env.DATABASE_URL);
  assert.equal(db.hostname, 'music-db'); assert.equal(db.port, '5432');
  assert.equal(db.searchParams.get('sslmode'), 'verify-full');
  assert.equal(env.PGSSLROOTCERT, '/fixture-ca.crt');
  assert.equal(env.CURL_CA_BUNDLE, '/fixture-ca.crt');
  assert(!('HTTP_PROXY' in env)); assert(!('AWS_PROFILE' in env));
  assert(args.includes(source.TDF_MUSIC_LINUX_IMAGE));
  for (const flag of ['--read-only', '--cap-drop=ALL', '--pull=never', '--security-opt=no-new-privileges']) assert(args.includes(flag));
  assert.match(args.at(-1), /pg_stat_ssl/);
  assert.match(args.at(-1), /exec \/app\/scripts\/run-music-release-worker-once.sh$/);
  assert.doesNotMatch(args.join(' '), /host\.docker\.internal|--privileged|--network=host/);
});

test('Credentials stay outside argv and only a read-only public CA is mounted', () => {
  const { args } = linuxWorkerConfiguration(source);
  for (const key of ['PGPASSWORD', 'MUSIC_S3_ACCESS_KEY_ID', 'MUSIC_S3_SECRET_ACCESS_KEY']) {
    assert(args.includes(key)); assert(!args.join(' ').includes(source[key]));
  }
  assert.equal(args.filter(arg => arg === '--mount').length, 1);
  assert.equal(args[args.indexOf('--mount') + 1], 'type=bind,source=/tmp/fixture/public.crt,target=/fixture-ca.crt,readonly');
});

test('Unsafe or non-fixture configuration fails before invoking Docker', () => {
  for (const [key, value] of [
    ['TDF_MUSIC_LINUX_NETWORK', 'production'], ['TDF_MUSIC_LINUX_NETWORK', `tdf-music-linux-${'-'.repeat(36)}`],
    ['TDF_MUSIC_LINUX_IMAGE', 'worker:latest'], ['TDF_MUSIC_API_E2E_DATABASE', 'tdf_production'],
    ['PGUSER', 'postgres'], ['PGPASSWORD', ''], ['PGSSLMODE', 'require'],
    ['MUSIC_S3_ACCESS_KEY_ID', 'real-access'], ['MUSIC_S3_SECRET_ACCESS_KEY', ''],
    ['TDF_MUSIC_API_E2E_S3_CA', 'relative.crt'], ['TDF_MUSIC_API_E2E_S3_CA', '/tmp/public.crt,target=/app'],
  ]) assert.throws(() => linuxWorkerConfiguration({ ...source, [key]: value }), undefined, key);
});

test('Every worker gets a fresh label-scoped name', () => {
  const first = linuxWorkerConfiguration(source), second = linuxWorkerConfiguration(source);
  assert.notEqual(first.name, second.name);
  assert(first.args.includes(`tdf.music-linux-run=${source.TDF_MUSIC_LINUX_NETWORK}`));
  assert.equal(first.env.MUSIC_WORKER_ID, first.name);
});

test('Optional reviewed DDEX schemas mount read-only without inheriting host runtime paths', () => {
  const { env, args } = linuxWorkerConfiguration({ ...source,
    TDF_MUSIC_API_E2E_DDEX_SCHEMA: '/tmp/synthetic/ddex-schema', MUSIC_DDEX_SCHEMA_DIR: '/production' });
  assert.equal(env.MUSIC_DDEX_SCHEMA_DIR, '/fixture-ddex-schema');
  assert(args.includes('type=bind,source=/tmp/synthetic/ddex-schema,target=/fixture-ddex-schema,readonly'));
  assert.equal(args.filter(arg => arg === '--mount').length, 2);
  for (const schema of ['relative', '/tmp/schema,target=/app', '/tmp/schema\n']) {
    assert.throws(() => linuxWorkerConfiguration({ ...source, TDF_MUSIC_API_E2E_DDEX_SCHEMA: schema }));
  }
});

test('API migration harness rejects remote libpq routing before touching a database', () => {
  const script = fileURLToPath(new URL('../test-music-release-api-e2e.sh', import.meta.url));
  for (const override of [{ PGHOST: 'remote.example' }, { PGHOSTADDR: '192.0.2.1' },
    { PGSERVICE: 'production' }, { PGSERVICEFILE: '/tmp/external.conf' }, { PGPORT: 'invalid' }]) {
    const result = spawnSync('sh', [script], { encoding: 'utf8', timeout: 10000,
      env: { PATH: process.env.PATH, HOME: process.env.HOME, ...override } });
    assert.equal(result.status, 2, result.stderr);
    assert.match(result.stderr, /Music API E2E (requires|refuses)/);
  }
});

for (const failure of ['bridge', 'database']) test(`Fixture setup cleans owned resources after ${failure} failure (command double)`, async () => {
  const runtime = mkdtempSync(join(tmpdir(), 'tdf-music-linux-cleanup-'));
  const certs = join(runtime, 'certs'); mkdirSync(certs);
  writeFileSync(join(certs, 'public.crt'), 'synthetic certificate');
  writeFileSync(join(certs, 'private.key'), 'synthetic key');
  let network;
  const removed = [];
  const run = (program, args) => {
    if (program === 'node') return 'Command double: no image execution';
    assert.equal(program, 'docker');
    if (args[0] === 'image') return source.TDF_MUSIC_LINUX_IMAGE;
    if (args[0] === 'network' && args[1] === 'create') {
      network ??= args.at(-1);
      if (failure === 'bridge' && args.at(-1).endsWith('-loopback')) throw new Error('synthetic setup failure');
      return network;
    }
    if (args[0] === 'port') throw new Error('synthetic setup failure');
    if ((args[0] === 'network' && args[1] === 'inspect') || args[0] === 'inspect') return network;
    if (args[0] === 'ps') return failure === 'database' ? '0123456789ab' : '';
    if (args[0] === 'rm' || (args[0] === 'network' && args[1] === 'rm')) removed.push(args.at(-1));
    return '';
  };
  try {
    await assert.rejects(prepareLinuxIntegration({ run, root: '/', runtime, certs, env: {},
      signal: new AbortController().signal }), /synthetic setup failure/);
    assert.deepEqual(removed, failure === 'database'
      ? ['0123456789ab', network, `${network}-loopback`] : [network]);
  } finally { rmSync(runtime, { recursive: true, force: true }); }
});
