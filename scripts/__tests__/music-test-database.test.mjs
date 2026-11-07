import assert from 'node:assert/strict';
import test from 'node:test';
import { disposableMusicDatabase, musicTestEnvironment, musicTestDatabaseUrl } from '../lib/music-test-database.mjs';
const database = `tdf_music_worker_123_${'a'.repeat(32)}`;
function fixture({ exists = false, failCreate = false } = {}) {
  const calls = []; let polls = 0;
  const command = (program, args, options) => {
    calls.push({ program, args, options });
    if (program === 'createdb' && failCreate) throw new Error('Synthetic lost creation response');
    const query = args.at(-1);
    if (program === 'psql' && query.includes('pg_database')) return exists ? '1' : '0';
    if (program === 'psql' && query.includes('SELECT count(*) FROM pg_stat_activity')) return ++polls === 1 ? '1' : '0';
    return '';
  };
  return { calls, db: disposableMusicDatabase({ database, command, env: { PGHOST: '127.0.0.1' }, pause: async () => {} }) };
}
test('existing database is never claimed, cancelled or dropped', async () => {
  const f = fixture({ exists: true }); assert.throws(() => f.db.create(), /Refusing to reuse/);
  await f.db.cleanup(); assert.equal(f.calls.length, 1);
});
for (const failCreate of [false, true]) test(`cleanup waits for exact creator after ${failCreate ? 'timeout' : 'success'}`, async () => {
  const f = fixture({ failCreate });
  if (failCreate) assert.throws(() => f.db.create(), /lost creation response/); else f.db.create();
  const creation = f.calls.find((c) => c.program === 'createdb');
  assert.equal(creation.options.env.PGAPPNAME, `music-create-${'a'.repeat(32)}`);
  await f.db.cleanup(); const count = f.calls.length; await f.db.cleanup(); assert.equal(f.calls.length, count);
  const cancellation = f.calls.find((c) => c.args.at(-1).includes('pg_cancel_backend'));
  assert.match(cancellation.args.at(-1), /usename=current_user/);
  assert.ok(cancellation.args.at(-1).includes(database));
  assert.equal(f.calls.filter((c) => c.args.at(-1).includes('SELECT count(*) FROM pg_stat_activity')).length, 2);
  assert.deepEqual(f.calls.at(-1), { program: 'dropdb', args: ['--if-exists', database], options: undefined });
});
test('rejects broad, non-test or injected database targets', () => {
  for (const database of ['postgres', 'tdf_music_worker_%', "tdf_music_worker_1_';DROP DATABASE postgres;",
    `tdf_music_worker_${'1'.repeat(40)}_${'a'.repeat(32)}`]) {
    assert.throws(() => disposableMusicDatabase({ database }));
  }
});

test('worker fixture ignores ambient libpq and provider credentials by default', () => {
  const env = musicTestEnvironment({ source: {
    PATH: '/test/bin', PGHOST: 'production.invalid', PGPASSWORD: 'unrelated',
    PGSERVICE: 'production', AWS_SECRET_ACCESS_KEY: 'unrelated',
  }, username: 'tester', runtime: '/private/synthetic' });
  assert.deepEqual(env, { PATH: '/test/bin', LANG: 'C', LC_ALL: 'C',
    TMPDIR: '/private/synthetic', PGHOST: '127.0.0.1', PGPORT: '5432', PGUSER: 'tester' });
});
test('explicit isolated CI connection never reuses the supplied database', () => {
  const env = musicTestEnvironment({ source: { CI: 'true', PATH: '/test/bin',
    TDF_MUSIC_WORKER_TEST_DATABASE_URL: 'postgresql://postgres:p%40ss@postgres:5432/worker_test',
  }, username: 'root', runtime: '/tmp/synthetic' });
  assert.equal(env.PGHOST, 'postgres'); assert.equal(env.PGUSER, 'postgres');
  assert.equal(env.PGPASSWORD, 'p@ss'); assert.equal(env.PGDATABASE, undefined);
  const worker = new URL(musicTestDatabaseUrl(database, env));
  assert.equal(worker.hostname, 'postgres'); assert.equal(worker.port, '5432');
  assert.equal(worker.username, 'postgres'); assert.equal(decodeURIComponent(worker.password), 'p@ss');
  assert.equal(worker.pathname, `/${database}`); assert.equal(worker.search, '');

});
test('worker CI connection rejects remote, non-test and connection override targets', () => {
  for (const value of [
    'postgresql://postgres@production.invalid/worker_test',
    'postgresql://postgres@localhost/production',
    'postgresql://postgres@localhost/worker_test?host=production.invalid',
  ]) assert.throws(() => musicTestEnvironment({ source: { CI: 'true',
    TDF_MUSIC_WORKER_TEST_DATABASE_URL: value }, username: 'root', runtime: '/tmp/test' }));
  assert.throws(() => musicTestEnvironment({ source: {
    TDF_MUSIC_WORKER_TEST_DATABASE_URL: 'postgresql://postgres@postgres/worker_test',
  }, username: 'root', runtime: '/tmp/test' }));
  assert.throws(() => musicTestEnvironment({ source: { CI: 'true', PGHOSTADDR: '203.0.113.1',
    TDF_MUSIC_WORKER_TEST_DATABASE_URL: 'postgresql://postgres@postgres/worker_test',
  }, username: 'root', runtime: '/tmp/test' }));
});

test('worker connection follows the isolated server port and never targets a non-owned database', () => {
  const env = musicTestEnvironment({ source: {
    TDF_MUSIC_WORKER_TEST_DATABASE_URL: 'postgresql://tester:synthetic@127.0.0.1:55448/worker_test',
  }, username: 'unrelated', runtime: '/tmp/synthetic' });
  const worker = new URL(musicTestDatabaseUrl(database, env));
  assert.equal(worker.port, '55448'); assert.equal(worker.username, 'tester');
  assert.equal(worker.pathname, `/${database}`);
  assert.throws(() => musicTestDatabaseUrl('postgres', env));
  assert.throws(() => musicTestDatabaseUrl(database, { ...env, PGHOST: 'production.invalid' }));
});

import { mkdtempSync, existsSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

test('synthetic renderer accepts owned local/CI URLs and rejects routing overrides before writing', () => {
  const renderer = fileURLToPath(new URL('../../test/fixtures/music-worker/ddex-render.mjs', import.meta.url));
  const cases = [
    [`postgresql://tester:synthetic@127.0.0.1:55448/${database}`, {}, true],
    [`postgresql://postgres:synthetic@postgres:5432/${database}`, { CI: 'true' }, true],
    [`postgresql://postgres@postgres:5432/${database}`, {}, false],
    [`postgresql://production.invalid/${database}`, {}, false],
    ['postgresql://127.0.0.1/production', {}, false],
    [`postgresql://127.0.0.1/${database}?host=production.invalid`, {}, false],
    [`postgresql://127.0.0.1/${database}`, { PGHOSTADDR: '203.0.113.1' }, false],
  ];
  for (const [url, extra, accepted] of cases) {
    const scratch = mkdtempSync(join(tmpdir(), 'tdf-renderer-connection-'));
    try {
      const xml = join(scratch, 'fixture.xml');
      const result = spawnSync(process.execPath, [renderer, 'synthetic', xml, join(scratch, 'manifest')],
        { encoding: 'utf8', env: { PATH: process.env.PATH, DATABASE_URL: url, ...extra } });
      assert.equal(result.status === 0, accepted, result.stderr);
      assert.equal(existsSync(xml), accepted);
    } finally { rmSync(scratch, { recursive: true }); }
  }
});
