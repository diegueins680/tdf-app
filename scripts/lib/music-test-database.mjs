// Disposable worker-test databases only. Never used by the application worker.
import assert from 'node:assert/strict';
import { setTimeout as sleep } from 'node:timers/promises';
import { disposablePostgresUrl } from './disposable-postgres-url.mjs';

export function disposableMusicDatabase({ database, command, env, pause = sleep }) {
  assert.match(database, /^tdf_music_worker_[0-9]{1,10}_[a-f0-9]{32}$/);
  const creator = `music-create-${database.split('_').at(-1)}`;
  let owned = false;
  const admin = (query) => command('psql', ['-XAtq', '-v', 'ON_ERROR_STOP=1', '-d', 'postgres', '-c', query]);
  const sessions = `application_name='${creator}' AND usename=current_user`;
  return {
    create() {
      assert.equal(admin(`SELECT count(*) FROM pg_database WHERE datname='${database}'`), '0',
        'Refusing to reuse an existing test database');
      // Claim the previously absent random name BEFORE waiting for createdb.
      // The server can finish creation after the client has timed out.
      owned = true;
      command('createdb', [database], { env: { ...env, PGAPPNAME: creator } });
    },
    async cleanup() {
      if (!owned) return;
      admin(`SELECT pg_cancel_backend(pid) FROM pg_stat_activity WHERE ${sessions} AND query LIKE '%${database}%'`);
      const deadline = performance.now() + 35000;
      while (admin(`SELECT count(*) FROM pg_stat_activity WHERE ${sessions}`) !== '0') {
        assert.ok(performance.now() < deadline,
          `Creator still active; retain ${database} for explicit reconciliation`);
        await pause(250);
      }
      // Only after the exact creation session disappears can a drop be final.
      command('dropdb', ['--if-exists', database]);
      owned = false;
    },
  };
}

// Keep ambient libpq/provider credentials out of synthetic worker processes.
// An explicit local/CI URL selects only the server; each run still owns a fresh
// random database and never reuses the URL's database name.
export function musicTestEnvironment({ source, username, runtime }) {
  const env = {
    PATH: source.PATH, LANG: 'C', LC_ALL: 'C', TMPDIR: runtime,
    PGHOST: '127.0.0.1', PGPORT: '5432', PGUSER: username,
  };
  if (source.TDF_MUSIC_WORKER_TEST_DATABASE_URL) {
    const url = disposablePostgresUrl(source.TDF_MUSIC_WORKER_TEST_DATABASE_URL,
      { ci: source.CI === 'true', env: source });
    env.PGHOST = url.hostname;
    env.PGPORT = url.port || '5432';
    env.PGUSER = decodeURIComponent(url.username) || username;
    if (url.password) env.PGPASSWORD = decodeURIComponent(url.password);
  }
  return env;
}
