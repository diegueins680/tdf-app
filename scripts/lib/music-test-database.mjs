// Disposable worker-test databases only. Never used by the application worker.
import assert from 'node:assert/strict';
import { setTimeout as sleep } from 'node:timers/promises';

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
