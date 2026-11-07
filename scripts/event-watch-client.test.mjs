import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdtempSync, readFileSync, writeFileSync, statSync, rmSync, mkdirSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { provisioningSQL, revokeSQL } from './event-watch-client.mjs';

test('only a hash enters SQL and publication defaults off', () => {
  const secret = 'ab'.repeat(32);
  const sql = provisioningSQL({ clientId: 'quito-watch', partyId: '42', secret });
  assert.ok(!sql.includes(secret));
  assert.ok(sql.includes(createHash('sha256').update(secret).digest('hex')));
  assert.match(sql, /42,false/);
  assert.match(sql, /interval '90 days'/);
});
test('rejects SQL injection, invalid credentials and excessive lifetimes', () => {
  const base = { clientId: 'quito-watch', partyId: '42', secret: 'ab'.repeat(32) };
  for (const change of [{ clientId: "x'; DROP TABLE party;--" }, { partyId: '1;DELETE' },
    { secret: 'weak' }, { days: 91 }, { days: 0 }, { allowPublish: 'true' }]) {
    assert.throws(() => provisioningSQL({ ...base, ...change }));
  }
  assert.throws(() => revokeSQL("x' OR true--"));
});
test('publication is explicit and revocation never grants access', () => {
  assert.match(provisioningSQL({ clientId: 'quito-watch', partyId: 42,
    secret: 'ab'.repeat(32), allowPublish: true }), /42,true/);
  assert.equal(revokeSQL('quito-watch'),
    "UPDATE event_watch_client SET active=false WHERE client_id='quito-watch';\n");
});
test('CLI creates a private credential once, with no secret in stdout or SQL', () => {
  const root = mkdtempSync(path.join(tmpdir(), 'event-watch-client-'));
  try {
    const bin = path.join(root, 'bin');
    mkdirSync(bin);
    const sqlPath = path.join(root, 'request.sql');
    writeFileSync(path.join(bin, 'psql'), '#!/bin/sh\ncat > "$EVENT_WATCH_SQL_CAPTURE"\n', { mode: 0o700 });
    const output = path.join(root, 'credential.json');
    const cli = fileURLToPath(new URL('./event-watch-client.mjs', import.meta.url));
    const args = [cli, 'provision', 'quito-watch', '42', output];
    const options = { encoding: 'utf8', env: { ...process.env,
      PATH: `${bin}:${process.env.PATH}`, EVENT_WATCH_SQL_CAPTURE: sqlPath } };
    const result = spawnSync(process.execPath, args, options);
    assert.equal(result.status, 0, result.stderr);
    assert.equal(statSync(output).mode & 0o777, 0o600);
    const bytes = readFileSync(output, 'utf8');
    const secret = JSON.parse(bytes).client_secret;
    assert.match(secret, /^[0-9a-f]{64}$/);
    assert.ok(!`${result.stdout}${result.stderr}${readFileSync(sqlPath)}`.includes(secret));
    const replay = spawnSync(process.execPath, args, options);
    assert.equal(replay.status, 1);
    assert.equal(readFileSync(output, 'utf8'), bytes);
  } finally { rmSync(root, { recursive: true, force: true }); }
});
