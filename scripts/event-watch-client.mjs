#!/usr/bin/env node
// Operator-only provisioning. No API/admin credential is stored in the repository.
import { randomBytes, createHash } from 'node:crypto';
import { openSync, writeFileSync, closeSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import { pathToFileURL } from 'node:url';

export function provisioningSQL({ clientId, partyId, secret, days = 90, allowPublish = false }) {
  if (!/^[a-z0-9][a-z0-9_-]{2,47}$/.test(clientId ?? '')) throw new Error('Invalid client id');
  if (!/^[1-9][0-9]{0,14}$/.test(String(partyId))) throw new Error('Invalid Party id');
  if (!/^[0-9a-f]{64}$/.test(secret ?? '')) throw new Error('Expected a random 32-byte hex secret');
  if (!Number.isInteger(days) || days < 1 || days > 90) throw new Error('Expiry must be 1–90 days');
  if (typeof allowPublish !== 'boolean') throw new Error('Invalid publication flag');
  const digest = createHash('sha256').update(secret).digest('hex');
  return `BEGIN;
INSERT INTO event_watch_client(client_id,secret_hash,actor_party_id,allow_publish,expires_at)
VALUES ('${clientId}','${digest}',${partyId},${allowPublish},clock_timestamp()+interval '${days} days')
ON CONFLICT (client_id) DO UPDATE SET secret_hash=EXCLUDED.secret_hash,
  actor_party_id=EXCLUDED.actor_party_id,active=true,
  allow_publish=EXCLUDED.allow_publish,expires_at=EXCLUDED.expires_at;
COMMIT;\n`;
}

export function revokeSQL(clientId) {
  if (!/^[a-z0-9][a-z0-9_-]{2,47}$/.test(clientId ?? '')) throw new Error('Invalid client id');
  return `UPDATE event_watch_client SET active=false WHERE client_id='${clientId}';\n`;
}

function main(args) {
  const [command, clientId, partyId, output, publication] = args;
  if (command === 'revoke' && args.length === 2) {
    const result = spawnSync('psql', ['-X', '-v', 'ON_ERROR_STOP=1', '-q'],
      { input: revokeSQL(clientId), stdio: ['pipe', 'pipe', 'pipe'], encoding: 'utf8' });
    if (result.error || result.status !== 0) throw new Error('Revocation failed; inspect PostgreSQL privately');
    console.log('Client revoked; all outstanding sessions invalidated.');
    return;
  }
  if (command !== 'provision' || ![4, 5].includes(args.length)
      || (publication !== undefined && publication !== '--allow-publish')) {
    throw new Error('Usage: event-watch-client.mjs provision CLIENT PARTY PRIVATE_OUTPUT [--allow-publish] | revoke CLIENT');
  }
  const secret = randomBytes(32).toString('hex');
  const sql = provisioningSQL({ clientId, partyId, secret, allowPublish: publication === '--allow-publish' });
  // Exclusive creation refuses existing files and symlinks. Restrictive mode at
  // creation, rather than chmod after writing. No secret in argv/stdout/SQL.
  const fd = openSync(output, 'wx', 0o600);
  try {
    writeFileSync(fd, JSON.stringify({ client_id: clientId, client_secret: secret,
      grant_type: 'client_credentials' }) + '\n');
  } finally { closeSync(fd); }
  const result = spawnSync('psql', ['-X', '-v', 'ON_ERROR_STOP=1', '-q'],
    { input: sql, stdio: ['pipe', 'pipe', 'pipe'], encoding: 'utf8' });
  if (result.error || result.status !== 0) {
    throw new Error('Provisioning failed or commit is uncertain; retain the private file and inspect PostgreSQL privately. Do not retry blindly.');
  }
  console.log('Client provisioned. Import the private output into the job secret store; never commit it.');
}
if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  try { main(process.argv.slice(2)); } catch (error) { console.error(error.message); process.exitCode = 1; }
}
