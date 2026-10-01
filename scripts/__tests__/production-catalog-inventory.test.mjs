import test from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { EventEmitter } from 'node:events';
import { fetchJson, validateOriginAddress, validateProvenance } from '../production-catalog-inventory.mjs';
const health = { status: 'ok', db: 'ok' };
const version = { commit: 'a'.repeat(40) };
const deployment = { provider: 'hetzner', project: 'tdf-production', database: 'tdf_hq', health, version,
  databaseVolume: 'tdf_production_postgres_data', sshServerAddress: '178.105.93.101', apiContainer: 'api', databaseContainer: 'db', apiImage: 'digest', configuredImage: 'digest', databaseImage: 'pg17' };
const metadata = { database: 'tdf_hq', transactionReadOnly: 'on' };
test('accepts a stable current API/database deployment', () => {
  assert.doesNotThrow(() => validateProvenance(health, version, deployment, deployment, metadata));
});
test('refuses mismatched public origin, unhealthy database, stale target and deployment races', () => {
  for (const change of [{ version: { commit: 'b'.repeat(40) } }, { health: { status: 'ok', db: 'down' } },
    { database: 'trader' }, { databaseVolume: 'restore_data' }, { provider: 'fly' }, { apiContainer: 'replaced' }, { databaseImage: 'replaced' }]) {
    assert.throws(() => validateProvenance(health, version, deployment, { ...deployment, ...change }, metadata));
  }
  assert.throws(() => validateProvenance(health, version, deployment, deployment, { ...metadata, transactionReadOnly: 'off' }));
  assert.throws(() => validateProvenance(health, version, deployment, deployment, { ...metadata, database: 'trader' }));
});
test('dry-run SQL keeps bounded read-only and sensitive-column exclusions', () => {
  const sql = execFileSync(process.execPath, ['scripts/production-catalog-inventory.mjs', '--dry-run'], { encoding: 'utf8' });
  assert.match(sql, /BEGIN TRANSACTION READ ONLY/);
  assert.match(sql, /statement_timeout = '120s'/);
  assert.match(sql, /LIMIT 200/);
  assert.match(sql, /token\|secret\|password\|credential\|email\|phone/);
  assert.match(sql, /ROLLBACK;/);
});

test('rejects another deployment even when its Git SHA and health are identical', async () => {
  const expected = deployment.sshServerAddress;
  assert.doesNotThrow(() => validateOriginAddress('::ffff:' + expected, expected));
  for (const address of ['203.0.113.10', undefined, 'api.tdfrecords.net']) {
    assert.throws(() => validateOriginAddress(address, expected));
  }
  for (const peer of [expected, '203.0.113.10']) {
    const get = (url, options, callback) => {
      assert.equal(options.agent, false);
      assert.equal(options.rejectUnauthorized, undefined); // Node's strict TLS default.
      const req = new EventEmitter();
      queueMicrotask(() => {
        const response = new EventEmitter();
        response.socket = { remoteAddress: peer };
        response.statusCode = 200;
        response.setEncoding = () => {};
        response.destroy = () => {};
        callback(response);
        response.emit('data', JSON.stringify(version));
        response.emit('end');
      });
      return req;
    };
    const result = fetchJson('https://api.tdfrecords.net/version', expected, { get });
    if (peer === expected) assert.deepEqual(await result, version);
    else await assert.rejects(result, /SSH-authenticated/);
  }
});
test('rejects mixed public DNS answers before accepting a matching peer', async () => {
  const expected = deployment.sshServerAddress;
  for (const addresses of [[{ address: expected, family: 4 }],
    [{ address: expected, family: 4 }, { address: '203.0.113.10', family: 4 }], []]) {
    const lookup = (host, options, callback) => callback(null, addresses);
    const get = (url, options) => {
      const req = new EventEmitter();
      queueMicrotask(() => options.lookup('api.tdfrecords.net', { all: true }, (error, result) => {
        if (!error) assert.deepEqual(result, addresses);
        req.emit('error', error || new Error('accepted matching DNS'));
      }));
      return req;
    };
    await assert.rejects(fetchJson('https://api.tdfrecords.net/version', expected, { get, lookup }),
      addresses.length === 1 ? /accepted matching DNS/ : /SSH-authenticated|no origin/);
  }
});
