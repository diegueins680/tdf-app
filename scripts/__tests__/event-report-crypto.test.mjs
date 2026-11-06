import assert from 'node:assert/strict';
import test from 'node:test';
import { createDecipheriv } from 'node:crypto';
import { mkdtempSync, readFileSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';
import { openEventReport, sealEventReport, tokenDigest } from '../../functions/_shared/event-report-crypto.mjs';
import { independentEnvelope, reportToken, syntheticReport } from './fixtures/event-report.mjs';

test('Worker decrypts an independent AES-256-GCM fixture with the complete capability', async () => {
  assert.deepEqual(await openEventReport(independentEnvelope(), reportToken), syntheticReport);
});

test('fresh sealing is interoperable and uses a different nonce on every revision', async () => {
  const first = await sealEventReport(syntheticReport, reportToken);
  const second = await sealEventReport(syntheticReport, reportToken);
  assert.notEqual(first.nonce, second.nonce);
  assert.notEqual(first.ciphertext, second.ciphertext);
  assert.equal(first.tokenSha256, await tokenDigest(reportToken));
  const combined = Buffer.from(first.ciphertext, 'base64url');
  const decipher = createDecipheriv('aes-256-gcm', Buffer.from(reportToken, 'base64url'), Buffer.from(first.nonce, 'base64url'));
  decipher.setAAD(Buffer.from('tdf-event-report:v1'));
  decipher.setAuthTag(combined.subarray(-16));
  const plaintext = Buffer.concat([decipher.update(combined.subarray(0, -16)), decipher.final()]);
  assert.deepEqual(JSON.parse(plaintext), syntheticReport);
  assert.doesNotMatch(JSON.stringify(first), /Synthetic report fixture|Example venue|Fixture synthetic-only/);
  assert.ok(!JSON.stringify(first).includes(reportToken));
});

test('wrong key, digest override, tampered ciphertext, nonce and associated data deny access', async () => {
  const original = independentEnvelope();
  const wrong = Buffer.alloc(32, 8).toString('base64url');
  assert.equal(await openEventReport(original, wrong), null);
  assert.equal(await openEventReport({ ...original, tokenSha256: await tokenDigest(wrong) }, wrong), null);
  const changed = Buffer.from(original.ciphertext, 'base64url');
  changed[0] ^= 1;
  const tagChanged = Buffer.from(original.ciphertext, 'base64url');
  tagChanged[tagChanged.length - 1] ^= 1;
  for (const envelope of [
    { ...original, ciphertext: changed.toString('base64url') },
    { ...original, ciphertext: tagChanged.toString('base64url') },
    { ...original, nonce: Buffer.alloc(12, 4).toString('base64url') },
    independentEnvelope(syntheticReport, reportToken, { aad: 'another-purpose' }),
  ]) assert.equal(await openEventReport(envelope, reportToken), null);
});

test('malformed envelopes, noncanonical capabilities and unauthenticated payload shapes fail closed', async () => {
  const original = independentEnvelope();
  for (const envelope of [null, {}, { ...original, version: 2 },
    { ...original, nonce: original.nonce + '=' }, { ...original, nonce: 'AA' },
    { ...original, ciphertext: 'AA' }, { ...original, ciphertext: 'x'.repeat(180001) },
    { ...original, tokenSha256: 'X'.repeat(64) },
    independentEnvelope({}, reportToken),
    independentEnvelope(syntheticReport, reportToken, { plaintext: 'invalid JSON' }),
    independentEnvelope({ ...syntheticReport, reportedGrossCents: 1.5 }),
    independentEnvelope({ ...syntheticReport, beer: [{ pints: -1 }] }),
    independentEnvelope({ ...syntheticReport, title: 'x'.repeat(16001) }),
  ]) assert.equal(await openEventReport(envelope, reportToken), null);
  // Same decoded bytes as the valid key, but forbidden nonzero base64 padding bits.
  const noncanonical = reportToken.slice(0, -1) + 'd';
  assert.deepEqual(Buffer.from(noncanonical, 'base64url'), Buffer.from(reportToken, 'base64url'));
  assert.equal(await openEventReport({ ...original, tokenSha256: await tokenDigest(noncanonical) }, noncanonical), null);
  await assert.rejects(sealEventReport(syntheticReport, noncanonical));
  await assert.rejects(sealEventReport({}, reportToken));
});

test('offline CLI publishes only ciphertext and sanitizes errors containing private input', async () => {
  const directory = mkdtempSync(join(tmpdir(), 'tdf-report-seal-test-'));
  try {
    const input = join(directory, 'snapshot.json'), key = join(directory, 'key.json'), output = join(directory, 'sealed.mjs');
    writeFileSync(input, JSON.stringify(syntheticReport), { mode: 0o600 });
    writeFileSync(key, JSON.stringify({ token: reportToken }), { mode: 0o600 });
    const args = ['scripts/seal-event-report.mjs', '--input', input, '--token-file', key, '--output', output];
    const success = spawnSync(process.execPath, args, { encoding: 'utf8' });
    assert.equal(success.status, 0, success.stderr);
    const module = readFileSync(output, 'utf8');
    assert.ok(!module.includes(reportToken));
    assert.ok(!module.includes(syntheticReport.title));
    const { sealedEventReport } = await import(output);
    assert.deepEqual(await openEventReport(sealedEventReport, reportToken), syntheticReport);
    writeFileSync(input, 'PRIVATE_INPUT_SENTINEL invalid JSON');
    const denied = spawnSync(process.execPath, args, { encoding: 'utf8' });
    assert.equal(denied.status, 1);
    assert.doesNotMatch(denied.stdout + denied.stderr, /PRIVATE_INPUT_SENTINEL|snapshot\.json|key\.json/);
    assert.equal(readFileSync(output, 'utf8'), module);
    assert.equal(spawnSync(process.execPath, args.slice(0, -1).concat(key)).status, 1);
  } finally {
    rmSync(directory, { recursive: true });
  }
});
