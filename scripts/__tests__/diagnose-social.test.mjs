import assert from 'node:assert/strict';
import { existsSync, mkdtempSync, readFileSync, writeFileSync, rmSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const script = fileURLToPath(new URL('../diagnose-social.mjs', import.meta.url));
const secrets = ['fixture-app-secret/+?=', 'fixture-instagram-secret/+?=',
  'fixture-facebook-secret/+?=', 'fixture-verification-secret/+?='];
const loader = `
import { appendFileSync } from 'node:fs';
globalThis.fetch = async (input, options = {}) => {
  const url = new URL(input);
  appendFileSync(process.env.REQUESTS, JSON.stringify({ method: options.method || 'GET',
    redirect: options.redirect, bounded: Boolean(options.signal), path: url.pathname }) + '\\n');
  if (process.env.MODE === 'throw') throw new Error(process.env.FACEBOOK_APP_SECRET);
  if (process.env.MODE === 'malformed') return { json: async () => { throw new Error(input); } };
  if (url.pathname.endsWith('/subscriptions')) return { json: async () => ({ data:
    process.env.MODE === 'missing' ? [] : [
      { object: 'instagram', active: process.env.MODE !== 'inactive', callback_url: process.env.MODE === 'wrong-callback'
        ? 'https://example.test/webhook' : 'https://api.tdfrecords.net/instagram/webhook' },
      { object: 'page', active: true, callback_url: 'https://api.tdfrecords.net/facebook/webhook' },
    ] }) };
  if (url.pathname.endsWith('/debug_token')) return { json: async () => process.env.MODE === 'rejected'
    ? { error: { code: 190, message: process.env.INSTAGRAM_MESSAGING_TOKEN } }
    : { data: { is_valid: process.env.MODE !== 'invalid', app_id: 'fixture-app', type: 'PAGE',
      scopes: [process.env.FACEBOOK_MESSAGING_TOKEN, encodeURIComponent(process.env.FACEBOOK_APP_SECRET)], expires_at: 1900000000 } } };
  return { json: async () => ({ username: process.env.INSTAGRAM_VERIFY_TOKEN }) };
};
`;

function run(mode) {
  const dir = mkdtempSync(path.join(os.tmpdir(), 'tdf-social-diagnostic-'));
  try {
    const fixture = path.join(dir, 'fetch.mjs');
    const requests = path.join(dir, 'requests.jsonl');
    writeFileSync(fixture, loader);
    const result = spawnSync(process.execPath, ['--import', fixture, script], {
      encoding: 'utf8', timeout: 10_000,
      env: { MODE: mode, REQUESTS: requests, FACEBOOK_APP_ID: 'fixture-app',
        FACEBOOK_GRAPH_BASE: mode === 'wrong-host' ? 'https://example.test/v25.0' : 'https://graph.facebook.com/v25.0',
        FACEBOOK_APP_SECRET: secrets[0], INSTAGRAM_MESSAGING_TOKEN: secrets[1],
        FACEBOOK_MESSAGING_TOKEN: secrets[2], INSTAGRAM_VERIFY_TOKEN: secrets[3],
        INSTAGRAM_MESSAGING_ACCOUNT_ID: 'fixture-instagram', FACEBOOK_PAGE_ID: 'fixture-page' },
    });
    assert.ifError(result.error);
    const output = result.stdout + result.stderr;
    for (const secret of secrets.flatMap(value => [value, encodeURIComponent(value)])) {
      assert.ok(!output.includes(secret), 'configured credential leaked');
    }
    const calls = existsSync(requests) ? readFileSync(requests, 'utf8').trim().split('\n').map(JSON.parse) : [];
    if (mode === 'wrong-host') assert.equal(calls.length, 0);
    else assert.ok(calls.length > 0);
    assert.ok(calls.every(call => call.method === 'GET' && call.redirect === 'error' && call.bounded));
    assert.ok(calls.every(call => !call.path.endsWith('/messages')));
    return { ...result, output, calls };
  } finally { rmSync(dir, { recursive: true, force: true }); }
}

test('successful diagnostic performs only bounded reads and redacts reflected credentials', () => {
  const result = run('valid');
  assert.equal(result.status, 0);
  assert.match(result.output, /All checks passed/);
  assert.match(result.output, /Message delivery not tested/);
});
test('missing subscriptions produce safe canonical guidance and failing exit status', () => {
  const result = run('missing');
  assert.equal(result.status, 1);
  assert.match(result.output, /https:\/\/api.tdfrecords.net\/instagram\/webhook/);
  assert.match(result.output, /ops\/hetzner\/README.md/);
  assert.doesNotMatch(result.output, /flyctl|curl -X POST|All checks passed/);
});
for (const mode of ['rejected', 'invalid', 'inactive', 'wrong-callback', 'wrong-host', 'throw', 'malformed']) {
  test(`${mode} provider result fails without credentials or a false pass`, () => {
    const result = run(mode);
    assert.equal(result.status, 1);
    assert.doesNotMatch(result.output, /All checks passed/);
  });
}
