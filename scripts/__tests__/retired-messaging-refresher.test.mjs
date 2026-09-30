import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
for (const args of [[], ['--auto']]) {
  test(`retired refresher refuses ${args.length ? 'automatic' : 'manual'} token exchange without disclosing credentials`, () => {
    const secret = 'SENSITIVE_TEST_VALUE_93457';
    const result = spawnSync(process.execPath, ['scripts/refresh-messaging-token.mjs', ...args], {
      encoding: 'utf8', env: { ...process.env, FACEBOOK_USER_TOKEN: secret, FACEBOOK_APP_SECRET: secret, FLY_APP_NAME: secret },
    });
    assert.equal(result.status, 1);
    assert.equal(result.stdout, '');
    assert.match(result.stderr, /refresher is retired/);
    assert.match(result.stderr, /ops\/hetzner\/README.md/);
    assert.doesNotMatch(result.stderr, new RegExp(secret));
    assert.doesNotMatch(result.stderr, /flyctl|secrets set/);
  });
}

test('legacy Instagram diagnostic refuses without exposing supplied token prefixes', () => {
  const secret = 'SENSITIVE_INSTAGRAM_CANARY';
  const result = spawnSync(process.execPath, ['scripts/diagnose-instagram.mjs'], {
    encoding: 'utf8', env: { ...process.env, INSTAGRAM_MESSAGING_TOKEN: secret, INSTAGRAM_MESSAGING_ACCOUNT_ID: 'test' },
  });
  assert.equal(result.status, 1);
  assert.equal(result.stdout, '');
  assert.match(result.stderr, /check-messaging-token.mjs/);
  assert.doesNotMatch(result.stderr, /SENSITIVE_|flyctl/);
});
