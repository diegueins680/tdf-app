import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import test from 'node:test';

test('payment readiness reports actual health and never claims an unperformed payment test', () => {
  const dir = mkdtempSync(path.join(tmpdir(), 'tdf-payment-readiness-'));
  try {
    writeFileSync(path.join(dir, 'curl'), '#!/bin/sh\nprintf "%s" "$TEST_HEALTH_CODE"\nexit "${TEST_CURL_EXIT:-0}"\n', { mode: 0o700 });
    for (const [code, curlExit, expected] of [['200', '0', 0], ['503', '0', 1], ['', '7', 1]]) {
      const result = spawnSync('bash', ['scripts/test-stripe-integration.sh'], {
        encoding: 'utf8', timeout: 10000,
        env: { PATH: dir + ':' + process.env.PATH, PROJECT_DIR: dir, TEST_HEALTH_CODE: code, TEST_CURL_EXIT: curlExit },
      });
      assert.equal(result.error, undefined);
      assert.equal(result.status, expected, result.stderr);
      assert.match(result.stdout, /https:\/\/api\.tdfrecords\.net\/social-events\/stripe\/webhook/);
      assert.match(result.stdout, /payment\/webhook behavior: not exercised/);
      assert.doesNotMatch(result.stdout, /Health endpoint: Working|Stripe module: Compiled|flyctl/);
    }
  } finally { rmSync(dir, { recursive: true, force: true }); }
});
