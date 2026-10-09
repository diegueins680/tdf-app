import assert from 'node:assert/strict';
import test from 'node:test';
import base from '../../playwright.config.mjs';
import native from '../../playwright.music-native.config.mjs';

test('general browser command excludes suites that need dedicated infrastructure', () => {
  for (const file of ['music-player-native.spec.mjs', 'music-player-integration.spec.mjs']) {
    assert.ok(base.testIgnore.some((pattern) => pattern.test(file)));
  }
  assert.ok(!base.testIgnore.some((pattern) => pattern.test('music-player.spec.mjs')));
});

test('dedicated configs restore their complete test selection with no inherited ignores', async () => {
  const keys = { TDF_MUSIC_BROWSER_API: 'http://127.0.0.1:19099',
    TDF_MUSIC_BROWSER_STORAGE: 'https://127.0.0.1:19000', PLAYWRIGHT_PORT: '4187' };
  const previous = Object.fromEntries(Object.keys(keys).map((key) => [key, process.env[key]]));
  try {
    Object.assign(process.env, keys);
    const { default: integrated } = await import('../../playwright.music-integration.config.mjs');
    assert.deepEqual(native.testIgnore, []);
    assert.ok(native.testMatch.test('music-player-native.spec.mjs'));
    assert.deepEqual(integrated.testIgnore, []);
    assert.ok(integrated.testMatch.test('music-player-integration.spec.mjs'));
    assert.equal(integrated.webServer.reuseExistingServer, false);
  } finally {
    for (const [key, value] of Object.entries(previous)) {
      if (value === undefined) delete process.env[key]; else process.env[key] = value;
    }
  }
});
