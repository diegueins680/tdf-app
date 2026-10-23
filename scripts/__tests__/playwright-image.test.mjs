import assert from 'node:assert/strict';
import test from 'node:test';
import {
  validateImage, validateHomeOwner, launchBrowsers, packages, browsers,
} from '../verify-playwright-image.mjs';

test('accepts the CI browser user owning its mounted home', () => {
  assert.doesNotThrow(() => validateHomeOwner(1001, 1001));
});

test('rejects the root process and regular-user home mismatch observed in Linux CI', () => {
  assert.throws(() => validateHomeOwner(0, 1001), /Browser user must own its home directory/);
});

test('rejects missing or invalid user/home ownership', () => {
  assert.throws(() => validateHomeOwner(undefined, 1001), /must be known/);
  assert.throws(() => validateHomeOwner(1001, undefined), /must be known/);
  assert.throws(() => validateHomeOwner(-1, 1001), /must be known/);
});

function fixture() {
  const version = '1.59.1';
  return {
    image: 'mcr.microsoft.com/playwright:v' + version + '-noble@sha256:' + 'a'.repeat(64),
    locked: Object.fromEntries(packages.map(name => [name, version])),
    installed: Object.fromEntries(packages.map(name => [name, version])),
    metadata: { driverVersion: version, dockerImageName: 'mcr.microsoft.com/playwright:v' + version + '-noble' },
    browserPath: '/ms-playwright',
  };
}

test('accepts aligned immutable image, lockfile, installed packages and bundled metadata', () => {
  assert.equal(validateImage(fixture()), '1.59.1');
});

for (const name of packages) {
  test('rejects a lockfile upgrade without a matching image: ' + name, () => {
    const value = fixture();
    value.locked[name] = '1.60.0';
    assert.throws(() => validateImage(value), /Image and lockfile differ/);
  });
  test('rejects a stale installed dependency: ' + name, () => {
    const value = fixture();
    value.installed[name] = '1.58.0';
    assert.throws(() => validateImage(value), /Image and installed package differ/);
  });
}

test('rejects mutable tags, foreign registries, malformed digests and image-version drift', () => {
  for (const image of [
    'mcr.microsoft.com/playwright:v1.59.1-noble',
    'example.test/playwright:v1.59.1-noble@sha256:' + 'a'.repeat(64),
    'mcr.microsoft.com/playwright:v1.59.1-noble@sha256:short',
    fixture().image.replace('1.59.1', '1.60.0'),
  ]) assert.throws(() => validateImage({ ...fixture(), image }));
});

test('rejects stale or absent bundled metadata and redirected browser directories', () => {
  for (const change of [
    { metadata: undefined },
    { metadata: { ...fixture().metadata, driverVersion: '1.58.0' } },
    { metadata: { ...fixture().metadata, dockerImageName: 'foreign-image' } },
    { browserPath: '/tmp/foreign-browsers' },
  ]) assert.throws(() => validateImage({ ...fixture(), ...change }));
});

test('launches and closes every required browser before returning a complete result', async () => {
  const events = [];
  const types = Object.fromEntries(browsers.map(name => [name, {
    launch: async options => {
      assert.deepEqual(options, { headless: true });
      events.push('launch:' + name);
      return { version: () => 'fixture', close: async () => events.push('close:' + name) };
    },
  }]));
  assert.deepEqual((await launchBrowsers(types)).map(x => x.browser), ['chromium', 'firefox', 'webkit']);
  assert.deepEqual(events, [
    'launch:chromium', 'close:chromium', 'launch:firefox', 'close:firefox', 'launch:webkit', 'close:webkit',
  ]);
});

test('a browser launch failure rejects instead of emitting a partial success', async () => {
  const closed = [];
  const types = {
    chromium: { launch: async () => ({ version: () => 'fixture', close: async () => closed.push('chromium') }) },
    firefox: { launch: async () => { throw new Error('missing Firefox executable'); } },
    webkit: { launch: async () => { throw new Error('must not reach WebKit'); } },
  };
  await assert.rejects(launchBrowsers(types), /missing Firefox executable/);
  assert.deepEqual(closed, ['chromium']);
});

test('closes a launched browser when reading its runtime version fails', async () => {
  let closed = false;
  const types = { chromium: { launch: async () => ({
    version: () => { throw new Error('browser disconnected'); },
    close: async () => { closed = true; },
  }) } };
  await assert.rejects(launchBrowsers(types), /browser disconnected/);
  assert.equal(closed, true);
});
