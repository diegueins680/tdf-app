'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { EventEmitter } = require('node:events');
const { createUpdater, reasonForDisabled } = require('../src/main/updater.cjs');

const channel = (overrides = {}) => ({ name: 'production', unsigned: false, updates: { enabled: true, url: 'https://example.com/feed', channel: 'latest' }, ...overrides });

function fakeAutoUpdater() {
  const emitter = new EventEmitter();
  emitter.feed = null;
  emitter.setFeedURL = (feed) => { emitter.feed = feed; };
  emitter.checkForUpdates = async () => { emitter.emit('checking-for-update'); };
  emitter.downloadUpdate = async () => { emitter.emit('download-progress', { percent: 42.4 }); };
  emitter.quitAndInstall = () => { emitter.installed = true; };
  return emitter;
}

test('updates are disabled for development runs, unsigned test builds and channels without updates', () => {
  assert.equal(reasonForDisabled({ channel: channel(), isPackaged: false }), 'development-run');
  assert.equal(reasonForDisabled({ channel: channel({ unsigned: true }), isPackaged: true }), 'unsigned-test-build');
  assert.equal(reasonForDisabled({ channel: channel({ updates: { enabled: false } }), isPackaged: true }), 'channel-updates-off');
  assert.equal(reasonForDisabled({ channel: channel(), isPackaged: true }), null);
  const disabled = createUpdater({ channel: channel({ unsigned: true }), isPackaged: true, autoUpdater: fakeAutoUpdater() });
  assert.equal(disabled.state.status, 'disabled');
  assert.equal(disabled.state.reason, 'unsigned-test-build');
  assert.equal(disabled.installNow(), false);
});

test('a signed packaged build follows check -> available -> download -> downloaded -> install', async () => {
  const auto = fakeAutoUpdater();
  const updater = createUpdater({ channel: channel(), isPackaged: true, autoUpdater: auto });
  const transitions = [];
  updater.onChange((state) => transitions.push(state.status));
  assert.deepEqual(auto.feed, { provider: 'generic', url: 'https://example.com/feed', channel: 'latest' });
  assert.equal(auto.autoDownload, false);
  assert.equal(auto.autoInstallOnAppQuit, true);
  await updater.check();
  auto.emit('update-available', { version: '0.2.0' });
  assert.equal(updater.state.status, 'available');
  assert.equal(updater.installNow(), false, 'nothing to install before a verified download');
  await updater.download();
  auto.emit('update-downloaded', { version: '0.2.0' });
  assert.equal(updater.state.status, 'downloaded');
  assert.equal(updater.installNow(), true);
  assert.equal(auto.installed, true);
  assert.deepEqual(transitions, ['checking', 'available', 'downloading', 'downloading', 'downloaded']);
});

test('download or feed failures leave the installed version in charge and are reported', async () => {
  const auto = fakeAutoUpdater();
  auto.checkForUpdates = async () => { throw new Error('feed unreachable'); };
  const updater = createUpdater({ channel: channel(), isPackaged: true, autoUpdater: auto, log: () => undefined });
  await updater.check();
  assert.equal(updater.state.status, 'error');
  assert.match(updater.state.error, /feed unreachable/);
  assert.equal(updater.installNow(), false);
  auto.emit('update-not-available', { version: '0.1.0' });
  assert.equal(updater.state.status, 'up-to-date');
});

test('a missing electron-updater module disables updates without throwing', () => {
  const updater = createUpdater({ channel: channel(), isPackaged: true, autoUpdater: null });
  assert.equal(updater.state.status, 'disabled');
  assert.equal(updater.state.reason, 'updater-unavailable');
});
