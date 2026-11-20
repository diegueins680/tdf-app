'use strict';
// Shell update channel. The hosted web application updates itself on deploy;
// this only governs the native shell (Electron, menus, installer).
//
// States: disabled | idle | checking | available | downloading | downloaded | up-to-date | error
// Downloads never start without an explicit user decision; installation happens
// on quit (or immediately when the user confirms), so a failed download leaves
// the installed version untouched. Recovery guidance lives in docs/desktop.
const STATES = new Set(['disabled', 'idle', 'checking', 'available', 'downloading', 'downloaded', 'up-to-date', 'error']);

function reasonForDisabled({ channel, isPackaged }) {
  if (!isPackaged) return 'development-run';
  if (channel.unsigned) return 'unsigned-test-build';
  if (!channel.updates.enabled) return 'channel-updates-off';
  return null;
}

/**
 * @param {object} options
 * @param {object} options.channel normalized channel
 * @param {boolean} options.isPackaged
 * @param {object|null} options.autoUpdater electron-updater instance (null when unavailable)
 * @param {(message: string) => void} options.log
 */
function createUpdater({ channel, isPackaged, autoUpdater, log = () => undefined }) {
  const listeners = new Set();
  const disabledReason = reasonForDisabled({ channel, isPackaged }) ?? (autoUpdater ? null : 'updater-unavailable');
  let state = { status: disabledReason ? 'disabled' : 'idle', reason: disabledReason, version: null, percent: null, error: null, checkedAt: null };

  const emit = (patch) => {
    state = { ...state, ...patch };
    if (!STATES.has(state.status)) throw new Error(`Invalid updater state ${state.status}`);
    for (const listener of listeners) listener(state);
  };

  if (!disabledReason) {
    autoUpdater.autoDownload = false;
    autoUpdater.autoInstallOnAppQuit = true;
    autoUpdater.allowDowngrade = false;
    autoUpdater.setFeedURL({ provider: 'generic', url: channel.updates.url, channel: channel.updates.channel });
    autoUpdater.on('checking-for-update', () => emit({ status: 'checking', error: null }));
    autoUpdater.on('update-available', (info) => emit({ status: 'available', version: info?.version ?? null, checkedAt: new Date().toISOString() }));
    autoUpdater.on('update-not-available', (info) => emit({ status: 'up-to-date', version: info?.version ?? null, checkedAt: new Date().toISOString() }));
    autoUpdater.on('download-progress', (progress) => emit({ status: 'downloading', percent: Math.round(progress?.percent ?? 0) }));
    autoUpdater.on('update-downloaded', (info) => emit({ status: 'downloaded', version: info?.version ?? state.version, percent: 100 }));
    autoUpdater.on('error', (error) => {
      log(`updater error: ${error?.message ?? error}`);
      emit({ status: 'error', error: error?.message ?? String(error) });
    });
  }

  return {
    get state() { return state; },
    onChange(listener) { listeners.add(listener); return () => listeners.delete(listener); },
    async check() {
      if (disabledReason) return state;
      if (state.status === 'checking' || state.status === 'downloading') return state;
      try { await autoUpdater.checkForUpdates(); } catch (error) { emit({ status: 'error', error: error?.message ?? String(error) }); }
      return state;
    },
    async download() {
      if (disabledReason || state.status !== 'available') return state;
      emit({ status: 'downloading', percent: 0 });
      try { await autoUpdater.downloadUpdate(); } catch (error) { emit({ status: 'error', error: error?.message ?? String(error) }); }
      return state;
    },
    installNow() {
      if (disabledReason || state.status !== 'downloaded') return false;
      autoUpdater.quitAndInstall(false, true);
      return true;
    },
  };
}

module.exports = { STATES, createUpdater, reasonForDisabled };
