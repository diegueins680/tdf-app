'use strict';
// Preload: the only code that runs with access to Electron IPC inside a page.
// It exposes a small, frozen bridge to the hosted TDF application and nothing
// else. The bridge appears only on the authorized application origin, and the
// main process re-validates the sender of every message it receives.
const { contextBridge, ipcRenderer } = require('electron');

const argument = (name) => {
  const prefix = `--${name}=`;
  const match = process.argv.find((entry) => entry.startsWith(prefix));
  return match ? match.slice(prefix.length) : '';
};

const appOrigin = argument('tdf-app-origin');
const authorized = appOrigin !== '' && window.location.origin === appOrigin;

if (authorized) {
  const commandListeners = new Set();
  ipcRenderer.on('tdf:playback-command', (_event, command) => {
    if (typeof command !== 'string') return;
    for (const listener of commandListeners) {
      try { listener(command); } catch { /* listener errors stay in the page */ }
    }
  });

  const version = Object.freeze({
    app: argument('tdf-app-version'),
    electron: process.versions.electron,
    chromium: process.versions.chrome,
    platform: process.platform,
    arch: process.arch,
    channel: argument('tdf-channel'),
    unsigned: argument('tdf-unsigned') === 'true',
  });

  contextBridge.exposeInMainWorld('tdfDesktop', Object.freeze({
    version,
    playback: Object.freeze({
      report(state) {
        if (!state || typeof state !== 'object') return;
        ipcRenderer.send('tdf:playback-report', {
          owner: state.owner ?? null,
          status: String(state.status ?? 'idle'),
          title: state.title == null ? null : String(state.title),
          artist: state.artist == null ? null : String(state.artist),
          canNext: state.canNext === true,
          canPrevious: state.canPrevious === true,
        });
      },
      onCommand(listener) {
        if (typeof listener !== 'function') return () => undefined;
        commandListeners.add(listener);
        return () => { commandListeners.delete(listener); };
      },
    }),
    links: Object.freeze({
      openExternal: (url) => ipcRenderer.invoke('tdf:open-external', String(url)),
    }),
  }));
}

// A file dropped outside an application drop zone must never replace the
// application with the file (Chromium's default navigation).
window.addEventListener('dragover', (event) => { if (!event.defaultPrevented) event.preventDefault(); });
window.addEventListener('drop', (event) => { if (!event.defaultPrevented) event.preventDefault(); });
