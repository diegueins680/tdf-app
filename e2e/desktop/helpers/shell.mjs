import { _electron as electron } from '@playwright/test';
import { mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { createRequire } from 'node:module';
import { tmpdir } from 'node:os';
import path from 'node:path';

const require = createRequire(import.meta.url);
const root = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', '..', '..');
export const DESKTOP_DIR = path.join(root, 'tdf-desktop');
export const API_ORIGIN = 'http://127.0.0.1:19099';

/** Electron executable from the desktop package, not from any global install. */
export function electronExecutable() {
  return require(path.join(DESKTOP_DIR, 'node_modules', 'electron'));
}

/**
 * Launches the unpackaged shell with an isolated user-data directory against
 * the loopback web build. Returns the app, its first window and a disposer.
 */
export async function launchShell({ appOrigin, args = [], env = {} } = {}) {
  const userData = env.TDF_DESKTOP_USER_DATA ?? mkdtempSync(path.join(tmpdir(), 'tdf-desktop-e2e-'));
  const app = await electron.launch({
    executablePath: electronExecutable(),
    args: [DESKTOP_DIR, '--no-sandbox', ...args],
    env: {
      ...process.env,
      // The journeys exercise the loopback "local" channel even when the
      // surrounding CI job prepares the production channel for packaging.
      TDF_DESKTOP_CHANNEL: 'local',
      TDF_DESKTOP_APP_URL: appOrigin,
      TDF_DESKTOP_API_URL: API_ORIGIN,
      ELECTRON_DISABLE_SECURITY_WARNINGS: 'true',
      ...env,
      TDF_DESKTOP_USER_DATA: userData,
    },
    timeout: 60_000,
  });
  const window = await app.firstWindow({ timeout: 60_000 });
  // The shell starts on the public welcome route; journeys that need the
  // sign-in form go there explicitly, as a user would from the welcome page.
  const openLogin = async () => {
    await window.goto(`${appOrigin}/login`, { waitUntil: 'domcontentloaded' });
    await window.getByRole('button', { name: 'Ingresar', exact: true }).waitFor({ timeout: 30_000 });
  };
  const close = async () => {
    try { await app.close(); } catch { /* already closed */ }
    rmSync(userData, { recursive: true, force: true });
  };
  const readLog = () => {
    try { return readFileSync(path.join(userData, 'logs', 'tdf-desktop.log'), 'utf8'); } catch { return ''; }
  };
  return { app, window, userData, close, readLog, openLogin };
}

/** Labels of the native "Reproducción" menu as the operating system shows them. */
export async function playbackMenu(app) {
  return app.evaluate(({ Menu }) => {
    const menu = Menu.getApplicationMenu();
    const submenu = menu?.items.find((item) => item.label === 'Reproducción')?.submenu;
    return submenu ? submenu.items.filter((item) => item.type !== 'separator').map((item) => ({ label: item.label, enabled: item.enabled })) : null;
  });
}

/** Clicks any native menu entry by top-level menu label and item label. */
export async function clickMenuItem(app, menuLabel, itemLabel) {
  return app.evaluate(({ Menu }, [wantedMenu, wantedItem]) => {
    const submenu = Menu.getApplicationMenu()?.items.find((item) => item.label === wantedMenu)?.submenu;
    const item = submenu?.items.find((entry) => entry.label === wantedItem);
    if (!item || !item.enabled) return false;
    item.click();
    return true;
  }, [menuLabel, itemLabel]);
}

/** Clicks a native playback menu entry by label, as a user would from the menu bar. */
export async function clickPlaybackMenu(app, label) {
  return app.evaluate(({ Menu }, wanted) => {
    const submenu = Menu.getApplicationMenu()?.items.find((item) => item.label === 'Reproducción')?.submenu;
    const item = submenu?.items.find((entry) => entry.label === wanted);
    if (!item || !item.enabled) return false;
    item.click();
    return true;
  }, label);
}
