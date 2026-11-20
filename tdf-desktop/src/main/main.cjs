'use strict';
// TDF Records desktop shell: hosts the production web application inside a
// hardened Electron window, bridges system media controls, validates deep
// links and manages shell updates. Business rules, permissions and data stay
// in the hosted web application and the TDF API.
const { app, BrowserWindow, Menu, clipboard, dialog, ipcMain, net, protocol, screen, session, shell } = require('electron');
const fs = require('node:fs');
const path = require('node:path');
const { INTERNAL_HOST, INTERNAL_SCHEME, loadChannel, sanitizeAppPath } = require('./config.cjs');
const { classifyNavigation, deepLinkFromArgv, describeForLog, isSafeExternalUrl, parseDeepLink } = require('./links.cjs');
const { PlaybackState, isCommand } = require('./playbackState.cjs');
const { createUpdater } = require('./updater.cjs');
const { buildMenu } = require('./menu.cjs');
const { createLogger } = require('./log.cjs');

const PRODUCT_NAME = 'TDF Records';
const INTERNAL_PAGES = Object.freeze({ 'offline.html': 'text/html', 'blocked.html': 'text/html' });
const ALLOWED_PERMISSIONS = new Set(['media', 'notifications', 'fullscreen', 'clipboard-sanitized-write', 'clipboard-read', 'speaker-selection']);
const SMOKE_TIMEOUT_MS = 90_000;
const RETRY_DELAYS_MS = [5_000, 10_000, 20_000, 40_000, 60_000];

const cliArguments = process.argv.slice(app.isPackaged ? 1 : 2);
const hasFlag = (name) => cliArguments.some((entry) => entry === `--${name}` || entry.startsWith(`--${name}=`));
const flagValue = (name) => {
  const entry = cliArguments.find((candidate) => candidate.startsWith(`--${name}=`));
  return entry ? entry.slice(name.length + 3) : null;
};

if (hasFlag('version')) {
  process.stdout.write(`${app.getVersion()}\n`);
  app.exit(0);
}

// Development and test runs may isolate their profile; packaged builds always
// use the per-user application data directory.
if (!app.isPackaged && process.env.TDF_DESKTOP_USER_DATA) {
  app.setPath('userData', path.resolve(process.env.TDF_DESKTOP_USER_DATA));
}

app.setName(PRODUCT_NAME);
// The hosted player resumes listening on launch; a desktop app is an explicit
// user gesture, so Chromium's autoplay gate adds nothing here.
app.commandLine.appendSwitch('autoplay-policy', 'no-user-gesture-required');

protocol.registerSchemesAsPrivileged([{
  scheme: INTERNAL_SCHEME,
  privileges: { standard: true, secure: true, supportFetchAPI: false, corsEnabled: false, stream: false },
}]);

const smokeMode = hasFlag('smoke');
const log = createLogger(app.getPath('logs'));
const playback = new PlaybackState();

let channel = null;
let channelError = null;
try {
  channel = loadChannel({
    isPackaged: app.isPackaged,
    resourcesPath: process.resourcesPath,
    channelsDir: path.join(__dirname, '..', '..', 'channels'),
  });
} catch (error) {
  channelError = error;
}

/** @type {BrowserWindow | null} */
let mainWindow = null;
let updater = null;
let lastAppPath = channel?.startPath ?? '/inicio';
let retryTimer = null;
let retryAttempt = 0;
let pendingDeepLinkPath = null;
let lastMainFrameHttpStatus = null;
const governedContents = new WeakSet();

const appUrl = (appPath) => `${channel.appOrigin}${sanitizeAppPath(appPath) ?? channel.startPath}`;
const internalUrl = (page, query = '') => `${INTERNAL_SCHEME}://${INTERNAL_HOST}/${page}${query}`;
const windowStateFile = () => path.join(app.getPath('userData'), 'window-state.json');

function readWindowState() {
  try {
    const saved = JSON.parse(fs.readFileSync(windowStateFile(), 'utf8'));
    if (!saved || typeof saved !== 'object') return null;
    const { x, y, width, height } = saved;
    if (![x, y, width, height].every(Number.isFinite) || width < 960 || height < 600) return null;
    const visible = screen.getAllDisplays().some((display) => {
      const area = display.workArea;
      return x >= area.x - 32 && y >= area.y - 32 && x < area.x + area.width && y < area.y + area.height;
    });
    return visible ? { x, y, width, height } : { width, height };
  } catch {
    return null;
  }
}

function saveWindowState(win) {
  try {
    if (win.isMinimized() || win.isFullScreen()) return;
    fs.writeFileSync(windowStateFile(), JSON.stringify(win.getNormalBounds()));
  } catch (error) {
    log.warn(`window state not saved: ${error.message}`);
  }
}

function clearRetry() {
  if (retryTimer) clearTimeout(retryTimer);
  retryTimer = null;
}

function scheduleRetry(contents) {
  clearRetry();
  const delay = RETRY_DELAYS_MS[Math.min(retryAttempt, RETRY_DELAYS_MS.length - 1)];
  retryAttempt += 1;
  retryTimer = setTimeout(() => {
    retryTimer = null;
    if (contents.isDestroyed()) return;
    if (!net.isOnline()) { scheduleRetry(contents); return; }
    log.info(`retrying hosted application (attempt ${retryAttempt})`);
    void loadApp(contents, lastAppPath);
  }, delay);
}

async function loadApp(contents, appPath) {
  const target = appUrl(appPath);
  lastAppPath = sanitizeAppPath(appPath) ?? channel.startPath;
  try {
    await contents.loadURL(target);
  } catch (error) {
    // did-fail-load handles the user-visible fallback; loadURL rejects for the
    // same failure. Its message quotes the full URL, so log the error code and
    // a redacted target instead.
    const reason = typeof error?.code === 'string' ? error.code : String(error?.message ?? error).split(' loading ')[0];
    log.warn(`loadURL rejected: ${reason} loading ${describeForLog(target)}`);
  }
}

function navigateTo(appPath) {
  const safePath = sanitizeAppPath(appPath);
  if (!safePath) return false;
  if (!mainWindow || mainWindow.isDestroyed()) {
    pendingDeepLinkPath = safePath;
    // Before readiness (macOS can deliver open-url early) the startup window
    // consumes the queued route. Afterwards the last window may have been
    // closed with the application still running (macOS): a menu action or a
    // deep link has to reopen the window itself, because only Dock activation
    // fires `activate`.
    if (app.isReady() && channel) createMainWindow();
    return true;
  }
  clearRetry();
  void loadApp(mainWindow.webContents, safePath);
  if (mainWindow.isMinimized()) mainWindow.restore();
  mainWindow.show();
  mainWindow.focus();
  return true;
}

function handleDeepLink(rawUrl) {
  const link = parseDeepLink(rawUrl, channel.appOrigin);
  if (!link) {
    log.warn(`rejected deep link ${describeForLog(rawUrl)}`);
    return false;
  }
  log.info(`deep link -> ${describeForLog(link.path)}`);
  return navigateTo(link.path);
}

function attachNavigationPolicy(contents) {
  if (governedContents.has(contents)) return;
  governedContents.add(contents);
  const decide = (event, url) => {
    const kind = classifyNavigation(url, channel.appOrigin);
    if (kind === 'app') {
      const parsed = new URL(url);
      lastAppPath = sanitizeAppPath(`${parsed.pathname}${parsed.search}${parsed.hash}`) ?? lastAppPath;
      return;
    }
    if (kind === 'internal') {
      if (new URL(url).pathname === '/retry') {
        event.preventDefault();
        clearRetry();
        retryAttempt = 0;
        void loadApp(contents, lastAppPath);
      }
      return;
    }
    event.preventDefault();
    if (kind === 'external') {
      log.info(`external navigation opened in the system browser: ${new URL(url).origin}`);
      void shell.openExternal(url);
    } else {
      log.warn(`blocked navigation ${describeForLog(url)}`);
    }
  };
  contents.on('will-navigate', decide);
  contents.on('will-redirect', decide);
  // The hosted application routes with the History API; those changes never
  // reach will-navigate, so track them here to keep recovery on the live route.
  contents.on('did-navigate-in-page', (_event, url, isMainFrame) => {
    if (!isMainFrame || classifyNavigation(url, channel.appOrigin) !== 'app') return;
    const parsed = new URL(url);
    lastAppPath = sanitizeAppPath(`${parsed.pathname}${parsed.search}${parsed.hash}`) ?? lastAppPath;
  });
  contents.setWindowOpenHandler(({ url }) => {
    const kind = classifyNavigation(url, channel.appOrigin);
    if (kind === 'external') {
      void shell.openExternal(url);
    } else if (kind === 'app') {
      return {
        action: 'allow',
        overrideBrowserWindowOptions: {
          autoHideMenuBar: true,
          webPreferences: {
            preload: path.join(__dirname, '..', 'preload', 'preload.cjs'),
            sandbox: true, contextIsolation: true, nodeIntegration: false, webviewTag: false,
          },
        },
      };
    } else {
      log.warn(`blocked popup ${describeForLog(url)}`);
    }
    return { action: 'deny' };
  });
  contents.on('did-fail-load', (_event, errorCode, errorDescription, validatedURL, isMainFrame) => {
    if (!isMainFrame || errorCode === -3) return; // -3: navigation superseded by a newer one
    if (classifyNavigation(validatedURL, channel.appOrigin) !== 'app') return;
    log.warn(`hosted application failed to load (${errorCode} ${errorDescription})`);
    void contents.loadURL(internalUrl('offline.html', `?code=${encodeURIComponent(String(errorCode))}`));
    scheduleRetry(contents);
  });
  contents.on('did-navigate', (_event, _url, httpResponseCode) => { lastMainFrameHttpStatus = httpResponseCode; });
  contents.on('did-finish-load', () => {
    if (classifyNavigation(contents.getURL(), channel.appOrigin) === 'app') {
      clearRetry();
      retryAttempt = 0;
    }
  });
  contents.on('render-process-gone', (_event, details) => {
    log.error(`renderer gone: ${details.reason}`);
    if (details.reason === 'clean-exit' || smokeMode) return;
    void dialog.showMessageBox({ type: 'error', title: PRODUCT_NAME, message: 'La ventana de TDF Records se cerró inesperadamente.',
      detail: `Motivo: ${details.reason}. Puedes recargar la aplicación; tu sesión se conserva.`, buttons: ['Recargar', 'Salir'], defaultId: 0, cancelId: 1 })
      .then(({ response }) => { if (response === 0) void loadApp(contents, lastAppPath); else app.quit(); });
  });
}

function configureSession(targetSession) {
  const originOf = (url) => { try { return new URL(url).origin; } catch { return null; } };
  targetSession.setPermissionRequestHandler((contents, permission, callback, details) => {
    const origin = originOf(details?.requestingUrl ?? contents.getURL());
    const allowed = origin === channel.appOrigin && ALLOWED_PERMISSIONS.has(permission);
    if (!allowed) log.warn(`permission ${permission} denied for ${origin ?? 'unknown origin'}`);
    callback(allowed);
  });
  targetSession.setPermissionCheckHandler((_contents, permission, requestingOrigin) =>
    originOf(requestingOrigin) === channel.appOrigin && ALLOWED_PERMISSIONS.has(permission));
  targetSession.setDevicePermissionHandler(() => false);
  targetSession.on('will-download', (_event, item) => {
    log.info(`download started: ${item.getFilename()} (${item.getTotalBytes()} bytes)`);
  });
}

function registerInternalProtocol() {
  protocol.handle(INTERNAL_SCHEME, async (request) => {
    const url = new URL(request.url);
    const page = url.pathname.replace(/^\/+/, '');
    const contentType = url.host === INTERNAL_HOST ? INTERNAL_PAGES[page] : undefined;
    if (!contentType) return new Response('Not found', { status: 404, headers: { 'Content-Type': 'text/plain' } });
    const body = await fs.promises.readFile(path.join(__dirname, '..', '..', 'internal', page));
    return new Response(body, {
      headers: {
        'Content-Type': `${contentType}; charset=utf-8`,
        'Content-Security-Policy': "default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline'; img-src data:",
        'X-Content-Type-Options': 'nosniff',
        'Cache-Control': 'no-store',
      },
    });
  });
}

function senderAuthorized(event) {
  const frameUrl = event.senderFrame?.url ?? '';
  return classifyNavigation(frameUrl, channel.appOrigin) === 'app';
}

function registerIpc() {
  ipcMain.on('tdf:playback-report', (event, report) => {
    if (!senderAuthorized(event)) { log.warn('playback report from an unauthorized frame ignored'); return; }
    if (!playback.update(report)) log.warn('malformed playback report ignored');
  });
  ipcMain.handle('tdf:open-external', async (event, url) => {
    if (!senderAuthorized(event) || !isSafeExternalUrl(url)) return false;
    await shell.openExternal(url);
    return true;
  });
}

function sendPlaybackCommand(command) {
  if (!isCommand(command) || !mainWindow || mainWindow.isDestroyed()) return;
  const contents = mainWindow.webContents;
  if (classifyNavigation(contents.getURL(), channel.appOrigin) !== 'app') return;
  contents.send('tdf:playback-command', command);
}

function describeUpdater(state) {
  switch (state.status) {
    case 'disabled': return state.reason === 'unsigned-test-build' ? 'desactivadas en compilaciones de prueba sin firma'
      : state.reason === 'development-run' ? 'desactivadas en ejecución de desarrollo' : 'desactivadas en este canal';
    case 'available': return `versión ${state.version} disponible para descargar`;
    case 'downloading': return `descargando ${state.percent ?? 0}%`;
    case 'downloaded': return `versión ${state.version} lista; se instala al reiniciar`;
    case 'up-to-date': return `al día (comprobado ${state.checkedAt ?? 'ahora'})`;
    case 'error': return `error: ${state.error}`;
    case 'checking': return 'comprobando…';
    default: return 'sin comprobar';
  }
}

function diagnosticLines() {
  return [
    `Versión de la app: ${app.getVersion()}${channel.unsigned ? ' (compilación de prueba sin firma)' : ''}`,
    `Canal: ${channel.name}`,
    `Web: ${channel.appOrigin}`,
    `API: ${channel.apiOrigin}`,
    `Electron ${process.versions.electron} · Chromium ${process.versions.chrome} · ${process.platform} ${process.arch}`,
    channel.sourceCommit ? `Commit: ${channel.sourceCommit}` : null,
    channel.builtAt ? `Compilado: ${channel.builtAt}` : null,
    `Actualizaciones: ${describeUpdater(updater.state)}`,
    `Diagnóstico: ${log.directory}`,
  ].filter(Boolean);
}

async function onUpdateAction() {
  const state = updater.state;
  if (state.status === 'disabled') return;
  if (state.status === 'downloaded') {
    const { response } = await dialog.showMessageBox(mainWindow, { type: 'question', title: PRODUCT_NAME,
      message: `Instalar TDF Records ${state.version ?? ''}`.trim(), detail: 'La aplicación se reiniciará para completar la actualización. Tu sesión se conserva.',
      buttons: ['Reiniciar ahora', 'Más tarde'], defaultId: 0, cancelId: 1 });
    if (response === 0) updater.installNow();
    return;
  }
  if (state.status === 'available') {
    const { response } = await dialog.showMessageBox(mainWindow, { type: 'question', title: PRODUCT_NAME,
      message: `Descargar TDF Records ${state.version ?? ''}`.trim(), detail: 'La descarga se verifica antes de instalarse. Si falla, la versión actual sigue funcionando.',
      buttons: ['Descargar', 'Más tarde'], defaultId: 0, cancelId: 1 });
    if (response === 0) await updater.download();
    return;
  }
  const result = await updater.check();
  if (result.status === 'up-to-date') {
    void dialog.showMessageBox(mainWindow, { type: 'info', title: PRODUCT_NAME, message: 'TDF Records está al día.', detail: `Versión instalada ${app.getVersion()}.`, buttons: ['Cerrar'] });
  } else if (result.status === 'error') {
    void dialog.showMessageBox(mainWindow, { type: 'warning', title: PRODUCT_NAME, message: 'No se pudo comprobar la actualización.', detail: `${result.error}\n\nLa versión instalada sigue funcionando. Consulta el procedimiento de recuperación en la documentación de escritorio.`, buttons: ['Cerrar'] });
  } else if (result.status === 'available') {
    await onUpdateAction();
  }
}

async function showAbout() {
  const lines = diagnosticLines();
  const buttons = ['Cerrar', 'Copiar diagnóstico'];
  if (updater.state.status !== 'disabled') buttons.push('Buscar actualizaciones');
  const { response } = await dialog.showMessageBox(mainWindow, {
    type: 'info', title: `Acerca de ${PRODUCT_NAME}`, message: PRODUCT_NAME, detail: lines.join('\n'), buttons, defaultId: 0, cancelId: 0,
  });
  if (response === 1) clipboard.writeText(lines.join('\n'));
  if (response === 2) await onUpdateAction();
}

async function clearLocalData() {
  // The menu stays available on macOS after the last window closed, so the
  // dialog may have no parent and the final navigation must be able to reopen
  // the window (navigateTo does both).
  const parent = mainWindow && !mainWindow.isDestroyed() ? mainWindow : null;
  const options = { type: 'warning', title: PRODUCT_NAME,
    message: '¿Borrar los datos locales de TDF Records en este equipo?',
    detail: 'Se eliminarán las cookies de sesión, la caché y los datos guardados por la aplicación en este dispositivo. Los datos del servidor no cambian; para cerrar la sesión en el servidor usa "Cerrar sesión" dentro de la aplicación.',
    buttons: ['Borrar y volver al inicio de sesión', 'Cancelar'], defaultId: 1, cancelId: 1 };
  const { response } = parent ? await dialog.showMessageBox(parent, options) : await dialog.showMessageBox(options);
  if (response !== 0) return;
  await session.defaultSession.clearStorageData();
  await session.defaultSession.clearCache();
  playback.reset();
  log.info('local data cleared by the user');
  navigateTo('/login');
}

function rebuildMenu() {
  Menu.setApplicationMenu(buildMenu({
    app, channel, playback: playback.get(), updater: updater.state,
    actions: {
      navigate: (appPath) => { navigateTo(appPath); },
      openCurrentInBrowser: () => {
        const current = mainWindow?.webContents.getURL();
        void shell.openExternal(current && classifyNavigation(current, channel.appOrigin) === 'app' ? current : appUrl(lastAppPath));
      },
      clearLocalData: () => { void clearLocalData(); },
      reload: () => {
        if (!mainWindow) return;
        // Reload what the user is looking at; lastAppPath is for offline recovery.
        const current = mainWindow.webContents.getURL();
        if (classifyNavigation(current, channel.appOrigin) === 'app') {
          const parsed = new URL(current);
          void loadApp(mainWindow.webContents, `${parsed.pathname}${parsed.search}${parsed.hash}`);
        } else {
          void loadApp(mainWindow.webContents, lastAppPath);
        }
      },
      playback: (command) => sendPlaybackCommand(command),
      showAbout: () => { void showAbout(); },
      onUpdateAction: () => { void onUpdateAction(); },
      openLogs: () => { void shell.openPath(log.directory); },
    },
  }));
}

function createMainWindow() {
  const saved = readWindowState();
  const win = new BrowserWindow({
    width: saved?.width ?? 1280,
    height: saved?.height ?? 820,
    ...(saved?.x !== undefined ? { x: saved.x, y: saved.y } : {}),
    minWidth: 960,
    minHeight: 600,
    show: false,
    title: PRODUCT_NAME,
    backgroundColor: '#0f172a',
    webPreferences: {
      preload: path.join(__dirname, '..', 'preload', 'preload.cjs'),
      sandbox: true,
      contextIsolation: true,
      nodeIntegration: false,
      nodeIntegrationInWorker: false,
      webviewTag: false,
      spellcheck: true,
      additionalArguments: [
        `--tdf-app-origin=${channel.appOrigin}`,
        `--tdf-app-version=${app.getVersion()}`,
        `--tdf-channel=${channel.name}`,
        `--tdf-unsigned=${channel.unsigned ? 'true' : 'false'}`,
      ],
    },
  });
  mainWindow = win;
  attachNavigationPolicy(win.webContents);
  win.on('page-title-updated', (event, title) => {
    event.preventDefault();
    const base = title && title.trim() ? title.trim() : PRODUCT_NAME;
    win.setTitle(channel.unsigned ? `${base} — prueba sin firma` : base);
  });
  win.once('ready-to-show', () => { if (!smokeMode) win.show(); });
  win.on('close', () => saveWindowState(win));
  win.on('closed', () => { mainWindow = null; clearRetry(); playback.reset(); });
  win.webContents.on('did-navigate', () => rebuildMenu());
  void loadApp(win.webContents, pendingDeepLinkPath ?? lastAppPath);
  pendingDeepLinkPath = null;
  return win;
}

async function runSmoke(win) {
  const receiptPath = flagValue('smoke-receipt') ?? path.join(app.getPath('userData'), 'smoke-receipt.json');
  const outcome = await new Promise((resolve) => {
    const timer = setTimeout(() => resolve({ ok: false, reason: 'timeout' }), SMOKE_TIMEOUT_MS);
    const finish = (value) => { clearTimeout(timer); resolve(value); };
    win.webContents.on('did-finish-load', () => {
      const url = win.webContents.getURL();
      const kind = classifyNavigation(url, channel.appOrigin);
      if (kind === 'app') {
        const httpStatus = lastMainFrameHttpStatus;
        const healthy = httpStatus === null || (httpStatus >= 200 && httpStatus < 400);
        finish({ ok: healthy, reason: healthy ? 'hosted-application-loaded' : `hosted-application-http-${httpStatus}`, url });
      } else if (kind === 'internal') finish({ ok: false, reason: 'offline-fallback-shown', url });
    });
  });
  const httpStatus = lastMainFrameHttpStatus;
  const receipt = {
    ok: outcome.ok,
    reason: outcome.reason,
    loadedUrl: outcome.url ?? null,
    httpStatus,
    title: win.isDestroyed() ? null : win.webContents.getTitle(),
    appVersion: app.getVersion(),
    channel: channel.name,
    appOrigin: channel.appOrigin,
    unsigned: channel.unsigned,
    electron: process.versions.electron,
    chromium: process.versions.chrome,
    platform: process.platform,
    arch: process.arch,
    packaged: app.isPackaged,
    executable: process.execPath,
    recordedAt: new Date().toISOString(),
  };
  fs.mkdirSync(path.dirname(receiptPath), { recursive: true });
  fs.writeFileSync(receiptPath, `${JSON.stringify(receipt, null, 2)}\n`);
  process.stdout.write(`${JSON.stringify(receipt)}\n`);
  app.exit(outcome.ok ? 0 : 1);
}

const singleInstance = app.requestSingleInstanceLock();
if (!singleInstance) {
  app.quit();
} else {
  app.on('second-instance', (_event, argv) => {
    const link = deepLinkFromArgv(argv, channel?.appOrigin ?? '');
    if (link) navigateTo(link.path);
    else if (mainWindow) { if (mainWindow.isMinimized()) mainWindow.restore(); mainWindow.focus(); }
  });
  app.on('open-url', (event, url) => {
    event.preventDefault();
    if (channel) handleDeepLink(url);
  });

  void app.whenReady().then(async () => {
    if (channelError) {
      log.error(`channel configuration invalid: ${channelError.message}`);
      if (smokeMode) {
        const receiptPath = flagValue('smoke-receipt') ?? path.join(app.getPath('userData'), 'smoke-receipt.json');
        fs.mkdirSync(path.dirname(receiptPath), { recursive: true });
        fs.writeFileSync(receiptPath, `${JSON.stringify({ ok: false, reason: `channel-invalid: ${channelError.message}`, appVersion: app.getVersion(), packaged: app.isPackaged, recordedAt: new Date().toISOString() }, null, 2)}\n`);
      } else {
        dialog.showErrorBox(PRODUCT_NAME, `La configuración de la aplicación no es válida y no se puede iniciar.\n\n${channelError.message}`);
      }
      app.exit(2);
      return;
    }
    log.info(`starting ${PRODUCT_NAME} ${app.getVersion()} channel=${channel.name} origin=${channel.appOrigin} packaged=${app.isPackaged} unsigned=${channel.unsigned}`);
    if (app.isPackaged) app.setAsDefaultProtocolClient(INTERNAL_SCHEME);
    let autoUpdater = null;
    try { ({ autoUpdater } = require('electron-updater')); } catch (error) { log.warn(`electron-updater unavailable: ${error.message}`); }
    updater = createUpdater({ channel, isPackaged: app.isPackaged, autoUpdater, log: (message) => log.warn(message) });
    updater.onChange(() => rebuildMenu());
    playback.onChange(() => rebuildMenu());
    configureSession(session.defaultSession);
    registerInternalProtocol();
    registerIpc();
    // Every web contents (main window, same-origin popups) gets one policy.
    app.on('web-contents-created', (_event, contents) => { attachNavigationPolicy(contents); });
    const initialLink = deepLinkFromArgv(process.argv, channel.appOrigin);
    if (initialLink) pendingDeepLinkPath = initialLink.path;
    const win = createMainWindow();
    rebuildMenu();
    if (smokeMode) { await runSmoke(win); return; }
    if (updater.state.status === 'idle') {
      setTimeout(() => { void updater.check().then((state) => {
        if (state.status === 'available') void onUpdateAction();
      }); }, 8_000);
    }
  });

  app.on('activate', () => { if (!mainWindow && channel) createMainWindow(); });
  app.on('window-all-closed', () => { if (process.platform !== 'darwin' || smokeMode) app.quit(); });
}
