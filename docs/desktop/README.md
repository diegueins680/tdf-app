# TDF App Desktop

Status: implemented for review (2026-11-08). Installed-platform acceptance on
macOS and Windows is produced by the `Desktop app` workflow and by the manual
plan below; it is not established by this document.

TDF App Desktop is an Electron shell (`tdf-desktop/`) that hosts the canonical
web application at `https://www.tdfrecords.net` and talks to
`https://api.tdfrecords.net` with the same session, permissions and business
rules as the browser. The decision and alternatives are in
[ADR 0132](../adr/0132-desktop-shell-electron.md). The requirement contract is
[formal/system/desktop-shell.md](../../formal/system/desktop-shell.md).

## Compatibility matrix

| Platform | Architectures | Minimum OS | Package | Status |
| --- | --- | --- | --- | --- |
| macOS | x64 (Intel), arm64 (Apple silicon) | macOS 12 Monterey or later (Electron 45 / Chromium 156 runtime floor) | `TDF-Records-<version>-mac-<arch>.dmg` and `.zip` (the zip feeds updates) | Built in CI on `macos-15`; installed smoke on arm64 runner; manual journey pending until a signed build exists |
| Windows | x64, arm64 | Windows 10 version 1809 or later, Windows 11 | `TDF-Records-<version>-win-<arch>.exe` (NSIS, per-user, custom directory allowed) | Built in CI on `windows-2025`; silent install, installed smoke and silent uninstall on x64 runner; manual journey pending |
| Linux | x64 | Any distribution with GTK 3 | AppImage, verification only | Electron journeys (Playwright + Xvfb) run here in CI and locally; not a distribution target |

Runtime versions are pinned in `tdf-desktop/package.json` (Electron 45.2.3,
electron-builder 26.18.0, electron-updater 6.8.11). Node 22 builds the shell.
The matrix is "verified" only where the workflow receipts exist; see
[verification report](verification-2026-11-08.md).

## Build and run

```bash
cd tdf-desktop
npm ci
npm test
```

Development run against a local web and API:

```bash
cd tdf-desktop
TDF_DESKTOP_APP_URL=http://localhost:5173 TDF_DESKTOP_API_URL=http://localhost:8080 npm start
```

Unpackaged runs are always marked "prueba sin firma", disable shell updates and
accept `http://` only for loopback hosts. Packaged builds ignore these
variables and read the embedded `channel.json`.

Installers. `scripts/prepare-channel.cjs` and `electron-builder.config.cjs`
resolve one signing decision from the environment (`scripts/signing.cjs`):
without `CSC_LINK`/`CSC_NAME` (macOS) or `WIN_CSC_LINK`/Azure Trusted Signing
(Windows) the package is an unsigned test build, its embedded `channel.json`
says `unsigned: true` and its file name carries `UNSIGNED-TEST`.
`TDF_DESKTOP_UNSIGNED=1` forces that even when a certificate is available, and
electron-builder refuses a `channel.json` whose flag disagrees with the
environment it runs in:

```bash
cd tdf-desktop
TDF_DESKTOP_UNSIGNED=1 npm run dist:mac
```

```bash
cd tdf-desktop
TDF_DESKTOP_UNSIGNED=1 npm run dist:win
```

Each installer embeds `channel.json` with the hosted origin, API origin,
update feed, source commit, build time and the unsigned flag. The About dialog,
`--version` and `--smoke --smoke-receipt=<file>` expose them.

## Security model

- The renderer runs sandboxed with context isolation, no Node integration, no
  `<webview>`. The preload exposes a frozen `window.tdfDesktop` only when the
  page origin equals the channel origin; the main process re-checks
  `senderFrame.url` on every IPC message.
- Top-level navigation and popups are classified before they happen: the hosted
  origin and bundled internal pages are allowed, `http(s)` destinations open in
  the system browser, every other scheme (`file:`, `javascript:`, `data:`, foreign
  custom schemes) is blocked and logged.
- Deep links `tdf-desktop://open?path=/ruta` (and `?path=` form) are parsed
  into an in-app path only when the path is absolute, single-slash, free of
  traversal, encoded separators and control characters; everything else is
  rejected and logged before any navigation.
- Permissions: only `media`, `notifications`, `fullscreen`, clipboard write/read
  and speaker selection for the hosted origin; device (USB/HID/serial) access is
  denied.
- Credentials: the shell adds no storage of its own. The TDF session cookie and
  the web application's local state live in Chromium's profile, encrypted at
  rest with the operating system's mechanism (Keychain on macOS, DPAPI on
  Windows). "Cerrar sesión y borrar datos locales" clears cookies, cache and
  storage on the device; server-side sign-out remains the application's
  "Cerrar sesión".
- Diagnostics log (`tdf-desktop.log` in the user logs directory) records
  navigation decisions and update states only; never request bodies, tokens or
  page content.
- The diagnostics log (`tdf-desktop.log`) records deep links, blocked
  navigations and failed loads without query strings, fragments or embedded
  credentials, so invitation or password-reset tokens carried by a link never
  persist in log files.

## Update channel

- Provider: `electron-updater` generic feed at
  `https://github.com/diegueins680/tdf-app/releases/download/desktop-latest/`
  (`latest-mac.yml`, `latest.yml`, zip/exe/blockmap). Each version also lives in
  the immutable release `desktop-v<version>`.
- Publication happens only when a `desktop-v<version>` tag is pushed and only
  with signed artifacts: the release job fails unless the macOS certificate and
  the three Apple notarization credentials are configured, and it refuses
  `UNSIGNED-TEST` files.
- Behavior: silent check 8 s after launch and on demand from the menu
  ("Buscar actualizaciones…"). Downloads start only after the user accepts; the
  package signature/hash is verified by electron-updater before installation;
  installation happens on quit or when the user chooses "Reiniciar ahora".
- Unsigned test builds and development runs disable updates and say so in the
  menu and the About dialog.

### Verifying the installed version

- Menu: "Acerca de TDF Records" shows app version, channel, hosted origin, API
  origin, Electron/Chromium versions, source commit, build time and update
  state; "Copiar diagnóstico" copies the same lines.
- Terminal: `TDF Records --version` prints the shell version; `--smoke
  --smoke-receipt=<file>` loads the hosted application headlessly and writes a
  JSON receipt with `ok`, `loadedUrl`, `httpStatus`, `packaged`, `unsigned`.
- Web version: the hosted application's own `/acerca` page shows the web UI
  version and commit; `GET /version` on the API shows the backend.

### Recovery when an update fails

1. A failed check or download leaves the installed version untouched; the menu
   shows "Reintentar búsqueda de actualizaciones" and the About dialog shows the
   error. Retry when the network is back.
2. If an installed update does not start: macOS — drag the app to the Trash and
   reinstall the DMG from the `desktop-v<version>` release (user data in
   `~/Library/Application Support/TDF Records` is preserved); Windows — run the
   installer of the wanted version again (it replaces the per-user install; data
   in `%APPDATA%\TDF Records` is preserved) or uninstall from Settings › Apps.
3. If the hosted application fails to load, the shell shows the offline page
   and retries with backoff (5 s to 60 s); "Reintentar ahora" forces a retry. The
   web application remains the source of truth, so no desktop reinstall changes
   data.
4. To return to a clean state use "Cerrar sesión y borrar datos locales…" or
   delete the profile directory above; then sign in again.

## Manual acceptance plan (installed app)

Run on a real macOS 12+ machine and a real Windows 10/11 machine with the
installer produced by the workflow, a test account per role (admin/staff,
artist, customer) and synthetic data only. Record the receipts and screenshots
in `docs/desktop/evidence/<date>-<platform>/`.

1. Clean install, first launch (unsigned test build: expect Gatekeeper /
   SmartScreen warnings and the "prueba sin firma" title), sign in with
   email/password, confirm Google sign-in is replaced by the explanatory notice.
2. Cargas y lanzamientos: open a draft release, drag a WAV master, disconnect
   the network during the upload, reconnect, press "Reintentar": one upload
   session, same file, draft text preserved; accept the declaration, validate,
   submit to review.
3. Música y radio: play a release, use the hardware play/pause and next keys,
   the macOS Now Playing widget or the Windows media overlay, then the
   "Reproducción" menu; start the radio and confirm the keys now control the
   radio; pause explicitly; switch artist; open a release whose source fails and
   move to the next track.
4. Operación del estudio: open the calendar, edit a synthetic booking's status,
   confirm the change in a browser session of the web.
5. Eventos, campañas y ventas: open events, campaigns and payments; register a
   synthetic sale; disconnect before confirming an event publication and retry.
6. Expired session: revoke the session server-side (or wait for expiry) and
   confirm the app returns to the login screen without losing the target route.
7. Insufficient permissions: sign in as a customer and open a staff route;
   expect the 403 page.
8. Quit, reopen (session persists), sign out, uninstall; verify the profile
   directory behavior described above.

## Open limitations

- Google sign-in and Google Drive connection are not available inside the
  shell (Google Identity Services rejects embedded contexts); accounts created
  only with Google must reset a TDF password first.
- Hosted provider checkouts (Stripe, PayPal) and OAuth popups open in the
  system browser and finish there.
- Manual payment registration (`POST /payments`) carries no `Idempotency-Key`
  in the web client; a retry after a connection loss relies on the operator and
  server-side checks. This is pre-existing web behavior, listed here because
  the desktop sales flow inherits it.
- The native menu offers play/pause/next/previous; seeking, volume and a tray
  icon are left to the in-page player.
- Official distribution requires signing credentials (see the workflow secrets
  `DESKTOP_MAC_CSC_LINK`, `DESKTOP_MAC_CSC_KEY_PASSWORD`, `DESKTOP_APPLE_ID`,
  `DESKTOP_APPLE_APP_SPECIFIC_PASSWORD`, `DESKTOP_APPLE_TEAM_ID`,
  `DESKTOP_WIN_CSC_LINK`, `DESKTOP_WIN_CSC_KEY_PASSWORD`). Until they exist, CI
  produces `UNSIGNED-TEST` packages that are never published to the update feed.
