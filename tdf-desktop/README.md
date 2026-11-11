# tdf-desktop

Electron shell for TDF Records on macOS and Windows. It hosts the canonical web
application (`tdf-hq-ui` served at the channel's `appOrigin`) and adds the
native pieces a browser cannot: hardened window, navigation boundary, validated
deep links, native playback menu wired to the shared playback session, shell
updates and installers.

Documentation: [docs/desktop/README.md](../docs/desktop/README.md),
[ADR 0132](../docs/adr/0132-desktop-shell-electron.md),
[formal/system/desktop-shell.md](../formal/system/desktop-shell.md).

## Layout

| Path | Purpose |
| --- | --- |
| `src/main/main.cjs` | Main process: window, session hardening, navigation policy, internal pages, IPC, menu, updater, smoke mode |
| `src/main/config.cjs` | Channel loading and validation (`appOrigin`, `apiOrigin`, update feed) |
| `src/main/links.cjs` | Deep-link parsing and navigation classification |
| `src/main/playbackState.cjs` | Sanitized playback reports and menu model |
| `src/main/updater.cjs` | `electron-updater` state machine (check → available → download → install) |
| `src/main/menu.cjs` | Native application menu |
| `src/preload/preload.cjs` | `window.tdfDesktop` bridge, exposed only on the authorized origin |
| `internal/` | Offline and blocked pages served on `tdf-desktop://internal/` |
| `channels/` | `production.json`, `local.json` |
| `scripts/prepare-channel.cjs` | Writes `generated/channel.json` and copies the official icon to `build/icon.png` before packaging |
| `electron-builder.config.cjs` | DMG/ZIP (x64, arm64), NSIS (x64, arm64), conditional signing/notarization, `UNSIGNED-TEST` marker |
| `test/` | `node --test` unit tests |

Electron journeys live in `../e2e/desktop` (`npx playwright test --config=playwright.desktop.config.mjs`,
through `xvfb-run -a` on headless Linux).

## Commands

```bash
npm ci
```

```bash
npm test
```

```bash
TDF_DESKTOP_APP_URL=http://localhost:5173 TDF_DESKTOP_API_URL=http://localhost:8080 npm start
```

Packaging resolves one signing decision from the environment
(`scripts/signing.cjs`): without a certificate the package is an unsigned test
build and its embedded `channel.json` says so; `TDF_DESKTOP_UNSIGNED=1` forces
that even when a certificate is available.

```bash
TDF_DESKTOP_UNSIGNED=1 npm run dist:mac
```

```bash
TDF_DESKTOP_UNSIGNED=1 npm run dist:win
```

## Renderer bridge contract

`window.tdfDesktop` (frozen; present only on the channel origin):

- `version`: `{ app, electron, chromium, platform, arch, channel, unsigned }`
- `playback.report(state)`: `{ owner: 'music' | 'radio' | null, status, title, artist, canNext, canPrevious }`
- `playback.onCommand(listener)`: receives `'play' | 'pause' | 'toggle' | 'next' | 'previous'`; returns an unsubscribe function
- `links.openExternal(url)`: resolves `true` only for `http(s)` URLs without credentials

The web side lives in `tdf-hq-ui/src/desktop/shell.ts` and
`tdf-hq-ui/src/player/playbackSession.ts`.
