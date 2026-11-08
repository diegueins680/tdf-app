# Desktop shell boundary, shared playback session and shell distribution

Requirements: SYS-DESKTOP-001 (hosted-origin boundary and validated entry
points), SYS-DESKTOP-002 (one system playback session for release player and
radio), SYS-DESKTOP-003 (reproducible installers, shell update channel and
recovery). Decision record: [ADR 0132](../../docs/adr/0132-desktop-shell-electron.md).
Operator documentation: [docs/desktop/README.md](../../docs/desktop/README.md).

## SYS-DESKTOP-001 — Boundary

Intended behavior. The Electron shell (`tdf-desktop/`) hosts the canonical web
application at the channel's `appOrigin` (production: `https://www.tdfrecords.net`)
and never grants the page Node or Electron APIs beyond a frozen
`window.tdfDesktop` bridge. The bridge is exposed only when the document origin
equals `appOrigin`; the main process re-validates `senderFrame.url` for every
IPC message and ignores anything else. Top-level navigations and popups are
classified before they happen: `app` (same origin) proceeds, `internal`
(`tdf-desktop://internal/*`, bundled pages) proceeds, `external` (`http(s)`)
opens in the system browser and is cancelled in the shell, everything else is
blocked and logged. Deep links `tdf-desktop://open` resolve to an in-app path
only through `sanitizeAppPath` (absolute, single slash, no `.`/`..` segments,
no encoded separators, no control characters); rejected links are logged and
never navigate. Channel files are validated (`https` origins only; loopback
`http` only for local/unpackaged runs; development overrides ignored when
packaged). Permissions granted to the hosted origin are limited to media,
notifications, fullscreen, clipboard and speaker selection; device access is
denied. Unpackaged and unsigned builds are visibly labelled.

Authentication, authorization, business rules and data remain in the hosted
application and the TDF API; the shell adds no credential store. Google
Identity Services are not offered inside the shell.

Evidence. `tdf-desktop/test/links.test.cjs` and `config.test.cjs` exercise
the parsers with traversal, protocol-relative, credential, foreign-scheme and
oversize inputs. `tdf-hq-ui/src/desktop/shell.test.ts` rejects malformed
globals. `e2e/desktop/shell-session.spec.mjs` runs the real shell (Electron,
Xvfb) against the production web build and a synthetic API: bridge presence
only on the hosted origin, Google notice, sign-in/relaunch/sign-out, server
expiry, 403 page, deep-link rejection count, offline fallback and retry,
external links to the system browser, blocked `file:` navigation. These are
Linux evidence; macOS/Windows installed-app runs are produced by the
`Desktop app` workflow smoke and the manual plan.

## SYS-DESKTOP-002 — Shared playback session

Intended behavior. `tdf-hq-ui/src/player/playbackSession.ts` keeps an ordered
set of playback owners (`music`, `radio`). The owner that most recently started
or selected audio is active; only the active owner's metadata and action
handlers are installed on `navigator.mediaSession` and reported to the desktop
shell. Passive state updates never change the active owner, so a release
player paused by a radio takeover does not steal hardware keys from the radio.
Releasing the active owner restores the previous one; with no owner, metadata
is cleared and handlers are null. Shell commands (`play`, `pause`, `toggle`,
`next`, `previous`) route to the active owner. The shell's native menu mirrors
the last sanitized report.

Evidence. `playbackSession.test.ts` covers ownership order, passive updates,
restoration, missing Media Session and shell reporting/command routing.
`PlayerProvider.test.tsx`, `PlayerIdentity.test.tsx` and
`RoutedRadioWidget.test.tsx` remain green with the integration.
`tdf-desktop/test/playbackState.test.cjs` covers report sanitization and the
menu model. `e2e/desktop/playback.spec.mjs` plays generated PCM in the real
shell: menu labels, native pause, next (artist change), failing source,
radio takeover and pause/resume through the native menu. Hardware key
delivery by the operating system is not exercised in CI.

## SYS-DESKTOP-003 — Installers, updates and recovery

Intended behavior. `electron-builder.config.cjs` produces DMG and ZIP for
macOS (x64, arm64) and NSIS for Windows (x64, arm64) from the same source
commit; `scripts/prepare-channel.cjs` embeds the validated channel, source
commit, build time and unsigned flag, and copies the official TDF icon. Signing
and notarization run only when the standard electron-builder credentials are
present; otherwise `identity: null` disables signing explicitly and artifacts
carry `UNSIGNED-TEST`. `electron-updater` reads a generic feed
(`desktop-latest` release); downloads require a user decision, are verified
before installation, and a failed check or download leaves the installed
version in charge. Unsigned or unpackaged builds disable updates and say so.
The workflow publishes to the feed only from a `desktop-v<version>` tag whose
version matches `tdf-desktop/package.json` and refuses unsigned files.

Evidence. `tdf-desktop/test/updater.test.cjs` covers disabled reasons and
the check/available/download/downloaded/install path plus failure handling;
`config.test.cjs` covers packaged versus development channel loading.
`.github/workflows/desktop.yml` builds on `macos-15` and `windows-2025`, runs
the installed binary's `--smoke` receipt (macOS from the mounted DMG, Windows
after a silent NSIS install, followed by silent uninstall) and uploads
installers, update metadata, checksums and receipts. Notarization, Authenticode
and real update rollouts are unverified until the credentials exist.

## Limitations

No property here is PROVEN. Linux Electron journeys and unit tests are TESTED;
installed macOS/Windows behavior is TESTED only through the workflow smoke
receipt until the manual acceptance plan is executed on real machines.
Operating-system media key delivery, Now Playing/SMTC rendering, Gatekeeper and
SmartScreen outcomes, and the live update feed are outside CI evidence.
