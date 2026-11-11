# TDF App Desktop — verification report (2026-11-08)

Status: evidence for the first delivery stage. It distinguishes what ran on the
installed application, what ran on the unpackaged Electron shell on Linux, and
what ran in the browser or Node test runners. Installed macOS and Windows
journeys are **not** yet established; the `Desktop app` workflow produces the
installers and their installed-binary smoke receipts for the pull request, and
the manual plan in [README.md](README.md) completes acceptance on real machines.

## Summary matrix

| Platform | Package | Installed smoke (CI) | Manual journey (4 flows, sign-in/out, reopen, uninstall) |
| --- | --- | --- | --- |
| macOS 12+ (x64, arm64) | DMG + ZIP, `UNSIGNED-TEST` until Developer ID credentials exist | Produced by `.github/workflows/desktop.yml` on `macos-15` (mount DMG, `--version`, `--smoke` receipt) | Pending: run the plan in README.md and attach evidence |
| Windows 10 1809+/11 (x64, arm64) | NSIS, `UNSIGNED-TEST` until Authenticode or Azure Trusted Signing exists | Produced on `windows-2025` (silent install, `--smoke` receipt, silent uninstall) | Pending: run the plan in README.md and attach evidence |
| Linux x64 (verification only) | Unpacked `electron-builder --dir` | Ran in this session: packaged shell loaded `https://www.tdfrecords.net/inicio` with the embedded production channel (`ok: true`, `packaged: true`, `unsigned: true`) | Electron journeys below (not an installed package) |

## Evidence executed on the Electron shell (Linux, unpackaged, Xvfb)

Suite: `playwright.desktop.config.mjs` → `e2e/desktop/*.spec.mjs`. The hosted
application is the production web build of `tdf-hq-ui` served from a loopback
origin; the TDF API is synthetic (`e2e/desktop/helpers/api-fixture.mjs`). No
request reaches a live service.

| Journey | What it checks | Result |
| --- | --- | --- |
| `shell-session` › bridge | `window.tdfDesktop` frozen and present only on the hosted origin; Google sign-in replaced by the desktop notice; no Node in the page; unsigned title marker | passed |
| `shell-session` › sign-in / relaunch / sign-out | wrong password refused; TDF credentials accepted; API reads carry the session Bearer; relaunch with the same profile keeps the session; sign-out revokes on the server | passed |
| `shell-session` › expired session | server returns `invalid or inactive session token`; the app returns to `/login` | passed |
| `shell-session` › insufficient permissions | customer account opening a staff route sees `ERROR 403`; no staff data requested | passed |
| `shell-session` › deep links | `tdf-desktop://open?path=/social/eventos` navigates; four unsafe links rejected and logged, URL unchanged | passed |
| `shell-session` › offline fallback | unreachable hosted origin shows the bundled offline page (`code=-106`); "Reintentar ahora" recovers | passed |
| `shell-session` › navigation boundary | `http(s)` popups and links open in the system browser; `file:` and foreign schemes blocked; bridge refuses unsafe `openExternal` | passed |
| `uploads` (flow 1) | drag-and-drop WAV master; storage part upload cut (`ERR_INTERNET_DISCONNECTED`); retry reuses the same file and `Idempotency-Key`; one upload session, one confirm; draft title preserved and saved; accept declaration + validate; submission cut and retried with the same `Idempotency-Key`; server records one transition, version reaches `in_review` | passed |
| `playback` (flow 2) | native "Reproducción" menu mirrors title/artist and state; menu pause/next act on the release player; artist change; failing source (403) reported and next/previous recover; radio takeover pauses the release player; menu pause/resume act on the radio; `navigator.mediaSession` metadata and `playbackState` follow the active owner | passed |
| `operations` (flows 3 and 4) | staff opens studio calendar (synthetic booking visible), contacts, events, campaigns and payments; every data read carried `Authorization: Bearer <session token>` | passed |

Run locally: 10 passed (see the PR's `desktop-electron-journeys-<run id>`
artifact for the CI run of the same suite).

## Evidence executed in Node / Jest (CI and local)

| Suite | Scope | Result |
| --- | --- | --- |
| `tdf-desktop/test/*.test.cjs` (`node --test`) | deep-link parsing, navigation classification, channel validation, playback report sanitization and menu model, updater state machine and failure handling | 19 passed |
| `tdf-hq-ui` Jest (full suite) | includes new `src/desktop/shell.test.ts`, `src/player/playbackSession.test.ts`, `src/pages/MusicReleaseStudioPage.upload.test.tsx` plus the existing player, radio, Studio and login suites | 364 suites, 4119 tests passed |
| `tdf-hq-ui` typecheck and lint (`--max-warnings=0`) | whole web application | passed |
| Repository gates touched by this change | `scripts/__tests__/ci-pipeline.test.mjs` (workflow action pins), `test-specification-inventory.py`, `test-specification-conformance.py`, `test-new-specification-surfaces.py`, `test-requirement-declarations.py`; `specification-inventory.py --check` and `specification-conformance.py --check` regenerated with the pinned Mobile source | passed |

## Packaging evidence (this session)

- `electron-builder --config electron-builder.config.cjs --linux --dir` with the
  production channel: `resources/channel.json` embedded (origin
  `https://www.tdfrecords.net`, generic update feed, `unsigned: true`, source
  commit), `app-update.yml` generated for `electron-updater`.
- Packaged smoke receipt: `ok: true`, `reason: hosted-application-loaded`,
  `loadedUrl: https://www.tdfrecords.net/inicio`, `packaged: true`,
  Electron 45.2.3 / Chromium 156.
- Configuration validated against electron-builder's JSON schema.

## Not verified in this stage

- Installed macOS and Windows journeys with hardware media keys, Now Playing,
  Windows media overlay, Gatekeeper/SmartScreen behavior and uninstall.
- Code signing, notarization and the live update feed (no credentials).
- Google sign-in and Google Drive connection inside the shell (not offered).
- Manual payment registration retry idempotency (`POST /payments` has no
  `Idempotency-Key` in the web client; inherited limitation).
