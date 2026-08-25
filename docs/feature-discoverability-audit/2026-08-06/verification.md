# Verification and accessibility evidence

This is branch evidence, not production-deployment evidence. All browser and mobile captures use synthetic fixtures or a signed-out local app; no production record, name, email, token, credential, or identifier appears in an artifact.

## Automated gates

| Gate | Result |
| --- | --- |
| Registry drift audit | Pass: 115 features, 127 web routes, 35 Expo routes |
| Generated capability audit | Pass: 408 endpoints, 393 mapped to stable feature/actions, 0 undecided dispositions |
| Authorized primary discoverability | Pass: 466 fixture assertions; every allowed primary feature resolves through global navigation or safe search within two interactions |
| Role/action/platform matrix | Generated: 2,530 rows for 11 user types and 17 actions |
| Web tests | Pass: 124 suites, 1,538 tests |
| Mobile tests | Pass: 38 suites, 206 tests, including empty credential defaults and stale-route handling |
| Backend tests | Pass: 2,264 Hspec examples, 0 failures |
| Type checking | Pass: web and mobile |
| Lint | Pass with 0 errors; web retains 73 pre-existing warnings, mobile has 0 warnings |
| Web production build | Pass; initial bundle gzip 387,037 bytes within the repository budget |
| Expo web export | Pass; 47 static routes; dependency export warnings are documented below |
| Android native debug build | Pass on API 36.1 emulator: 491 Gradle tasks, 15m 11s |
| Database migrations | Pass in isolated PostgreSQL 16: forward, constraints, rollback, and forward reapply |
| Dependency remediation | Pass for reproducible install and CI production gate: clean `npm ci` and `audit-ci` succeed; root 0 critical/7 high/6 moderate, mobile 0 critical/0 high/16 moderate |

The 2026-08-07 dependency pass reduced the root baseline from 23 findings (1 critical, 17 high, 3 moderate, 2 low) to 13 (0 critical, 7 high, 6 moderate), and the mobile baseline from 52 (2 critical, 18 high, 28 moderate, 4 low) to 16 (0 critical, 0 high, 16 moderate). It removed unused root/mobile Testomatio duplication and the pinned mobile EAS CLI, updated the active web Testomatio adapter and DOMPurify, applied narrow patched transitive overrides, and retained `npx eas-cli@latest` for release operations. Clean root and mobile `npm ci` installs, the production-dependency `audit-ci` gate, all web/mobile tests, lint, type checking, the web build, and the Expo web export pass after remediation.

The remaining root high findings are bounded to three dependency families: the `webtorrent`/`ip` chain (no non-breaking patched release is offered), the React Router RSC-mode CSRF advisory (this Vite application does not enable RSC; npm proposes an incompatible downgrade), and `tmp` pinned by the test-only Testomatio/Cucumber adapter. The remaining mobile moderate findings are in the Expo SDK 54 toolchain and its `uuid`/Xcode metadata; npm's offered resolution requires an Expo SDK major upgrade. These residuals require upstream or separately tested major-version work and remain a production-complete release risk, but no critical advisory remains and mobile has no high advisory. No `npm audit fix --force`, Expo SDK jump, WebTorrent downgrade, or React Router downgrade was applied.

The full web suite still emits existing React `act(...)` warnings in older tests. The Android build emits dependency deprecation warnings and the Expo web export reports `react-native-webrtc` compatibility warnings. The native debug log contained emulator/new-architecture warnings and a development loading-overlay timing error, but no `FATAL EXCEPTION`, `AndroidRuntime` crash, or application termination; the signed-out app rendered and remained interactive.

## Authorization and credential regressions

- Backend negative tests prove known URLs do not grant DDEX import/partner actions, venue writes, artist-profile writes outside ownership, or follow/unfollow under another caller identity.
- Access-request reviewers must possess the exact requested action, cannot decide their own request, and approval does not provision a broader grant.
- Unsupported actions do not inherit `view` in backend, web, or mobile evaluators.
- Mobile username/password fields start empty even under `__DEV__`; the login action starts disabled.
- Demo seed credentials are created only in a non-hosted/non-production runtime when a runtime password of at least 16 characters and token prefix of at least 24 characters are both present.
- Operational scripts default to localhost and require runtime credentials rather than repository-known values.

Historical credentials created before this correction must be rotated through a secure operational channel. Rotation was not attempted from this branch because it requires identifying the account owner, preserving recovery access, and delivering replacement credentials outside chat and source control.

## Responsive and visual evidence

The reproducible `scripts/capture-feature-audit-visuals.mjs` run produced:

- Desktop expanded and collapsed DDEX navigation in Spanish.
- Desktop English navigation.
- Bilingual command-palette search and global quick creation.
- DDEX detail breadcrumbs.
- Tablet, mobile-web drawer, and 320 px layouts.
- Locked `403` and internal access-request states.
- Authenticated mobile feature explorer using synthetic session data.
- Native Android signed-out application screen.

Artifacts are in [`screenshots/`](screenshots/). The native capture is [`mobile-app-native-android.png`](screenshots/mobile-app-native-android.png).

## Automated accessibility

Axe ran against eight representative changed surfaces: desktop DDEX, DDEX detail, tablet, mobile drawer, 320 px, locked `403`, access request, and mobile feature explorer. All eight reported zero violations. Each result retained one or two `incomplete` checks requiring human interpretation; these were reviewed alongside the screenshots and DOM/accessibility metadata. Raw counts are in [`screenshots/accessibility-results.json`](screenshots/accessibility-results.json).

Automated/manual browser checks also confirmed:

- Command-palette focus restores to the invoker.
- The 320 px surface has no body-level horizontal overflow.
- No changed interactive target fell below 44 × 44 CSS pixels.
- Drawers/dialogs use focus traps, Escape behavior, labelled roles, and main/breadcrumb landmarks.
- Spanish/English long labels wrap without covering controls.

## Manual keyboard and screen-reader spot checks

Keyboard focus order, visible focus, Escape, palette focus restoration, drawer closure, and breadcrumb navigation were spot-checked in Chromium. On the native Android emulator, TalkBack was enabled reversibly. Accessibility focus visibly traversed the public heading, `Ingresar`, and `Crear cuenta` in logical order; actionable elements exposed accessible names and bounds above 44 × 44 device-independent pixels. The original TalkBack-disabled emulator state was restored after the check.

A complete VoiceOver/NVDA traversal of every route is not represented by this spot check. It remains a release-candidate manual acceptance task, along with production-font/OS combinations, native hardware-back/gesture behavior, and authenticated native workflows requiring isolated fixture credentials.

## Business-flow scope

Registry, component, integration, authorization, migration, responsive, and signed-out native flows are verified. Credential-backed create/edit/approve/import/export workflows for every user type were not run against production and no isolated full-stack fixture environment was available. See [business-flow-results](business-flow-results.md). This limitation, the single emergency-administrator path, and the unmerged pull request prevent a production-complete declaration.
