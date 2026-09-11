## Problem

The optional first-reaction onboarding surface silently swallowed reaction failures and gave no explanation when a reaction existed only on the device. Event detail also displayed a saved-event retry label while disabling the same control. Shared moment cards and saved-event recovery states mixed Spanish and English despite a persisted locale.

## Changes

- distinguish remote-confirmed, local-only, and failed onboarding reactions;
- announce non-success states and provide an explicit idempotent retry;
- add in-place onboarding-feed retry;
- enable Event detail's saved-query error retry and give it a retry-specific accessible name;
- localize shared moment-card and saved-event state/action copy through the existing locale provider/catalog pattern;
- choose catalog reaction `nameEs`/`nameEn` explicitly;
- add accessible roles, names, busy/disabled state, and practical 44-point sizing to affected controls;
- add rendered regression tests for ES/EN copy, failure/local-only recovery, retry, and no false completion.

The paused experiment remains paused. No API, database, permission, price, policy, dependency, analytics event, or production configuration changed.

## Evidence

- focused Jest: 6 suites / 42 tests passed;
- final focused Jest: 4 suites / 28 tests passed;
- full mobile Jest: 67 suites / 375 tests passed;
- TypeScript passed;
- ESLint passed with zero warnings allowed;
- release check passed, including 5 assets and production-profile validation;
- Expo Doctor: 17/17 checks passed;
- required root doctor: 14 OK / 4 warnings / 0 errors;
- Android tooling found no attached device;
- iOS simulator was available, but native build/install was stopped before completion after compiling dependencies, so native runtime is unverified.

Full evidence and coverage: `reports/onboarding-recovery-localization-audit-2026-09-08.md`.

## Risks and gaps

- failed completion handshake after an already-successful server reaction is not durably resumed;
- surrounding Events/Event detail/Profile copy is not yet fully localized;
- Events/Profile state matrices lack dedicated rendered tests;
- no controlled-account native walkthrough, screen-reader run, screenshot comparison, analytics dashboard, backend rerun, or production validation occurred.

## Rollback

Revert the mobile commit and the parent submodule/report commit. There is no migration or data rewrite.
