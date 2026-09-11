# Onboarding mobile intent foreground recovery — 2026-09-09

## Outcome

Mobile now retries a retained Party-scoped onboarding intent whenever the authenticated app returns to the foreground. An offline startup can therefore recover as soon as connectivity returns without requiring a provider remount, logout, or app restart.

Startup and foreground triggers share the same Party-owned in-flight promise. Repeated active-state events cannot issue duplicate concurrent persistence requests, and the existing Party ownership guard still prevents a late request from clearing intent state after the authenticated Party changes.

## Verification

- Focused `FirstRunProvider` Jest passed: 1 suite, 11 tests.
- The complete mobile Jest corpus passed: 70 suites, 398 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `8704ac2`.

This slice changes no API path or payload, database schema, authorization rule, onboarding destination, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
