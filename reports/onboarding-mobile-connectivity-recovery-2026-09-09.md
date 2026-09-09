# Onboarding mobile connectivity recovery — 2026-09-09

## Outcome

Mobile now retries Party-owned onboarding intent, first-value completion, and authoritative eligibility when connectivity returns while the app remains active. Recovery no longer requires a background/foreground transition, logout, or app restart.

`NetworkProvider` now wraps `FirstRunProvider`, allowing onboarding to observe the existing connectivity signal without adding another poller. Only offline-to-online transitions trigger recovery, and simultaneous reconnect plus app-activation events reuse the existing per-Party in-flight requests.

## Verification

- Focused `FirstRunProvider` Jest passed: 1 suite, 21 tests.
- The complete mobile Jest corpus passed: 70 suites, 408 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `5df3ede`.

This slice changes no connectivity interval, API path or payload, database schema, authorization rule, onboarding destination, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
