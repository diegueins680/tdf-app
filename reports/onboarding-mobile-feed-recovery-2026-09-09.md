# Onboarding mobile feed recovery — 2026-09-09

## Outcome

The active mobile onboarding treatment feed now refreshes when connectivity returns while the app remains open. It also refreshes when an online app returns to the foreground, so an offline or stale events/moments result no longer requires a remount, logout, or unrelated cache mutation to recover.

Recovery is scoped to active React Query entries under the authenticated Party's onboarding cache key. Overlapping reconnect and foreground triggers share one in-flight recovery, existing requests are not cancelled and restarted, and late work remains isolated under the initiating Party's cache. Control, ineligible, exited, and offline states do not start treatment-feed recovery.

## Verification

- Focused first-run, experiment-flag, and onboarding-gate Jest suites passed: 3 suites, 59 tests.
- The complete mobile Jest corpus passed: 70 suites, 436 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `2ab1415c3458e7852e07b583868a05961b49f824`.

This slice changes no query payload, API path, cache lifetime, retry count, experiment assignment or exposure behavior, analytics taxonomy, variant algorithm, eligibility window, database schema, authorization rule, connectivity interval, or release flag. The onboarding experiment remains paused by server configuration.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
