# Onboarding mobile feed retry — 2026-09-10

## Outcome

The mobile onboarding treatment now gives users a direct, localized retry action when its events or moments feed fails while the device remains online. The action refreshes only active React Query entries under the authenticated Party's onboarding cache key, so recovery does not require leaving the focused first-value experience.

User-triggered recovery exposes an accessible busy and disabled state while the request is active. Repeated taps, reconnects, and foreground events share the same in-flight Party request; automatic lifecycle recovery remains visually silent. Completion restores the retry action even when the refetch rejects, allowing another attempt without remounting the gate.

## Verification

- The focused onboarding-gate Jest suite passed: 1 suite, 30 tests.
- The complete mobile Jest corpus passed: 70 suites, 437 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `6bf3956015ed1d813b0b368f3f1bb1e9d4d60058`.

This slice changes no query payload, API path, cache lifetime, automatic retry count, experiment assignment or exposure behavior, analytics taxonomy, variant algorithm, eligibility window, database schema, authorization rule, connectivity interval, or release flag. The onboarding experiment remains paused by server configuration.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
