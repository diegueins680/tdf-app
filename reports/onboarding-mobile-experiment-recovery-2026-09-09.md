# Onboarding mobile experiment recovery — 2026-09-09

## Outcome

Mobile now reloads the server-authoritative onboarding experiment assignment when the app returns to the foreground or connectivity returns. An offline startup still fails closed, but it no longer requires logout or process restart before an eligible Party can recover its assignment.

Assignment requests are coalesced per Party, so overlapping startup, foreground, and reconnect triggers reuse one in-flight request. Results and assignment analytics are applied only while the initiating Party still owns the session. A failed refresh clears the recovered assignment state instead of enabling a partial or stale experiment configuration.

## Verification

- Focused `ExperimentProvider` Jest passed: 1 suite, 6 tests.
- The complete mobile Jest corpus passed: 70 suites, 416 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `b494a8ede888080e5e58233c490ac2fba4ad8ec1`.

This slice changes no experiment assignment or exposure API, variant algorithm, eligibility window, analytics taxonomy, database schema, authorization rule, connectivity interval, or release flag. The onboarding experiment remains paused by server configuration.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
