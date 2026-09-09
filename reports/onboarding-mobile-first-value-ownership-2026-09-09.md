# Onboarding mobile first-value ownership — 2026-09-09

## Outcome

Mobile first-value completion is now bound to the Party that initiated the underlying domain mutation across event saves, artist follows, access requests, and moment reactions. Every mutation carries that Party through its success callback, and the shared completion boundary checks live ownership before starting the handshake, after the authoritative response, and before emitting conversion analytics.

The ownership hook tracks account changes and fails closed after its screen unmounts. A late success or failure therefore cannot show feedback, navigate, or emit first-value analytics through a later account. Only an authoritative `newlyCompleted: true` response emits `first_value_completed` and `onboarding_completed`.

Saved-event synchronization uses the same live ownership boundary. If an account changes before or during outbox replay, no later favorite request or completion handshake starts through the new session. Remaining desired-state changes stay queued under the initiating Party and can resume when that Party becomes active again.

## Verification

- Focused mobile Jest passed: 7 suites, 30 tests.
- The complete mobile Jest corpus passed: 70 suites, 379 tests. A 15-second command-line timeout was used because of severe local machine contention; no test or source timeout was changed.
- After CI exposed clean-install TypeScript inference in three new Jest wrappers, their argument tuples were made explicit. The exact root mobile-quality wrapper then passed lint and TypeScript; its default-timeout Jest phase passed 68/70 suites and 376/379 tests before two unchanged ticket-checkout tests exceeded five seconds and a downstream onboarding-gate wait missed its polling window under local contention. The three corrected screen suites passed 8/8 immediately afterward with the repository default timeout.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit completed with 945 candidates, 0 unreviewed candidates, and 0 stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation: [TDF-mobile #55](https://github.com/diegueins680/TDF-mobile/pull/55), commit `d126b9d`.

This slice changes no API path, database schema, authorization rule, analytics taxonomy, experiment assignment, or release flag. The server remains authoritative for Party-bound domain evidence and onboarding eligibility.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
