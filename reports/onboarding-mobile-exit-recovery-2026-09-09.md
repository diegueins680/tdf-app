# Onboarding mobile exit recovery — 2026-09-09

## Outcome

Mobile now preserves an explicit optional-onboarding exit across an offline app relaunch until the authenticated Party's completion handshake is acknowledged by the server. The app writes a Party-scoped pending-exit marker before calling the idempotent completion endpoint, keeps onboarding closed when that call fails, and retries on startup, foreground activation, or an offline-to-online transition.

The pending marker is removed only after the same authenticated Party receives a server acknowledgement. A late response after an account switch cannot clear or attribute the prior Party's marker. Simultaneous lifecycle recovery triggers share the existing in-flight exit request.

## Verification

- Focused `FirstRunProvider` and onboarding-intent Jest suites passed: 2 suites, 42 tests.
- The complete mobile Jest corpus passed: 70 suites, 413 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `28fd2adb1fa3253c0e311dd3ca4436c6f8bf83d2`.

This slice changes no API path or payload, database schema, authorization rule, onboarding destination, analytics taxonomy, experiment assignment, connectivity interval, or release flag. Local persistence remains best effort: if device storage itself is unavailable, the current Party session still exits immediately and the authoritative request is attempted, but no local marker can survive process termination.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
