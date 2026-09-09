# Onboarding mobile intent Party storage — 2026-09-09

## Outcome

Mobile now transfers a pending acquisition intent from device-global pre-auth storage into Party-scoped storage as soon as an existing-account login establishes the authenticated Party.

If the server sync is offline, the retained intent belongs only to that Party and cannot be inherited by a later account. Authenticated startup recovery prefers its Party-scoped value and leaves a different global intent for a newer authentication attempt untouched. Successful persistence removes only matching values, with token or Party ownership checked around cleanup.

## Verification

- Focused mobile intent and provider Jest passed: 3 suites, 48 tests.
- The complete mobile Jest corpus passed: 70 suites, 396 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `178e30c`.

This slice changes no API path or payload, database schema, authorization rule, onboarding destination, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
