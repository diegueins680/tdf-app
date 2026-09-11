# Onboarding mobile eligibility foreground recovery — 2026-09-09

## Outcome

Mobile now reloads authoritative first-run eligibility whenever the authenticated app returns to the foreground. A startup that failed closed while offline can therefore recover the correct new-user state after connectivity returns without requiring logout or an app restart.

Repeated startup and foreground triggers share one Party-owned eligibility request. Eligibility refresh and retained first-value recovery are reconciled together, while an acknowledged completion is applied immediately and takes precedence over a slower or stale eligible response.

## Verification

- Focused `FirstRunProvider` Jest passed: 1 suite, 16 tests.
- The complete mobile Jest corpus passed: 70 suites, 403 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `27d8332`.

This slice changes no API path or payload, database schema, authorization rule, onboarding destination, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
