# Onboarding mobile first-value foreground recovery — 2026-09-09

## Outcome

Mobile now retries a retained Party-scoped first-value completion whenever the authenticated app returns to the foreground. A completion handshake that failed offline can therefore reach the authoritative server after connectivity returns without requiring logout or an app restart.

Startup and foreground triggers share one Party-owned in-flight replay. A successful foreground result closes onboarding for that Party and remains authoritative if an older eligibility request completes later, while the existing ownership checks suppress state updates and attribution after an account change.

## Verification

- Focused `FirstRunProvider` Jest passed: 1 suite, 14 tests.
- The complete mobile Jest corpus passed: 70 suites, 401 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `0124cb0`.

This slice changes no API path or payload, database schema, authorization rule, onboarding destination, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
