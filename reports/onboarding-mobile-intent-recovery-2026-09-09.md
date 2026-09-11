# Onboarding mobile intent recovery — 2026-09-09

## Outcome

Mobile now retries retained onboarding-intent persistence when an authenticated Party enters the first-run lifecycle. An offline sync after login therefore receives a later best-effort recovery on app startup instead of waiting for another login.

Recovery validates the stored intent, requires an active Party, and rechecks Party ownership after the API response before clearing local state. A failed request or an account switch retains the intent. The recovery runs independently from cohort loading, so a slow intent request cannot delay onboarding eligibility or normal app use.

## Verification

- Focused mobile recovery Jest passed: 2 suites, 24 tests.
- The complete mobile Jest corpus passed: 70 suites, 395 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `f2d1f2a`.

This slice changes no API path or payload, database schema, authorization rule, onboarding destination, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
