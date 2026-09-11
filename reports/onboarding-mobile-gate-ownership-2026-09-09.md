# Onboarding mobile gate ownership — 2026-09-09

## Outcome

The mobile first-run provider and single-feature onboarding gate now fail closed across Party changes and unmounts. Eligibility and replay state are tagged with the Party they belong to, so a newly active account cannot briefly inherit the previous account's onboarding cohort or replay result while its own progress is loading.

Completion, exposure, reaction, conversion, and exit callbacks capture their initiating Party and re-check live ownership before returning results, changing local onboarding state, or emitting analytics. A late response from an earlier account is ignored. The explicit "Explore more" exit and one-shot conversion state are Party-scoped, allowing another eligible account in the same app process to receive its own experience.

The experiment event and moment query keys now include Party identity. Cached onboarding feed state and invalidation therefore remain isolated when accounts change.

## Verification

- Focused provider and gate Jest passed: 2 suites, 21 tests.
- The complete mobile Jest corpus passed: 70 suites, 384 tests. A 15-second command-line timeout was used to tolerate local machine contention; no source or test timeout was changed.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `b85469e`.

This slice changes no API path, database schema, authorization rule, analytics taxonomy, experiment assignment, or release flag. The server remains authoritative for onboarding eligibility and completion.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
