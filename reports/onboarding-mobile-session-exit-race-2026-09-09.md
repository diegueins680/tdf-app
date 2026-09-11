# Onboarding mobile session-exit race — 2026-09-09

## Outcome

Mobile now treats an explicit onboarding completion or exit as monotonic for the current authenticated Party session. A foreground eligibility request that began earlier can no longer return `eligible: true` afterward and reopen onboarding.

The local exit marker is established before the asynchronous completion call, so an offline completion failure still cannot trap the user again during that Party session. The marker is cleared when the active Party changes, allowing the next account to load its own authoritative eligibility normally.

## Verification

- Focused `FirstRunProvider` Jest passed: 1 suite, 19 tests.
- The complete mobile Jest corpus passed: 70 suites, 406 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `135062f`.

This slice changes no API path or payload, database schema, authorization rule, onboarding destination, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
