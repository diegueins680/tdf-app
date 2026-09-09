# Onboarding mobile event-reaction first value — 2026-09-08

## Outcome

A reaction posted from the normal mobile event-detail screen now submits the existing `moment_reaction` onboarding completion handshake only when the server returns a selected reaction. This closes the gap where the experiment gate could complete that first value but the destination event experience could not.

The initiating Party travels with the mutation, and a live Party-ownership callback is checked before, during, and after completion. Local/offline fallback reactions, reaction removal, missing identity, repeated completion, and account-switch outcomes do not emit completion analytics. A failed handshake remains Party-scoped for the existing retry path, while the successful reaction remains intact.

## Verification

- Focused mobile Jest passed with the repository's default timeout: 3 suites, 18 tests.
- The complete mobile Jest corpus passed: 67 suites, 370 tests. A 15-second command-line timeout was used for the full local run to avoid machine-contention false negatives; no timeout source was changed.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed with no new decision.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed in the mobile and root repositories.

## Scope

Mobile implementation: [TDF-mobile #53](https://github.com/diegueins680/TDF-mobile/pull/53), commit `4035db9`.

This slice changes no API path, database schema, authorization rule, experiment state, or release flag. The backend's existing Party-bound, signup-window, public-event evidence check remains authoritative for `moment_reaction`.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
