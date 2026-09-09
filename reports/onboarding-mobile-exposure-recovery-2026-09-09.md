# Onboarding mobile exposure recovery — 2026-09-09

## Outcome

Mobile no longer treats an attempted onboarding experiment exposure as complete before the server acknowledges it. A transport failure remains retryable, and the gate retries the idempotent exposure request when connectivity returns or the app returns to the foreground.

Exposure requests are coalesced by authenticated Party and authoritative experiment version. Late results after an account or assignment-version change are ignored. A server-acknowledged repeat, paused assignment, or ineligible assignment is terminal locally without emitting duplicate `experiment_viewed` analytics. Newly recorded analytics now include the authoritative experiment version.

`ExperimentProvider` exposes the assignment version it already receives from the server, allowing a new version to receive its own exposure even when its variant label is unchanged.

## Verification

- Focused experiment-provider, onboarding-gate, and first-run-flag Jest suites passed: 3 suites, 27 tests.
- The complete mobile Jest corpus passed: 70 suites, 421 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `6858ee1881bcc703d6082464495b09db44106561`.

This slice changes no experiment assignment or exposure API, variant algorithm, eligibility window, database schema, authorization rule, connectivity interval, or release flag. It adds `experimentVersion` metadata to an already-defined `experiment_viewed` event. The onboarding experiment remains paused by server configuration.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
