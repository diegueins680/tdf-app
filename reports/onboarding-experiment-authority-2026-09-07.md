# Onboarding experiment authority — 2026-09-07

## Outcome

Assignment and exposure for `single-feature-onboarding-v1` are now server-authoritative and bound to the authenticated Party. The backend stores one immutable assignment per Party, experiment identifier, and version, and atomically records at most one in-window exposure for both control and treatment arms.

The mobile client no longer assigns with `Math.random()` or stores assignment/exposure truth in AsyncStorage. It fetches the authenticated server assignment, reports exposure through the protected endpoint, and emits analytics only when the server says that request created the assignment or exposure. Live account switches hide the previous Party's state immediately and reset the per-Party exposure latch.

## Eligibility and rollout safety

- Assignment is permitted only while authoritative onboarding eligibility is active.
- Existing assignments remain stable for their recorded experiment version.
- Completed, expired, missing-progress, unsupported, and paused cases fail closed.
- The deployment flag `SINGLE_FEATURE_ONBOARDING_EXPERIMENT_ENABLED` defaults to `false`.
- Fly configuration validation and generated deployment arguments force the flag to remain `false` until a separate explicit activation approval.
- The rollback is intentionally non-destructive so a later roll-forward cannot rebucket Parties or duplicate exposure.

## Persistence and contract

The additive `user_experiment_assignment` table records Party ownership, experiment identifier/version, immutable arm, assignment time, eligibility deadline, and optional exposure time. Database checks constrain the current protocol and ensure exposure falls inside the assignment window. A partial index supports pending-exposure operations.

The authenticated API now exposes assignment resolution and one-shot exposure recording. OpenAPI remains the source for regenerated web and mobile types. The arm enum is reviewed as an immutable versioned protocol discriminant: changing arms requires a new experiment version.

## Verification

- PostgreSQL migration rehearsal passed forward application, repeat application, constraints, Party cascade, non-destructive rollback, and reapply.
- Production-release contract passed 51/51 tests; CI-scope safeguards passed 16/16.
- Mobile focused experiment/onboarding tests passed 16/16.
- Full mobile quality passed 66 suites and 355 tests, TypeScript, and ESLint.
- Mobile production release checks passed assets, lint, TypeScript, release profile, and Expo configuration.
- OpenAPI web and mobile clients regenerated successfully.
- Strict catalog-list audit has no unreviewed candidate or stale decision.

## Scope

This change does not activate the experiment, deploy, mutate production data, merge a pull request, or communicate externally. Activation remains a separate approval-gated operational decision after the stacked changes are reviewed and deployed safely.
