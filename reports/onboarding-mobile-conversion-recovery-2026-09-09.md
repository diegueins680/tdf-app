# Onboarding mobile conversion recovery — 2026-09-09

## Outcome

Mobile now retains Party-scoped onboarding experiment conversion attribution before starting the authoritative first-value completion handshake. If the completion response or app process is lost, lifecycle recovery reconciles the receipt with server onboarding progress on startup, foreground activation, or reconnect and emits the confirmed conversion with its original experiment variant and version.

Conversion reconciliation is coalesced per Party and ignores late results after an account change. A server-confirmed different terminal outcome clears the stale receipt without emitting conversion analytics. A confirmed receipt remains pending while the analytics client is unavailable and is cleared only after the configured client accepts the local capture calls. Direct server rejections remain terminal, preventing a stale gate from claiming an already-completed conversion.

## Verification

- Focused experiment-provider, onboarding-gate, and first-run-flag Jest suites passed: 3 suites, 55 tests.
- The complete mobile Jest corpus passed: 70 suites, 432 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `c8d515b6db67387d1d1d68748d2835f64677db0b`.

This slice changes no experiment assignment, exposure or onboarding API, variant algorithm, eligibility window, database schema, authorization rule, connectivity interval, or release flag. It adds `experimentVersion` metadata to the existing `experiment_converted` event and retains only the experiment ID, version, variant, and first-value label in local application storage. The onboarding experiment remains paused by server configuration.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
