# Draft PR: Recover interrupted onboarding completion safely

## Problem

A first useful action could succeed on the server while the separate onboarding-completion request failed. Mobile swallowed that failure and had no Party-bound retry after relaunch. The server also rejected a later retry solely because the handshake arrived after the 24-hour window, even when the durable action evidence was created in time.

## Changes

- persist only an allowlisted first-value retry label under the authenticated Party
- require and assert the initiating auth session across persistence, request, cleanup, state, and analytics
- retry once on authenticated bootstrap, app foreground, and offline-to-online recovery
- keep completion analytics gated by the server's atomic `newlyCompleted` result
- prevent an older eligibility GET from overwriting a newer completion
- accept delayed first-value handshakes when durable evidence is in the signup window
- preserve evidence time in `firstValueCompletedAt` and handshake time in `completedAt` for newly accepted completions; legacy rows are left unchanged
- keep late explicit exits, missing evidence, and out-of-window evidence rejected
- update canonical OpenAPI documentation and regenerate both clients
- keep `single-feature-onboarding-v1` disabled

## Evidence and tests

- mobile focused recovery: 2 suites / 24 tests passed
- mobile focused onboarding surfaces: 5 suites / 47 tests passed
- mobile full Jest: 67 suites / 386 tests passed on the final serial rerun
- mobile typecheck, lint, and release checks passed
- focused web session/onboarding/routing: 3 suites / 22 tests passed
- web typecheck passed
- canonical API generation passed with required mobile workspace
- repository doctor exited 0 with 14 OK / 4 documented warnings / 0 errors
- backend late-reconciliation match: 3 examples / 0 failures
- backend evidence match: 20 examples / 0 failures after correcting a SQLite/PostgreSQL timestamp-comparison regression caught by the first run
- full web lint did not complete under local resource contention; generated types are lint-ignored and hosted results are recorded separately
- root hosted results: see final PR update/report

Detailed evidence: `reports/onboarding-completion-recovery-audit-2026-09-08.md`.

## Risk and rollback

Risk is concentrated in onboarding timestamp semantics and mobile provider lifecycle. No schema or response-shape migration is required. The backend still derives Party identity from the session and requires durable in-window evidence. Mobile persistence is only a retry hint and cannot grant eligibility, roles, modules, or permissions.

Rollback is the focused root commit plus the mobile submodule-pointer commit. A stale client marker is harmless: the idempotent server response returns `newlyCompleted=false` and cleanup is best-effort.

## Remaining gaps

- pending retry metadata does not transfer across devices; server-side reconciliation is the next integrity batch
- analytics is intentionally at-most-once and may undercount when the server commits but its response is lost
- native device and authenticated staging walkthroughs were not available
- no screenshots were created because rendered UI did not change
- broader platform audit and localization backlog remains in the report

This is a stacked draft PR based on the preceding onboarding recovery/localization work. It must not be merged before its base stack.
