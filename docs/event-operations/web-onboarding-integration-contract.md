# Web onboarding integration contract

This compatibility repair unblocks the whole-web gate from PR 14. It does not add an
event domain, change authentication/permissions, or infer completion from browser storage.
The canonical contract remains `POST /session/onboarding/reconcile`, authenticated with
the current cookie or captured bearer token and no client-selected Party or action body.

## Audit and required behavior (specified before implementation)

At base `881882e15cd1b2400cc2e0452ea50683dea675de`, `LoginPage` imports the absent
`markWebSignupCompleted` and `AppShell` imports the absent
`retryPendingFirstValueCompletion`. Their tests manufacture these exports with module
mocks, concealing the broken integration. `analytics/onboardingProgress.ts` instead
exports server-authoritative capture helpers; `SessionContext.tsx` already reconciles
on session hydration/login. `ServerAuth.finishOnboardingProgress` uses an atomic
incomplete-to-complete update, and server evidence selects the canonical first value.
The earlier [cross-device audit](../../reports/onboarding-cross-device-reconciliation-audit-2026-09-09.md)
documents that migration and its limitations.

| ID | Guard / operation / outcome | Model / automated evidence required |
|---|---|---|
| WO-01 | Remove obsolete signup markers. Signup consent, server `accountCreated`, login, intent persistence and navigation remain unchanged. No local completion authority. | Login component tests import the real analytics module; whole-web typecheck |
| WO-02 | Session hydration/login and `online` trigger the existing reconciliation endpoint. One pending request per effect/session; overlapping reconnect signals coalesce. Settled failures permit a later retry. | `WebOnboardingRecovery.SingleFlight`; rendered provider retry/deduplication tests |
| WO-03 | Check effect cancellation, session generation and current Party before dispatch and before consuming success/error. Logout, unmount, Party switch or same-Party credential rotation invalidates old results. Cleanup removes the listener. | `WebOnboardingRecovery.CurrentSessionOnly`; provider adversarial tests |
| WO-04 | Capture only `newlyCompleted=true` plus an allowlisted canonical server value. Offline/error/no evidence never manufactures completion. | `WebOnboardingRecovery.AuthoritativeOnly`; existing analytics/API tests and provider retry tests |
| WO-05 | Shell retains Party-scoped intent recovery, reconnect listener cleanup and in-flight coalescing. It no longer owns first-value recovery. | Shell and intent-recovery tests; one provider owns first-value reconciliation |

## Formal bounds and limitations

`WebOnboardingRecovery.tla` models two request slots, two Parties plus logged-out state,
three session generations, arbitrary response order, online-trigger coalescing, and
server-winning/non-winning/invalid-value receipts. Session invalidation abstracts effect
cleanup, unmount and credential rotation. The positive configuration must pass before
implementation; negative controls remove generation checking, authoritative receipts or
coalescing and must violate their named invariants. No liveness claim assumes a network
response always arrives. A hung request remains in flight until a session change/unmount;
timeouts, durable analytics receipts and cross-tab deduplication are separate work.

This model checks client receipt consumption, **not** server authentication, evidence SQL,
revocation fencing, cookie identity races or access grants. Existing server APIs and their
authority are unchanged. Cookie-only reconciliation necessarily uses the browser's current
cookie at dispatch. Repeated successful requests remain server-idempotent, but a lost
winning response can undercount analytics (existing contract); no lossless claim is made.
No new Alloy relation is needed: this repair adds no ownership, task or permission relation.

## Compatibility and rollback

No schema, migration, OpenAPI/generated client, mobile pointer, feature flag, analytics
payload, rendered copy or production provider changes. Revert this bounded commit to
roll back (which restores the known broken imports); no data deletion is necessary.
This is not web/mobile E2E completion or a production activation.
