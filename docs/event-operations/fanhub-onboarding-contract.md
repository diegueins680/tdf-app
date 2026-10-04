# FanHub authoritative onboarding contract (FH-01–06)

Base: draft PR 364, `1299f9c63a325e17dff0bfe1f048ff52f7bb8b15`.
This is shared profile/discovery compatibility, not completion of event operations.

## Audit before implementation

The unmodified `FanHubPage.onboarding.test.tsx` reproduces five failures. FanHub reads and
writes the global `fanhub-onboarding-dismissed` key, never loads canonical eligibility,
and never persists explicit exit. That key crosses account boundaries and can throw when
storage is unavailable. A loading indicator lacks an accessible name. Commit `62316124e`
contains the lost recovery flow, but its Party-only fence and accepting any HTTP success
are insufficient for session rotation and malformed/nonterminal receipts.

`TDF/ServerAuth.hs:currentOnboardingProgress`, `completeOnboarding` and
`finishOnboardingProgress`, the OpenAPI contract and generated types establish that:
the authenticated Party is the owner; an empty completion request is an explicit optional
exit; verified evidence takes precedence; conditional completion prevents repeated writes;
and an ineligible/missing row can return a successful HTTP response without new completion.
No new exit endpoint, local completion authority, migration or legal policy is needed.

| ID | Operation / invariant | Executable refinement |
|---|---|---|
| FH-01 | Authenticated guidance requires a valid, current, eligible and incomplete server read. Loading, failed, malformed, logged-out and replaced-session reads cannot expose another lifetime's guidance. | Isolated session-generation query keys; strict progress decoder; component tests |
| FH-02 | Only explicit close/retry after eligible read may send empty completion. No render, read, guest/manager dismissal, legacy storage marker or timer sends completion. One in-flight exit per context. | `FanHubOnboarding.ConsentOnly`, `SingleFlight`; captured credentials; HTTP body assertions |
| FH-03 | A receipt may hide guidance only if structurally valid and terminal (`eligible=false`). `newlyCompleted` alone or HTTP 2xx is not proof. Failure/nonterminal/malformed receipt restores guidance with an actionable retry; no fabricated first-value analytics. | `TerminalOnly`; receipt/negative-response tests |
| FH-04 | Read, failure and completion results remain bound to session identity, effective generation and mounted lifetime, including same-Party rotation and A→B→A. A stale callback cannot change new UI or invalidate its cache. | `CurrentContext`; delayed session/unmount tests |
| FH-05 | Guest/manager tips are separate ephemeral presentation state, reset with context. Ignore the legacy global marker without deleting it. No local persistence is claimed or promoted to canonical completion. | Storage isolation/guest/manager tests; unchanged server routes |
| FH-06 | Spanish default and English copy for new states, named loading indicators, keyboard-operable close/retry, no raw server errors. Existing hub content and roles remain unchanged. | Component accessibility and synthetic browser tests |

## States and guards

Authenticated states: loading → ready/hidden/load-error; ready → saving on explicit close;
saving → hidden only on a validated terminal receipt, otherwise save-error; load-error →
loading on explicit retry; save-error → saving on explicit retry. Context change invalidates
all old reads, callbacks and local presentation state. A server-expired/ineligible result is
hidden, not displayed as successful first value. Saving is visible and not a fake completion.
An acknowledged terminal receipt remains hidden for that context even if a GET started
before completion resolves later with old eligibility. This acknowledgement is ephemeral
projection state derived only from the validated receipt, never a persisted local marker.
The existing SessionProvider may independently reconcile verified first-value evidence;
FH-02 governs FanHub's explicit `/complete` command, not that pre-existing server-evidence
reconciliation. Other components' progress changes become visible on the usual query
refresh/remount; this repair does not add a cross-component progress push subscription.

TLA+ uses three context generations, current/stale lookup and exit results, two pending
slots for duplicate-dispatch mutation testing, eligible/ineligible/invalid outcomes and
explicit versus implicit attempts. Mutations remove consent, context, terminal and
single-flight guards. All must fail their named invariants before implementation starts.
No new relational structure is introduced; existing Alloy checks remain regression gates.
Finite checking is not a universal proof. No fairness or network completion is assumed;
a hung request remains visibly pending, not silently successful. Cross-tab cookies,
server revocation after dispatch and complete auth transport verification remain outside
the client model. No UI fence cancels an already-dispatched server mutation.

## Compatibility and rollback

Use existing generated DTOs and API routes. Add optional captured bearer-token arguments
to the existing read/complete helpers, retaining cookie behavior and old call signatures.
Tokens never enter query keys, logs or local storage. Validate responses at the FanHub
boundary without changing other consumers' decoding contracts. Tests replace obsolete
`completed`/receipt stubs with actual generated DTO shapes, preserving all five original
behavior assertions and adding adversarial cases. No schemas, grants, provider flags,
mobile pointer or payment behavior change. Rollback reverts this bounded commit; no data
migration or deletion is performed. The separate hub follow/profile mutations are not
claimed to gain this exit workflow's context fence.
