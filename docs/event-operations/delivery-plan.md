# Dependency-ordered event operations delivery plan

Each phase is an independently reviewable branch/PR. Nothing here authorizes merge, production
deployment, credential changes, provider activation, or live financial operations.

| Phase | Scope | Dependency and exit evidence |
|---|---|---|
| 1. Audit/design | Audit, glossary, gap matrix, canonical domain ADRs, threat model | Evidence paths reviewed; duplicates and critical risks explicit. |
| 2. Formal specification | TLA+/PlusCal, Alloy, scopes/results/counterexamples, traceability, runner/CI | Models pass within bounds; valid Alloy instance exists; no feature schema/API before this exit. |
| 3. Foundations | Event ownership/coproduction/grants, revisions/audit, command receipts, lifecycle compatibility | Additive apply/rollback passes; API auth/model tests pass; invitation vulnerability closed; no public cutover by default. |
| 4. Logistics/tasks | Session/spaces, templates, transactional DAG, RACI, readiness, typed logistics, asset bindings | Concurrency/orphan/completion/recurrence tests; web read/write slice behind flag. |
| 5. Discovery/engagement | Event-linked opportunities, explainable search, availability/buffers, holds, proposals, contract versions, verified reputation | Search perf/privacy and parallel booking/contract-consent tests; old directory IDs preserved. |
| 6. Finance | Event budgets/procurement, deposits/milestones, commerce/ledger/payout settlement adapter | Sandbox verified-webhook, idempotency, refund/dispute/reconciliation tests; providers and payouts remain disabled by default. |
| 7. Collaboration/sync | Scoped discussions/decisions, notification policy/DLQ, calendar/webhooks, offline commands/conflicts | Retry/reorder/privacy/stale-grant/offline tests and operational alerts. |
| 8. UX | Coherent accessible Spanish-first web/mobile workspaces with English fallback | Critical Playwright/mobile journeys, keyboard/focus/contrast/reduced-motion tests; no dead controls. |
| 9. Hardening | Migration rehearsal, observability, security/privacy/legal/accounting review packets, recovery/runbooks | Full relevant CI, performance, restore/rollback exercises; precise production manual gates. |

## Required PR body

Every phase PR must state scope and previous-PR dependency; requirement/invariant IDs; schema and
migration/rollback effects; authorization/privacy consequences; exact commands/results; feature-flag
and compatibility behavior; remaining limitations; and manual legal/accounting/provider steps.

## Rollout policy

Use expand/backfill/dual-read/compare/cutover/contract. Backfills are restartable and report
unresolved ownership/resource links instead of guessing. Compatibility routes remain until web,
mobile, generated clients, workers, and external consumers are verified. Production flags stay off
until a separately authorized release review.

Current branch-chain status: phase 1 and phase 2 are complete within their documented evidence/bounds.
Phase 3 has an additive, database-tested foundation, closes the existing authenticated event-
invitation authorization flaw, and now includes an authenticated, typed, disabled-by-default
lifecycle API plus OpenAPI/generated web client contracts. Contextual grants are re-evaluated inside
the mutation transaction and only five early no-external-effect transitions are implementation-
enabled. It is not a complete phase-3 cutover: legacy lifecycle handlers remain, mobile generation is
unavailable, external invitation conversion is incomplete, and transitions requiring public,
booking, ticket, contract, notification, or financial effects deliberately fail closed.

The next dependent branch begins phase 4 with a compatibility-preserving correction to the existing
social-event logistics handler: an activity row and its complete assignment/dependency snapshot now
commit or roll back together. PostgreSQL tests cover cyclic-update and cross-event-create failures.
This is only phase-4 transaction hardening; RACI HTTP commands, templates, workstreams, checklists,
recurrence, readiness, typed production requirements, and workspace views remain unimplemented.

The fourth branch, `feat/event-task-commit-invariants`, refines the transaction model and adds
deferred final-state guards with a per-event write fence. It closes same-transaction dependency
replacement and concurrent RACI-removal gaps and tests RC/RR/SERIALIZABLE writers, rollback and
reapply. The complete operation contract and bounded results are linked from the specification
index. This correction does not advance phase 4 to complete.

`fix/event-command-replay-authorization` is the next dependent security correction: stored receipts
are reauthorized, permission edits serialize with commands on an authorization epoch, and wall-clock
expiry is checked after lock acquisition. The existing unmerged API migration is corrected in place
because it has never entered the production manifest. Scope administration, consistent GET snapshot
authorization, sensitive exception logging, full HTTP/offline tests and the remaining product phases
are still required before activation.

The sixth branch, `fix/event-snapshot-privacy-boundary`, addresses the preceding GET/logging gaps.
The bounded `SnapshotRead` model and three expected counterexamples precede the implementation.
Snapshots now share the permission-write fence, use one post-lock instant and suppress self-approval;
database failures discard sensitive payloads without swallowing cancellation. Real database races,
strict DTO decoding and the production PostgreSQL adapter have focused tests. This remains phase-3
security hardening, not completion of scope administration, HTTP/offline verification or product UX.

The seventh branch, `test/event-operations-http-boundary`, adds focused real HTTP verification of
production authentication, the event subrouter and PostgreSQL functions. Sixteen scenarios cover
credentials/roles, object IDs, strict commands, replay/concurrency, between-request revocation,
approval, feature gating and failure/recovery. Runner safety and path-selection tests integrate
the suite into the existing backend CI gate without weakening it. No business schema/API changes
are introduced. Before activation, close the separately documented in-flight session window and
review POST event-existence metadata; full `mkApp`, offline, schema rehearsal and UX remain pending.

The eighth branch, `fix/event-command-existence-privacy`, closes that POST response-envelope gap.
`CommandPrivacy` and two negative configurations were checked before changing the unreadable SQL
paths to the absent-target envelope. SQL regressions reproduce the old leak and now pass, including
immutable diagnostics and isolation/rollback tests. The HTTP suite now has 18 passing scenarios,
with exact status/body/non-Date-header comparisons and preserved readable-only mutation denials.
In-flight token revocation, timing channels and the remaining end-to-end phases are still pending.

The ninth branch, `fix/event-session-transaction-fence`, closes current-token revocation between
authentication and the event transaction. It reuses `api_token` without a migration, adds an opaque
request-local witness and locks/revalidates that row throughout reads/new commands/replays. The
bounded model and six mutation controls preceded implementation. Production HTTP barriers and
PostgreSQL lock observations complement captured-context tests. This does not complete global
role revocation, permanent token invalidation, other domains, full-app integration or the product.

The tenth branch, `test/event-operations-schema-rehearsal`, adds a strict full-repository-schema
rehearsal beside the reduced-fixture tests. It reproduces a pre-existing missing storefront
migration dependency before reaching event SQL; an explicit supplemented diagnostic also exposes
the provider/refund schema contract failure. Hosted verification never uses the diagnostic option.
See [PR 10 evidence and limitations](pr-10-schema-rehearsal.md). This is an additional compatibility
gate, not a green migration/release claim or completion of phase 3/4.

The eleventh branch, `fix/event-schema-migration-dependencies`, registers four unchanged existing
migrations required by the authoritative schema contract: storefronts before merch reputation,
canonical payments before attempt binding, and account onboarding. One identical duplicate
reputation entry is consolidated; all unique prior registrations and SQL checksums are preserved.
It extends the strict rehearsal with ledger retry and disabled-provider/
financial-setting preservation checks. See [PR 11 evidence and release blockers](pr-11-schema-dependencies.md).
This does not activate event operations or providers, and does not complete the remaining phases.

The twelfth branch, `test/merch-expiry-checkout-contract`, corrects the stale owning-storefront test
expectation using the existing checkout-count contract. MX-01–05 add exact-boundary, projection,
paid-evidence, retry and expired-reservation rejection checks, allowing the unchanged downstream
refund/settlement/rollback suite to execute. The owning runner is added to migration CI with path
selection regressions. See [PR 12 evidence and limitations](pr-12-merch-expiry-contract.md).

The thirteenth branch, `feat/event-task-read-projection`, begins the phase-4 read boundary
without adding another task system. It projects canonical task/policy/current-RACI data under
exact task/event grants, with current-time authorization after shared locks. Assignment alone
and event.read/finance permissions do not disclose task state. New TLC/Alloy checks precede
SQL, and disposable PostgreSQL tests cover permissions, races and reversible migration on
reduced and complete schemas. See [PR 13 scope and limitations](pr-13-task-read-projection.md).
The public task API, typed consumers, aggregate write tokens and workspace views remain next.

The fourteenth branch, `feat/event-task-read-api`, exposes the existing SQL read through the
production authenticated subrouter, strict nested JSON/identity validation, no-store headers
and a generated-type/runtime-validated web client. A real HTTP regression fails before the
route exists, then passes alongside the earlier session/lifecycle tests. No new SQL is added.
See [PR 14 evidence and blockers](pr-14-task-read-api.md): the full UI typecheck currently fails
on two unchanged onboarding imports; focused task client verification is not a substitute.
Mobile, current-head full backend build, workspace views and remaining phases were incomplete
at that checkpoint; the follow-up below records fresh verification.

The fifteenth branch, `fix/web-onboarding-reconciliation-integration`, repairs those obsolete
imports against the existing canonical server-evidence onboarding domain. Its new bounded
receipt/coalescing model precedes implementation. `SessionProvider` owns reconnect recovery;
Shell retains intent recovery and signup no longer calls removed local-marker functions.
The whole-web application typecheck now passes. The stable PR-14 backend compile-only rerun
also finished successfully. See [PR 15 verification and limitations](pr-15-web-onboarding-integration.md)
for independent component and whole-repository gate results. Neither result establishes
event workspace UX, mobile compatibility, full backend tests or end-to-end completion.

The sixteenth branch, `fix/artist-follow-onboarding-continuity`, restores the missing
explicit-consent artist-return flow against canonical Fans and server onboarding APIs.
A new bounded consent/context model precedes implementation; URL, rendered interaction
and synthetic browser tests cover retries and stale callbacks. See [PR 16 evidence and
limits](pr-16-artist-follow-continuity.md). This shared-domain compatibility repair does
not complete event talent discovery/hiring, FanHub onboarding, mobile or the product.

The seventeenth branch, `fix/fanhub-authoritative-onboarding`, repairs the five reproduced
FanHub failures using canonical eligibility and explicit optional exit. Its finite model
precedes implementation; current-session receipts, strict decoding, retry and accessible
states replace the global dismissal marker without migrating or deleting stored data.
See [PR 17 verification and limits](pr-17-fanhub-onboarding.md). It does not complete the
remaining event workspace, engagement, payment, offline or mobile phases.

The eighteenth branch, `fix/reservation-pluscal-integrity`, closes the reported source/
translation warning: two stripped generated trailing spaces, not different booking rules.
Fixed-width regeneration and a byte-exact, temporary-copy gate now precede TLC; real-translator
mutation tests run in the same CI job. See [PR 18 evidence and limits](pr-18-pluscal-integrity.md).
This hardens phase-2 evidence without advancing incomplete booking or workspace implementation.

The nineteenth branch, `feat/event-task-raci-view`, adds a read-only task/RACI subview to the
existing registered event route and links it from logistics. Its lightweight dispatcher does
not import or mount the ordinary event overview for task links. The new bounded `TaskView`
model precedes implementation; the existing exact-task API remains the only data source.
Context-generation fencing, explicit bearer binding, strict route/DTO validation and abort
cleanup protect rendered receipts. This is not a task editor, rich plan, readiness gate or
mobile implementation; see [the scoped read contract](task-view-contract.md).

The twentieth branch, `test/event-task-browser-startup`, investigates PR 19's local browser
timeouts. It removes local Vite module traffic from the test-runner callback, while retaining
API interception, foreign-origin blocking and the original assertion limits. The application
and formal state machines remain unchanged. See [PR 20 evidence](pr-20-task-browser-startup.md)
and the [fixture isolation contract](task-browser-isolation-contract.md). This is a browser
harness correction, not a production startup benchmark or server/database E2E completion.

The twenty-first branch, `feat/event-task-aggregate-revision`, prepares optimistic task/RACI
commands with task-scoped version metadata and the existing event write fence. A separate
internal guard compares after waiting; public task reads and clients remain unchanged.
See [the revision contract](task-revision-contract.md) and [PR 21 evidence](pr-21-task-aggregate-revision.md).
This database-only foundation does not enable editing, expose versions to scoped readers,
implement idempotent commands, or replace current authorization and final-state validation.

The twenty-second branch, `feat/event-task-revisioned-read`, builds on PR 21 with an opt-in
authenticated read envelope and exact decimal-string revision. It shares metadata locking
with existing transactional tracking and reuses the old canonical projector. Old task JSON,
UI behavior and production activation remain unchanged. See [the contract](task-revisioned-read-contract.md)
and [PR 22 evidence](pr-22-task-revisioned-read.md). Authenticated task/RACI command receipts,
immutable mutation audit, editor conflict handling and mobile adoption remain separate work.

The twenty-third branch, `feat/event-raci-reassignment-command`, adds a private planning-stage
single-pair RACI reassignment on the canonical task/RACI tables. Current scoped authorization,
aggregate revision, task-scoped keys in the existing receipt ledger and immutable audit compose
atomically. [The contract](raci-reassignment-contract.md) precedes feature SQL; see
[PR 23 evidence](pr-23-raci-reassignment-command.md). The next boundary is an authenticated,
strictly typed HTTP command with transaction-local session revalidation, followed by explicit
editor conflicts/consent. No public command, notification, native mobile or production activation
is included in this database increment.

The twenty-fourth branch, `feat/event-raci-reassignment-api`, composes the private RACI
command with the existing authenticated session transaction and validates the exact receipt
before commit. Adds strict Haskell/OpenAPI/generated TypeScript transport and a validating web
client without changing any page. [The HTTP contract](raci-api-contract.md) and
[PR 24 evidence](pr-24-raci-reassignment-api.md) record bounds, rollback and verified behavior.
Next: scoped editor/readiness/eligibility UX with explicit conflict/consent handling; broader
RACI and mobile/offline/notification workflows remain separate, and hosted failures must be
resolved before claiming complete CI or production readiness.

The twenty-fifth branch, `feat/event-raci-editor-context`, closes the missing authorization
and recipient-discovery prerequisite identified before building the editor. An additive
session-fenced read exposes current manager authority, conservative replaceable source pairs
and paginated eligible Party IDs, without contacts, new grants or mutation. Its coherent
revision is advisory and cannot replace command-time checks. See [the contract](raci-editor-context-contract.md)
and [PR 25 evidence](pr-25-raci-editor-context.md). Next: the scoped web selector, deliberate
confirmation and explicit stale/conflict handling; no UI, native mobile or production activation
is included here. Naming/consent policies must not be inferred from access eligibility.

The twenty-sixth branch, `feat/event-raci-web-editor`, adds the editor inside the existing
task subview, using PR 25 context and the existing idempotent command. Page changes discard
old options; review freezes a justified single-pair request; explicit confirmation is single-flight.
Initial conflicts require a new read/review, while uncertain results retain the exact key/body
for explicit retry. Session/navigation generations discard old receipts. [The contract](raci-web-editor-contract.md)
and [PR 26 evidence](pr-26-raci-web-editor.md) delimit the finite checks and synthetic browser
evidence. No names, consent, timed reassignment, durable offline recovery, native mobile or
notification claim; no production flags change. Next dependencies include those workflows,
full-stack browser verification and resolving existing hosted integration failures.

After the real RACI browser and web compatibility/accessibility increments (PRs
27–30), `feat/event-task-completion-command` resumes phase 4 with a private guarded
completion primitive for opted-in preparation tasks. The new bounded model and
seven mutations precede SQL; the command reuses all existing task identities,
authorization, write fences, versions and immutable evidence. See [PR 31](pr-31-task-completion-command.md).
Next: session-fenced completion HTTP/client contracts, then scoped review/readiness
and explicit completion UX. Approval/evidence, override, reopening and live-event
effects remain distinct contracts; none may be inferred from this preparation command.
