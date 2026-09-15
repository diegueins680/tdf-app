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
