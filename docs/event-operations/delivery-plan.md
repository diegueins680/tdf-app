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
