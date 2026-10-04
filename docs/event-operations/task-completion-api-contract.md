# Authenticated preparation-task completion — pre-implementation contract

Depends on draft #407 at `78bdd4df465d1c7d8a752f9df0ed714ecad2330d`.
Reuses the private completion command, canonical task/authority/RACI/version rows,
receipt ledger, and transaction-local `withCurrentAuthSession`. No new domain,
SQL migration, production activation, user interface or provider behavior.

`POST /event-operations/events/{eventId}/tasks/{activityId}/complete`
requires current bearer/cookie authentication and a UUID `Idempotency-Key` header.
Exact JSON fields: `expectedRevision` (positive canonical BIGINT decimal string),
`reason` (nonblank, <=2000 characters) and `correlationId` (nonblank, <=200).
Reject unknown/null fields, unsafe/nonpositive target IDs, malformed revision/key.
The body cannot choose actor, timestamp, hash, permissions, or an override.
Whitespace-only transport text is rejected; accepted text is preserved unchanged.

| ID | Operation/invariant and executable obligation |
| --- | --- |
| CA01 | Revalidate and lock the bound current session inside the same transaction as completion, receipt decoding and commit. Actor is derived only from the authenticated session. Revocation before that fence rejects both new commands and historical replay. |
| CA02 | Decode exactly one nonnull SQL result before commit. Accept only an exact allowlisted error object or strict receipt bound to event/task/key, status `completed`, PostgreSQL INTEGER activity version in 2..2147483647, and aggregate revision exactly expected+1 using Integer arithmetic. Malformed/foreign/overflow receipts throw inside the transaction, rolling back newly staged business/audit/receipt effects. |
| CA03 | Return 200 only after successful commit. DB/commit/unknown-response faults produce sanitized 503. No automatic retries, rebasing, fallback legacy writes or compensation. Handler success/errors use `Cache-Control: private, no-store`; framework parsing/authentication retains existing behavior. |
| CA04 | Errors: invalid_request400; feature_disabled/not_found404; forbidden403; version_conflict/idempotency_conflict/operation_not_ready/accountability_not_ready/dependencies_not_ready409. Preserve generic hidden-target/dependency failures. Session rejection401. |
| CA05 | Additive Servant/OpenAPI/generated web types. No hand-written client command or UI activation in this increment. Exact historical replay is validated against its original request, not a newer task read. Network ambiguity requires later callers to retain and retry that exact body/key. |

Applicable models, without changing their state spaces: `TaskCompletion` (two
attempts/one task/two keys/clock0..3), `SessionFence` (session mutation vs protected
transaction), `CommandBoundary` (one command/four validity combinations/commit or
abort), `ReceiptReplay`, `TaskRevision` and scoped Alloy task/ownership models.
The early-commit and unbound-receipt mutations must still fail `ValidatedCommit`;
completion's seven controls must still fail their named invariants. Run the full
pinned TLC/Alloy suite before implementation and record results below. These are
finite checks, not universal proofs or a proof of Haskell/PostgreSQL refinement;
no new fairness/liveness claim. SQL correctness and recognized no-write rejections
remain trusted. A compromised SQL function fabricating recognized errors is not
addressed by the receipt boundary.

Tests must cover strict types, exact BIGINT arithmetic, valid and stale commands,
hidden targets, permissions, dependencies/RACI, replay, concurrent retries,
post-authentication revocation, malformed post-write receipts and deferred COMMIT
failure with verified rollback of business/audit/receipts, disabled flag and
missing SQL prerequisite. Use the existing real-auth/subrouter Stack harness and
disposable PostgreSQL, never production. Reuse existing JSON parsing; do not claim
duplicate-key detection or new global body/rate limits.

Rollback: remove/disable the route before dropping the private SQL function;
preserve accepted task state, revisions, receipts and audit. The prerequisite
remains absent from the production manifest. Approvals/evidence, overrides,
reopening, live-event behavior, browser UX, native mobile and offline remain
separate dependent contracts.

Refinement trace; executed results are recorded in [PR 32](pr-32-task-completion-api.md):

| Requirement | Contract/model | Implementation target | Test target |
| --- | --- | --- | --- |
| EO-045/051 | CA01, `SessionFence` / scoped Alloy | Existing session-fenced handler | `SessionFenceSpec`: real post-authentication completion/replay revocation |
| EO-026–028/055–058 | CA02–04, `TaskCompletion`, `CommandBoundary`, `ReceiptReplay` | Strict DTO and transactional database adapter | `TypesSpec`, `DatabaseBoundarySpec`, `EventOperationsHttpMain`: guard/receipt/commit faults, concurrency and exact revision properties |
| EO-052–055 | CA05, executable schema contract | Additive OpenAPI and generated web declarations | Generated type check, unchanged existing client tests, runner prerequisite guards |

## Formal gate

Executed before feature implementation on 2026-09-16, exit 0: 24 positive TLC
configurations, 60 named negative controls, 13 PlusCal integrity tests, 2 SAT
Alloy scenarios and 13 UNSAT assertions. `TaskCompletion`: 944,014 generated /
231,576 distinct states, depth 14. `CommandBoundary`: 14 distinct states, depth 4;
both early/unbound mutations detected. `SessionFence`: 12,685 generated / 1,153
distinct states, depth 5; all six authority mutations detected. Exact command:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```
