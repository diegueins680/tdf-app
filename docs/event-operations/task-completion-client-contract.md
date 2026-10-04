# Completion client contract — before implementation

Depends on draft #410, exact `823da0fa801abe3bdb3b7679550f1315ad0946ed`.
Reuse generated types, existing strict scalar validators and the shared `post`
transport. No route/schema/database/authorization/feature flag or UI changes.

| ID | Executable contract |
| --- | --- |
| CC01 | `EventOperations.completeTask(eventId, activityId, commandId, command, context?)` validates positive safe IDs, UUID, exact nonnull command fields, canonical positive BIGINT revision text, nonblank reason <=2000 and correlation <=200 Unicode code points. Invalid input throws before dispatch. No actor, override or unknown property accepted. Preserve accepted strings unchanged. |
| CC02 | Parse into a fresh scalar snapshot before POST. Capture explicit key/bearer/signal in options, use no-store and only the canonical `/complete` path. Caller mutations after invocation cannot change the serialized request or response binding. No implicit GET, retry, rebase or compensation. |
| CC03 | Accept only a strict receipt matching event/task/UUID (case-insensitive UUID comparison), literal completed status, activityVersion in 2..2147483647, exact positive BIGINT revision equal to the captured expected+1, and boolean replayed. Validate historical replay against its original request without a new read. Malformed/foreign/extra-field response rejects with a fixed Spanish message, not raw payload or decoder diagnostics. |
| CC04 | Transport rejection/abort is propagated; the helper never claims rollback or server non-execution. After an ambiguous result the caller must retain and retry exactly the original request/key with current authorization. An explicit second invocation is distinct from an automatic retry. |
| CC05 | Library only. No persisted token/queue, task UI, permission/readiness inference, fake success, native mobile or production activation. Future UI must add reviewed consent, current view/session checks and conflict handling; this helper does not solve them. |

`TaskCompletionClient` abstracts one invocation, two caller-request identities,
two candidate receipt identities, valid/invalid shapes and at most two dispatches.
Caller mutation may interleave after capture. Transport failure is ambiguous about
server commit. Safety invariants: OriginalRequestSent, ValidatedReceipt, SingleDispatch.
Four negative controls remove capture, shape, binding or no-retry. There is no
fairness/liveness or client-controlled rollback claim. Text/UUID/BIGINT schemas,
fetch options, serialization and cancellation require executable tests.

Server authorization/atomicity remains the unchanged `TaskCompletion`, `SessionFence`,
`CommandBoundary`, `ReceiptReplay` and scoped Alloy contracts, fully checked at the
exact parent before its implementation (24 positive TLC / 60 negative controls,
13 PlusCal tests, 2 SAT scenarios / 13 UNSAT assertions; see PR 32). This increment
adds no relational entity or permission. Reuse that recorded evidence only for
unchanged models, and run the new model/controls before client feature code. Wire
them into the complete mandatory CI suite; finite checks are not universal proofs.

Tests: strict invalid inputs and receipts, code-point limits, exact maximum revision,
all target/key/status mismatches, captured body/context mutation, retry identity,
single network call on errors/abort, and real shared transport serialization with
a synthetic fetch endpoint. Mocked transport tests are not backend/browser E2E.
Existing API and task/workspace page tests must remain compatible.

Rollback removes only the unused helper/types; do not delete accepted server
receipts or attempt inverse task mutation. Migration/provider configuration unchanged.

## Verification checkpoint

Before feature code on 2026-09-16: positive TLC exit 0, **44 generated / 32
distinct states, depth 4**. All four mutations exited 12 with the required named
invariant. The first sandbox attempt exited 255 because TLC's local RMI listener
was denied; approved local runtime access completed all checks. No failed check
was accepted. JAR SHA-256 matched the pinned full runner; existing model files
were unchanged from the fully checked parent. The full Alloy/TLC suite was not
rerun locally for this client-only increment; mandatory CI now includes these
additional checks. Receipt identity abstracts only the bound target/key/revision;
reason/correlation/actor binding remains the trusted server command/receipt policy.

Exact per-config command (each run used a separate subdirectory under an owned
`mktemp -d /private/tmp/tdf-completion-client-model.XXXXXX` directory):

```sh
/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  -XX:+UseParallelGC -jar /private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  -workers 1 -metadir /private/tmp/tdf-completion-client-model.cDW2Q8/positive \
  -config TaskCompletionClient.cfg TaskCompletionClient.tla
```

Working directory: `formal/event-operations`. Config substitutions and required
exit-12 invariants: `TaskCompletionClientCapture.cfg` → OriginalRequestSent;
`TaskCompletionClientShape.cfg` and `TaskCompletionClientBinding.cfg` →
ValidatedReceipt; `TaskCompletionClientRetry.cfg` → SingleDispatch.
