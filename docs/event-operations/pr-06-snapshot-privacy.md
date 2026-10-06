# PR draft: Consistent event snapshots and safe database errors

## Scope and dependency

Base: `fix/event-command-replay-authorization`, draft PR 341, head
`6b0913bab2b065b327149a6d3a042efecd9e75d7`. This closes the snapshot and database-error
disclosure gaps documented in PR 05, using the existing canonical event domain and API.

## Requirements and formal gate

EO-045/EO-051/EO-055 map to `SnapshotRead.NoUnauthorizedSnapshot`, `CoherentProjection` and
`LogFieldsAllowlisted`. The complete pinned TLC/Alloy runner passed before feature implementation.
The new positive configuration explores 1,133 generated / 296 distinct states, depth 12. Three
negative configurations each detect their intended invariant violation with TLC exit 12. All
preceding configurations pass; Alloy finds the valid scenario and no counterexample to its eight
assertions within the documented scopes. These are finite checks, not an unbounded proof.

See the [operation contract](snapshot-privacy-contract.md), [formal bounds](../../formal/event-operations/README.md)
and [traceability matrix](traceability-matrix.md). Feature-flag locking, concrete SQL isolation,
JSON decoding and asynchronous exceptions are covered by executable tests, not by the abstract model alone.

## Implementation, privacy and compatibility

- `event_operation_read_snapshot` acquires shared feature-flag and event-state locks, checks current
  read permission after any wait and uses one wall-clock instant for the complete projection.
  Scope edits share the prior authorization epoch fence. Stale RR/Serializable transactions abort.
- Capabilities and transitions are deterministically ordered. Self-approval, approval without a
  recorded requester and implementation-disabled transitions are not advertised. Commands still
  revalidate authority, state, version and required input independently.
- `DatabaseBoundary.loadSnapshot` executes the real function and strictly decodes the existing DTO.
  SQL NULL means unavailable/not found; malformed, unexpected or wrong-event results fail closed.
  The Servant handler uses this adapter. No public DTO, OpenAPI or generated-client change is needed.
- Database errors log only an allowlisted event and category. Raw exceptions, SQLSTATE bytes,
  SQL messages/details/hints and decode diagnostics are discarded. Asynchronous cancellation is
  rethrown so normal transaction/pool cleanup can proceed. The public 503/code response is unchanged.
- The optional integration harness connects only when explicitly given a DSN. The test script
  supplies a fresh disposable PostgreSQL container and a random loopback-only port, without loading
  project runtime credentials. Its hard-coded password is only a disposable test credential.

## Schema and rollback

Adds one SQL function to the unmerged API migration, still outside the production manifest. No
deployed checksum or existing user record is rewritten. Rollback disables the feature and drops
both read/write functions while retaining audit, receipts, authorization epoch and guards. Apply
twice, rollback twice and reapply are exercised with preservation and disabled-default assertions.
Do not re-enable an old unsafe handler after rollback. Production activation remains prohibited.

## Verification commands

From the repository root, with the pinned tools documented in the formal README:

```bash
JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
bash scripts/verify-event-operations-formal.sh
RUN_EVENT_OPERATIONS_HASKELL_TESTS=1 sh scripts/test-event-operations-api-migration.sh
sh scripts/test-event-task-commit-migration.sh
npm run verify:formal
npm run test:formal
```

The API script covers revoked/expired/future permissions, a stale-clock negative control, coherent
projection, approval visibility, real revocation races under RC/RR/Serializable, feature-disable
serialization, earlier command/replay guards, rollback and reapply. Its optional Stack/runghc
harness runs seven pure Hspec examples (including 100 QuickCheck cases) and three real PostgreSQL
adapter examples. Without the opt-in flag, CI's existing PostgreSQL job runs the SQL tests only;
the pure Hspec spec is also registered in the normal backend suite.

Completed local results on 2026-09-14: pinned TLC/Alloy PASS; final API migration/adapter suite
PASS (10 examples, 0 failures, 100 QuickCheck cases); task-commit regression PASS; `verify:formal`
PASS (0 critical/errors, 351 advisory warnings); `test:formal` PASS (4/4); shell syntax, workflow
YAML parsing and `git diff --check` PASS. An initial Haskell test did not compile because a synthetic
`SqlError` used bytes instead of libpq's `FatalError` status; this was corrected before the passing
runs. SQL assertions also explicitly reject an unexpected NULL for authorized snapshots.

`stack test tdf-hq:tdf-hq-test --fast --no-run-tests` was attempted from `tdf-hq` with the repository's
GHC 9.10.3 resolver. It started a full 209-module rebuild and was interrupted during compilation
(exit 130), not reported as passing or as a source error. The new database module, DTO and focused
spec were compiled and executed by Stack/runghc against real PostgreSQL; compilation of the full
Servant application and the complete backend suite remains unverified at this head. Existing
missing-home-module and `http2` bound warnings were not suppressed or repaired in this slice.

## Remaining limitations

These are production-adapter/database tests, not a full authenticated HTTP journey. Global legacy
authorization/logging, grant administration and audit, guest conversion, stale sessions, complete
offline queues and the remaining logistics/marketplace/finance/web/mobile phases are not complete.
A response authorized at projection cannot be retracted by a later revocation or expiry. No
browser, accessibility, performance or payment-sandbox result is claimed for this slice.

The earlier dependency chain had failing hosted repository/UI/migration/preview checks; no waiver,
green-CI, production-readiness or full end-to-end completion claim is made. No merge, production
deployment, live payment, credential change or manual preview deployment was performed.
