# PR draft: Reauthorize lifecycle receipt replay

## Scope and dependency

Base: `feat/event-task-commit-invariants`, PR 339, implementation/documentation head
`35f261097d9e295794f4db7404ae7d9556a8e9d4`. This closes the SQL replay disclosure identified
in PR 337 without adding another event service or changing request/response DTOs.

## Requirement → invariant → implementation → test

EO-045/EO-051 map to `ReceiptReplay.NoUnauthorizedDisclosure` and
`ReceiptBindingPreserved`. The model passed before the SQL implementation changed. Three negative
controls independently detect bypassed authorization, stale transaction-start time and stale scope
snapshots. See [the complete operation contract](receipt-replay-contract.md).

`event_operation_apply_transition` now checks current read authority before disclosing an existing
receipt or a command-key conflict. Denied attempts append audit without changing the original
receipt. Authorized exact replays return the original historical result without another transition.
Read-only downgrades permit historical reads but not new mutation commands. Write authority and
read authority use `clock_timestamp()` rather than transaction-start `now()`.

Scope/ownership inserts, updates and deletes advance `authorization_version` on the event-state
row, serializing them with its existing command lock. Business `version` is unchanged by permission
edits. READ COMMITTED sees a preceding revocation; RR/Serializable stale writers abort with 40001.
Scope identities and event/party movement fail closed; audited revoke/issue APIs remain pending.

## Migration, compatibility, security and rollback

- Corrects the unmerged `2026-09-14_event_operations_api.sql`, outside the production manifest.
  No deployed migration/checksum is rewritten and no production migration is run.
- Adds one authorization epoch column and two triggers to the existing domain. No duplicate service,
  Haskell/Servant route, UI, generated client or public schema is introduced.
- Unreadable command-key conflicts now return the existing `forbidden` code, not
  `idempotency_conflict`. This prevents that response from disclosing whether a receipt exists.
- Rollback disables the feature and drops the command function; it preserves receipts, audit,
  authorization epoch and scope-write guards. Apply twice, rollback twice and reapply are tested.
- Do not restore the unsafe old replay function while the feature is enabled. No production
  activation, provider action, merge or branch-protection bypass is authorized by this change.

## Completed local verification

- Full pinned TLC/Alloy runner: PASS. New model: 2,866 generated / 1,164 distinct states, depth 8;
  three expected TLC exit-12 disclosure counterexamples. Alloy scenario SAT and all 8 checks UNSAT
  within their existing documented bounds. Finite analysis is not an unbounded proof.
- Before the SQL fix, the new PostgreSQL regression failed with
  `revoked replay disclosed a receipt`, including the original state/version. This was reproduced,
  not inferred solely from code inspection.
- `sh scripts/test-event-operations-api-migration.sh`: PASS on disposable PostgreSQL 16. Covers
  revoked/expired/future grants, expired write authority with surviving read access, read-only
  historical replay, actor/hash/event binding, ownership removal/coproduction downgrade, rejection
  receipt privacy, immutable receipts, scope-identity guards, simultaneous exact replay,
  deterministic revocation/replay races under RC/RR/Serializable, no duplicate effects, durable
  denial audit and rollback/reapply preservation. Existing lifecycle tests remain in the suite.
- `sh scripts/test-event-task-commit-migration.sh`: PASS, including the preceding task/RACI races.
- `npm run verify:formal`: PASS, 0 critical/errors and 351 advisory warnings.
- `npm run test:formal`: PASS, 4/4.
- Shell syntax, workflow YAML and whitespace checks: PASS.

No new Haskell compilation, HTTP integration, browser, mobile, offline-queue, accessibility,
provider sandbox or performance result is claimed. Database tests exercise the real command
function but do not replace an authenticated HTTP end-to-end test.

## Remaining limits

The separate GET snapshot handler, exception-log redaction, grant-administration authorization/audit,
guest conversion, full offline synchronization and the broader product phases are not complete.
The model does not cover global feature-toggle concurrency, every legacy event handler or network
delivery after authorization. A later revocation cannot retract a previously authorized response.

PR 339's hosted formal/DB jobs and API-contract checks passed at the inspected head, but its full CI
was not green: repository/UI/persona/hardcoded-list/general-migration gates and preview checks had
failures, with backend checking still running. Those failures were not waived or reclassified as
unrelated. This PR's exact-head hosted results must be checked separately after publication.
