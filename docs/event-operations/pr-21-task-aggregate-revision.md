# PR 21 — Canonical task aggregate revisions

## Scope and dependency

Depends on draft [PR 375](https://github.com/diegueins680/tdf-app/pull/375), branch
`test/event-task-browser-startup`, exact base `221d1205c9cc2e992f8f36f73d441a3f9c6fda21`.
Branch: `feat/event-task-aggregate-revision`. Implements database-only RV-01–07 from the
[revision contract](task-revision-contract.md). No public endpoint, UI or activation change.

Audit evidence: `2026-09-14_event_task_commit.sql` already fences activity/dependency/policy/
RACI writers at event scope. `2026-09-14_event_task_read.sql` exposes separate activity/policy
versions; RACI changes do not advance either. A stale RACI editor must not treat the activity
version as an aggregate precondition. Reuse that existing fence rather than create a second
task or locking system, and keep its event-wide revision private.

The new PK/FK metadata relation stores a positive BIGINT revision per canonical activity.
Activity, RACI, policy, outgoing-dependency and immutable-override mutations advance it in
the same transaction. Legacy activity versions and existing rows are not rewritten. The
internal comparison function acquires the common write fence before loading the current
task revision; conflicts abort. It is SECURITY INVOKER with PUBLIC execution revoked.

This is not an authenticated command API. Future handlers must combine current session and
event/task authorization, feature checks, revision comparison, idempotency, immutable audit
and task validation in one transaction. Legacy commands are not silently upgraded into
aggregate-version-aware commands by this migration. Time expiry and prerequisite readiness
must still be evaluated at execution; version equality alone grants no authority.

## Formal verification before implementation

Completed full command on 2026-09-15, before adding either migration:

```bash
env \
  JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

Exit 0 and final success sentinel: **18 positive TLC configurations, 35 named negative
controls, 2 SAT Alloy scenarios, 11 UNSAT assertions**, plus exact PlusCal regeneration and
its 13 tests. New `TaskRevision`: **110 generated / 93 distinct states, depth 8**. Both
`TaskRevisionRaci` and `TaskRevisionEarly` exit 12 on `NoStaleCommit`. No fairness assumption,
universal proof, SQL refinement proof or new authorization relation is claimed.

The first run rejected an incompletely specified successor in the negative configuration:
the boolean assignment needed parentheses around its disjunction. That modeling defect was
fixed, not accepted as a counterexample. The entire runner was then rerun successfully before
feature code. Existing models and PlusCal translation remain unchanged. The host emitted Perl
locale fallback warnings; those are not formal translation failures.

## Executable verification

The new runner creates only its
own PostgreSQL 16 container, with `--network none`, no published ports or caller DSN. It tests
tracked mutations, unchanged legacy version, ABA, task isolation, rollback, overflow, no PUBLIC
execution, exact target binding, and observed competing-command waits under RC/RR/Serializable.
The existing PostgreSQL 17 complete-schema rehearsal now includes apply/reapply, retained
metadata on rollback and roll-forward; the authoritative production manifest stays unchanged.

The first focused DB run failed on a fixture-only feature activation lacking required actor
and reason. Those fields were supplied in the synthetic fixture; the real constraint was
not weakened. This failure is not counted as a passing migration/concurrency run.

After that fixture correction, `sh scripts/test-event-task-revision-migration.sh` passed with
exit 0, including all three isolation levels and aborted-command recovery. The expanded
final run also passed, exit 0: **six observed writer races** (guarded/legacy × RC/RR/Serializable)
each left exactly one committed revision advancement, plus successful recovery after the
first writer rolled back. No live DB or provider was contacted. Final diagnostic logs remain
local at `/var/folders/0s/0tg301f95s51dvsjf74ksxjm0000gn/T/tdf-task-revision.0M9tOS/`.

`bash scripts/test-event-operations-schema-rehearsal.sh` passed with exit 0 and its final
success sentinel, using the authoritative manifest without a diagnostic workaround. It
verified legacy columns/records and immutable audit/receipt/RACI history, exact unchanged
task JSON, revision 4 after two RACI inserts plus one policy, rollback preservation and
roll-forward without version reset. The final full-schema verification and unchanged
production ledger also passed.

`node --test scripts/__tests__/event-task-revision-runner.test.mjs
scripts/__tests__/event-operations-schema-rehearsal.test.mjs` passed **9 tests** twice after
CI wiring. Shell/Node syntax checks passed. No current-head Haskell build/full backend tests,
web/mobile builds, browser screenshots or complete UI tests are claimed by this DB-only change.

`npm run quality:repo` passed, exit 0: **146 tests** (8 + 3 + 42 + 4 + 61 + 23 + 2 + 3).
The heuristic audit reported 9,634 findings, zero critical/errors and 355 warnings; it is not
a formal model checker. Repository Git/release tests operated on disposable local fixtures,
not application main/production. All 86 checked relative documentation links, workflow YAML
parsing and staged whitespace checks passed. No generated fixture drift remained.
Hosted checks must be verified separately at the published head; no hosted pass is claimed.

## Migration, rollback, security and limits

Apply `2026-09-15_event_task_revision.sql` after foundation/API/task-commit/task-read, with
event operations disabled and writers quiesced during migration. It uses bounded migration
lock/statement timeouts. Schema changes are additive metadata and triggers, not replacement
tasks or duplicated RACI data. No ORM/generated-client/mobile change is needed. Keep this
migration out of the production manifest until reviewed rollout and command API are ready.

Rollback drops only the new comparison function. Counters and passive tracking deliberately
remain, including the added override write fence, so intervening legacy mutations cannot
resurrect old expected versions on reapply. It does not restore pre-migration trigger overhead.
Removing that metadata is a separate quiesced cleanup with explicit token invalidation; do
not reset counters in place. Existing canonical audit/receipt/history is untouched. Canonical
task IDs must not be reused after deletion; privileged identity or counter tampering is out
of scope. Existing deadlocks/serialization errors require whole-command retry or conflict,
never automatic rebasing of a user's expected revision.

Remaining: compose real authenticated/idempotent task/RACI commands; expose a coherent scoped
revision through typed APIs and clients; provide audit/approval/override flows and UI conflict
states; verify native mobile and offline workflows. Full event operations are not complete.
No production deployment, merge, credential activation or financial operation is performed.
