# Ledger header qualification on protected main

Qualified clean source: `db2ec17c1fc2cd934d523a434fa833d286900a2c`. Protected main:
`e938c72ba4b87010214f6838f06bfac45a0f45c0`. All 221 preceding registry
entries and historical SQL bodies are unchanged. The header guard is migration
222, introduced by `5cc26ad46c958a029ec4539c60cc1ca77fec1d01`.

Fresh canonical Stack LTS24.42 / GHC9.10.3 builds server, DDEX renderer and tests.
Aggregate: 4,039 examples, zero failures, 21 explicit external-runner pending cases.
The canonical ledger runner executes ten real PostgreSQL examples without pending
cases, 16 ordinary-role protected-field mutations, four startup mutations,
namespace/legacy/reversal controls, and 37 snapshot cases: 30 positive, seven
controls. Three ordinary header writers wait behind posting, witnessed through
pg_blocking_pids: RC rejects with 23514; RR/SERIALIZABLE reject with 40001.
Removing the header guard commits the source-key violation. Definitions and
receipts restore exactly. All 222 migrations apply/replay. Owned fixture
clusters are stopped and databases dropped; no production operation occurs.

Frozen main's complete 221-migration schema independently admits the original
UPDATE in a rollback-scoped fixture. That baseline receipt retains its actual
pre-merge regression-harness identity and hashes, distinct from the clean current
qualification. The current orchestrator uses explicit checks. Four failure/marker
controls pass under normal and optimized Python; actual qualification runs under
python3 -O. Earlier local orchestrators are historical artifacts of their recorded
normal-Python executions.

TLC 1.7.2 checks one transaction, two values and two queued writers: 48 distinct,
138 generated states. Guard removal exits 12 on PostedHeaderFrozen. Repository
quality and fresh source-derived SMT reporting pass. All 142 complete requirements
and 284 implementation/refinement obligations remain UNPROVEN, separately from
eleven conditional SMT claims. No local independent CPC replay or whole-system,
SQL refinement, unbounded liveness or deployed equivalence proof is claimed.

Both previous 221-entry catalog source reviews are archived by exact hashes.
The active 222-entry successor preserves all other 1,327 decisions. Historical
213/214/220 checks remain, and both obsolete 221-entry reviews fail the gate.

verification-artifacts.tar.gz contains raw logs, serialized SMT inputs/replay,
exact local drivers, counterexamples and tool records. snapshot-receipt.json
identifies its own portfolio separately from other JSON-emitting suites in the
canonical log. A packaging assertion initially mixed those suite records; it
failed and was corrected before publication, without changing a test outcome.
Replay the preceding dossier's canonical commands on this committed source and
a newly owned disposable _test database; they now consume the 222-entry manifest.
Local driver paths/ports describe this machine. CI, current-head independent
approval, normal-merge ancestry and production approval remain separate gates.
