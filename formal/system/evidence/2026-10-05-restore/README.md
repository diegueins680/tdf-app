# Exact9b9d0e118 restore and migration checkpoint

The clean candidate9b9d0e118e25972a331eaa02056d21bb05bb2ea0 passed the repository gate
and complete bounded formal runner:57 positive TLC executions,125 expected negative
controls, one PlusCal regeneration with13 translator-integrity tests, and Alloy
2SAT/13UNSAT. The receipt binds unchanged source/tool/log hashes. These are bounded
model results, not a whole-program proof or a formal proof of the restore helper.

A real online production snapshot restored successfully into a resource-bounded,
network-isolated PostgreSQL17 container:652 relation counts matched the exported
snapshot. Its159-entry ledger advanced to169 through the exact candidate's ten
pending migrations; both canonical batch applications and schema verifications
passed. Historical ledger entries stayed unchanged. Existing reported revenue and
provider controls remained unchanged; three added flags and the added bank-transfer
provider were disabled. The isolate was removed before PASS. No production database
write, provider transaction or application deployment occurred.

The earlier default lock-table configuration failed during atomic restoration.
Private diagnostics classified PostgreSQL lock-table exhaustion. Increasing only
the isolate's max_locks_per_transaction to1024 retained the384MiB memory ceiling
and produced a successful restore. Failed attempts emitted no pass and left no
rehearsal containers. Raw archives and diagnostics stay private on the host.

Independent exact-source review ran13 Python and13 Node checks and recomputed the
receipt source/batch identities without findings. This is not GitHub approval.
The current Mobile pin remains the shipped-lineage c1d6031b36e52dd365e699163900e65adb0621e0.

Receipts are scoped evidence, not release clearance. Online database restoration
is not a coordinated database/assets backup, secret/off-host recovery, application
canary or safe rollback rehearsal. Runtime observations expire for future release
preparation. Full semantic coverage and the guarded routine release executor remain
open. Preserve these exclusions when using this checkpoint.
