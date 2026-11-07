# Ledger namespace repair checkpoint

Implementation source: `d65651ae97d4135da7187f7915fe0a3fa6ca34c0`.
Prerequisite: draft #551 at `59df1dd2a17952b906cf89d1c5502725d51239e1`.
This package qualifies a child of that draft; it does not qualify current main
or a production deployment. [Machine-readable scorecard](receipt.json).

| Measurement | Evidence |
| --- | --- |
| Source requirements | 135 total, 110 critical; all complete requirements remain unproven |
| Implementation/refinement obligations | 135 + 135 unproven |
| Existing conditional SMT obligations | 4 arithmetic, 3 authorization, 4 recovery PROVEN under their recorded translation assumptions |
| Financial correction | Two non-owner shadowing counterexamples reproduced and repaired; zero integrated at this checkpoint |
| Assurance | TESTED installed SQL guards; BOUNDED_VERIFIED abstraction; SYS-MONEY-003 remains PARTIAL |
| Schema | All 219 parent migration objects/SQL preserved; one additive entry; full 220 apply/replay and two music rollback/reapply cycles pass |
| Concurrency | Existing 27 positive and 6 deliberate write-fence mutation schedules pass with their expected results |
| Namespace mutations | Two function-lookup controls reproduce their named failure and restore exact definitions |
| Model | 22 generated, 12 distinct states; both negative configurations violate PostedBalanced |
| Verification pipeline | Legacy prototypes demonstrate false success under Python optimization and after predecessor replay loses function settings; repaired checks reject/preserve as required |
| Delivery | Independent child review/current-head hosted CI and later combined-parent qualification remain required |

[Baseline observations](baseline-counterexamples.json) record actual `posted|1`
states for a normal database role with TEMP, ledger DML, no public CREATE and no
table ownership. The installed-guard tests additionally retain the existing
identity-reference guard and grant its required archive-table SELECT only to the
transaction-scoped synthetic actor. Runtime production privileges are unchanged.

[PostgreSQL evidence](postgres-receipt.json) binds the exact clean source, all
220 migration hashes, harness hashes, tools, named steps and owned-database
cleanup. [Model evidence](bounded-model.json) retains its original execution
identity; its exact module/configuration inputs match the implementation source.
[The canonical assurance matrix](assurance-report.md) retains its source identity
and full-system release hold. The first full-schema attempt failed on the missing
fixture read privilege; it is recorded as a failure in the scorecard and the
complete corrected fixture was rerun.

The local replay driver used only new loopback PostgreSQL16.13 clusters. Its
legacy-harness controls load immutable Git revisions 7800d8e8 and 1d4d4f11 as
controlled variants, then compare real installed function definitions. Their own
historical/prototype success receipts are not admitted as current evidence.
Python optimization must not disable the new verifier's failure checks.

Replay commands and remaining assumptions are in [receipt.json](receipt.json)
and [the scoped contract](../../ledger-namespace.md). The existing CI ledger lane
runs the namespace checks after legacy/MVCC coverage; the formal lane includes
the positive model and both named controls. Local PostgreSQL16 results do not
substitute for hosted PostgreSQL17 qualification or a production privilege audit.

Trusted-schema ownership, privileged DDL, all financial writers, arbitrary MVCC
histories and whole-program refinement remain open. Retain the additive guard
through application rollback. No production migration, deployment, provider
activation or real payment occurred.
