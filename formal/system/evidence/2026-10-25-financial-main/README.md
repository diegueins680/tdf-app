# Financial candidate qualification

Qualified source: `b71ac3fad0fc86195ceea87c09c45b16847875f8`.
Protected main observed: `b732efd65de23e5b97f3f619a2242397c64965c3`.
This is a dated source snapshot, not deployed-system or future-head evidence.

[Scorecard](scorecard.json), [delivery](delivery.json),
[counterexamples](counterexamples.json), and [unresolved obligations](backlog.json)
separate the delivered expiry correction (#609) from the financial fixes still
awaiting protected-main admission through #551. Children #612 and #617 were
normally merged into that draft, preserving migration introduction ancestry.
Historical #548, #581 and #598 are already in main; their original receipts
retain their original source identities and are not reclassified as new runs.

| Evidence | Observed result | Limit |
| --- | --- | --- |
| Registered requirements | 140 total; 113 critical; all PARTIAL | No complete requirement proven |
| Implementation/refinement | 280 unresolved obligations | Tests/models do not discharge them |
| Source-derived SMT | 11 conditional claims | Trusted translation and named assumptions |
| Independent CPC | 14 obligations, 12 distinct inputs | F8 execution retained; all 41 generated candidate inputs match exactly |
| TLC | 120 positive configurations; 353 detected controls | Referenced finite scopes |
| Alloy | 2 scenarios; 13 assertions; 15 detected controls | Referenced finite scopes |
| Compiled aggregate | 4,039 examples; zero failures; 21 pending | Pending external runners are not passes |
| Required Music entry groups | 61 cases; zero pending | Subset of the aggregate, checked for discovery |
| Full-schema native groups | 79 cases; zero failures/pending | Named ledger, ticket, purchase, publication and privacy cases |

All 220 migrations apply/replay and the Music rollback/reapply cycles pass on
owned PostgreSQL fixtures. Exact ledger arithmetic, terms retention, checkout
snapshots, 27 positive MVCC schedules and six lock-removal schedules pass.
Ordinary-role namespace controls and direct migration replay/refusal are
recorded in the earlier namespace receipt. Startup and path-selection evidence
is in the [reviewed child receipt](../2026-10-25-ledger-startup/ci-selection-receipt.json).
The updated provider fixture also passes the real expiry-store regression and
its three clock mutants with both namespace settings installed.

The native executable is identified with F9. Every backend source and migration
is byte-identical to that tested source. The new startup/selector inputs match
their separately qualified F8 source. The protected UI and exact Mobile pin
are retained. These input checks do not claim a new binary build, new checker
execution, complete refinement or final-head hosted success.

[Source inputs](source-inputs.json) and the scorecard bind evidence to exact
commits and hashes. [Fixture failures](fixture-failures.json) retain rejected
qualification setups: the authenticated Music URL, inet `/32` formatting, and
the aggregate's initial wrong working directory. Their corrected runs pass;
project tests and protective guards were not weakened. Owned fixtures are
removed/stopped. Plain logs disclose normalization separately from raw hashes;
compressed logs retain the raw executions.

Reproduction commands are in the scorecard. Included external drivers require
fresh owned fixture/output names and their recorded working directories.
The canonical local Stack build completed for the server, test executable and
DDEX renderer; exact binary hashes and the compressed build log are recorded.
Hosted #617 independently completed the backend lane, including provider retry,
staff settlement, and real HTTP authorization/rollback controls. Final #551 head CI and independent protected-main
review remain required. No backend deployment, real charge, production migration
or financial activation was performed.
