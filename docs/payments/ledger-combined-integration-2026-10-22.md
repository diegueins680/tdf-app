# Combined ledger candidate — 22 October 2026

The draft ledger consolidation incorporates snapshot repair
`879900fa014b97e0ac971bcdf59a62b5bd55f28e` through merge
`ac588fcfcf294b46027dfeacc5928e86dd197617`, followed by protected main
`5e33a25cb9aaceca8fcafb6adef7c3faf2e6e767`. It now includes the physical
parent-row version fence required to reject posting from a stale snapshot.
The complete main prefix of 211 migrations remains unchanged, followed by the
five existing ledger, checkout snapshot, terms and fence migrations. The resulting
216-entry manifest, every SQL body and introduction identity match the repair
source. No historical migration is rewritten.

All 127 requirement objects retain the three-way combination of both parents;
81 authority decisions retain each distinct parent decision. Conformance is not
promoted. CI entrypoints and positive/negative formal invocations are preserved.
The compatible DOMPurify 3.4.17 and ip-address 10.7.4 lock patches and existing
regression floors match the separately qualified queue candidate; the audit
policy is unchanged. The Mobile gitlink remains
`3194982a01174d36017533e79400ce9209086162`.

The source review record
`docs/catalog-persistence/ledger-combined-reconciliation-20261022.json`
retains the superseded main manifest review by immutable file and entry hashes.
The existing 216-entry manifest classification remains active, together with both
parents' current source classifications. Previous reconciliation and execution
records retain their original identities. Discovery and traceability must be
regenerated after this source checkpoint.

Qualification must exercise this combined tree on a new owned PostgreSQL fixture:
full apply and replay, exact ledger arithmetic, legacy upgrade, all 33 witnessed
snapshot records including six expected lock-only counterexamples, checkout line
snapshots, historical terms identity and the Operations worker. The installed fence
must be restored exactly after negative controls, followed by database removal
and cluster shutdown. Retained earlier receipts do not count as a fresh execution.

PR #551 remains draft with automatic merge disabled until exact-head hosted CI,
independent review, and strict protected-base qualification complete. Source #597
remains preserved; no source branch is deleted. The named SQL schedules do not
prove complete MVCC refinement, production role privileges, retry behavior of all
compiled writers, or deployed-data compatibility. These obligations remain open
in `formal/system/ledger-refinement-2026-10-16.md`.

This source integration authorizes no production migration, deployment, provider
transaction, feature activation or retention policy. Recovery prerequisites and
production retention decisions remain separate gates.
