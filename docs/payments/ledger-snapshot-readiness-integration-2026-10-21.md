# Ledger snapshot repair readiness integration — 21 October 2026

This draft refresh retains repair parent
`935beed5d053bfc75d1a0e1a6e4ae204c1c64423` and ledger dependency
`380da8855ade877f79593e80f93291cd314d311d` as parents. The complete
215-entry dependency manifest remains the exact prefix. The unchanged physical
parent-row-version fence follows, producing 216 migrations. All SQL and original
introduction identities are retained. The Mobile pin matches protected main at
`3194982a01174d36017533e79400ce9209086162`.

The original fence SQL, witnessed concurrency runner, legacy-upgrade runner,
CI entrypoints and open refinement document retain their exact bytes. The
repair requires stale REPEATABLE READ and SERIALIZABLE transactions to abort
rather than commit against an old ledger snapshot. READ COMMITTED must observe
committed entries after waiting. Lock-only controls remain expected failures,
not evidence that the repair passed. The combined schema requires fresh apply,
replay, arithmetic, upgrade and witnessed-concurrency qualification.

`docs/catalog-persistence/ledger-snapshot-readiness-reconciliation-20261021.json`
preserves both parent reviews by immutable locators, retains the dependency's
current navigation/chat classifications and gives the 216-entry manifest a new
fingerprint. Previous source records and test receipts remain historical.
Requirements retain their three-way fields without a conformance promotion.
Inventories, traceability and catalog reports are regenerated from this source.

Keep PR #597 draft and stacked on #551, with auto-merge disabled. Its eventual
integration into the ledger parent requires qualification of the resulting main
candidate, fresh exact-head hosted CI and independent review. This document
makes no production migration, deployment, provider or retention authorization.
