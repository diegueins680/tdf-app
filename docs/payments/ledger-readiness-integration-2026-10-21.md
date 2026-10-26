# Ledger readiness integration — 21 October 2026

This draft refresh retains ledger parent
`f58f6ac1859d3cc294a3ed3cee23cb3da6fac487` and protected main
`8de1bae4c30961eaa11637e5b4307271250ee176` as merge parents. The complete
211-entry main migration prefix remains unchanged. The four original ledger,
checkout snapshot and terms migrations follow in their original relative order,
producing 215 entries. SQL, rollback SQL and introduction identities are retained.
The Mobile gitlink advances to main's
`3194982a01174d36017533e79400ce9209086162`.

The combined source retains 127 requirement entries and 77 authority decisions.
Three-way field comparisons preserve both parents' obligations without promoting
conformance claims. Both parents' CI and positive/negative model invocations
remain registered. Inventories and source fingerprints are regenerated after the
merge checkpoint; the earlier integration document remains historical.

`docs/catalog-persistence/ledger-readiness-reconciliation-20261021.json` records
immutable file and canonical-entry locators for 28 shared review differences,
both superseded manifest reviews and five replaced navigation/chat reviews.
Protected main's current navigation and chat reviews remain active. The synthetic
process-session timeout test retains its source-specific technical-constant
classification. The new 215-entry manifest receives its own fingerprint and
bounded deployment-manifest classification; previous records remain historical.
The music and public-UX regression checks retain their original prefix and review
assertions.

Keep PR #551 draft with auto-merge disabled. Its separate draft dependency PR #597
contains the physical parent-row version fence required to reject stale ledger
snapshots. This 215-entry parent does not contain that repair. It must be integrated
and the resulting candidate qualified before this work can be admitted to main.
Fresh hosted checks and independent approval of the eventual exact head remain
required. Local database checks use an owned disposable UTF-8 PostgreSQL 16
cluster, with database removal and cluster shutdown after verification.

This source reconciliation does not authorize production migrations, provider
activation or retention policy changes. Production recovery and retention gates
remain unresolved.
