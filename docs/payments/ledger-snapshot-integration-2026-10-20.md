# Ledger snapshot repair on the refreshed consolidation

This merge retains repair parent `2167253e5e83918ddd357a39ea2119bf658f46a7`
and dependency parent `f58f6ac1859d3cc294a3ed3cee23cb3da6fac487`.
The manifest preserves the dependency's exact 212-entry prefix and appends the
original stale-snapshot repair, for 213 entries. All parent SQL and introduction
SHAs remain unchanged, as does the shipped Mobile pin.

The repair SQL, Haskell tests, snapshot schedule runner, bounded model and open
refinement obligations remain exactly the repair parent's source. Main's Music
and Operations changes and the dependency's test registrations are inherited.
No conformance obligation is upgraded by combining these sources. The prior
209-entry receipt remains historical and cannot qualify this combined tree.

`docs/catalog-persistence/ledger-snapshot-reconciliation-20261020.json` retains
both immutable sources for every differing shared catalog decision and both
predecessor manifest reviews. The preceding music/main and ledger/main records
remain unchanged. The current fingerprint and both successor mappings agree.
Generated inventories and discovery counters are refreshed from the final tree.

The PR remains stacked on #551 and draft pending current-head hosted qualification
and independent review. The consolidation also remains draft until this repair
is integrated and the combined main candidate qualifies. These source changes
authorize no production migration, deployment, provider transaction or activation.
