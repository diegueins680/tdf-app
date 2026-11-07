# Ledger consolidation with protected main — 20 October 2026

This integration retains ledger parent `9d7551353a7c278cd0937eb57d04a94241318d42`
and protected main `0a37123ea1d9d6b7c6b44d73c9342e18515c06c9` as parents.
The manifest preserves main's complete 208-entry prefix and appends the four
original ledger, checkout and terms migrations in their original relative order.
All common manifest entries agree; no migration SQL, rollback SQL or introduction
SHA is changed. The combined manifest contains 212 entries. The Mobile gitlink
remains `4c84c60a5836f7d39f18e10d34d136996bb0f63f`.

Both Ledger and Music test registrations and CI commands remain active. All
positive and negative model commands from both parents are retained. Authority
IDs and state-machine bindings are united by identity; shared entries agree.
Automatically merged requirements retain both parents' obligations and unresolved
conformance boundaries. Generated inventories and catalog discovery are rebuilt.

`docs/catalog-persistence/ledger-main-reconciliation.json` retains immutable
file and canonical-entry hashes for all 58 differing shared catalog decisions.
Main's ten source-specific reviews remain byte-semantically unchanged. The two
retired manifest reviews retain their immutable locators; the current combined
manifest has a separate classification and matching successor mappings. The
music reconciliation regression still checks its original 208-entry prefix,
original SQL and reviews when the current manifest is extended.

The stale-snapshot ledger repair in draft PR #597 remains a separate stacked
candidate. This parent integration does not claim that repair or its refinement
obligations are complete. Its prior test receipts retain their original source
identity. Fresh exact-head hosted checks and independent approval are required
before protected FIFO integration. Keep this consolidation draft until the
stacked stale-snapshot repair is integrated and qualified; this merge alone is
not ready for main. No production migration, runtime change,
provider activation or financial transaction is authorized by this source merge.
