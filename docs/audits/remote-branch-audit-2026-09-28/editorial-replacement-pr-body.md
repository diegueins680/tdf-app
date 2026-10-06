Imported events must retain editorial changes across later provider refreshes and remain visible when their canonical publication metadata permits it. PR464 records field ownership, but its private snapshot is rejected by API and directory visibility checks, while lifecycle reconciliation can overwrite editorial visibility, ticket links and workflow state.

This replacement preserves PR464’s complete history and normally merges current main. Stored-data decoders and both database predicates accept only object-valued `_discoveryOwned` evidence; public requests retain strict allowlists and responses omit that namespace. Locked editorial updates and image uploads permanently relinquish changed fields. Provider and subscription reconciliation preserve those decisions and conservatively handle legacy rows; source disappearance can still hide an event.

The forward migration `2026-10-03_discovery_ownership_metadata_boundary.sql` aligns the public-directory function without changing source data, tables or the composed suppression views. It is registered after the existing 159 immutable migrations with introduction commit `6e7b87d8ad3a4c56b033484fe7174dceacec5961`. The production schema gate and historical-order rehearsal verify both ownership compatibility and existing privacy boundaries. Recovery remains forward-only; no source is activated.

Supersedes #464 only after successful integration. Its original PR/branch remain intact. Native stack #466 cannot use the established ordinary merge path without rewriting history, so this is a standalone replacement with source attribution retained. #469 already integrated the source/run prerequisites. Use a normal merge to retain migration introduction ancestry.

Verified validation:

- Hosted backend build, 3,553 Hspec examples with zero failures (six separate-runner cases exercised), the compiled PostgreSQL predicate, 1,141 social HTTP examples and all runtime/schema stages passed on current head `4100a04c0`. The local Stack-built suite also passed all 3,553 examples, followed by the compiled native PostgreSQL predicate and 15 invitation cases.
- Actual native PostgreSQL16: 160-migration production-shaped schema apply/reapply, schema verification, both historical privacy orders, and directory ownership/private/suppressed-event projection regressions passed.
- Reproduced the old directory rejection, then passed 16 metadata cases after apply and reapply. Ten source-derived PostgreSQL handler-predicate cases also passed.
- 82 production-release tests, 27 CI tests, two catalog regression tests, specification inventory, formal verification and the strict catalog gate (1,162 reviewed candidates) passed. All 159 earlier migration entries and SQL bytes are unchanged.

All applicable hosted checks pass on `4100a04c0de9496e7bf9cd6d656602dee1d8e277`; this PR is ready for independent review. Approval and resolution of any new blocking feedback remain required before merge.
