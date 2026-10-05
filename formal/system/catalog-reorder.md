# Catalog reorder transaction — REC-CATALOG-001

A catalog reorder is one accepted administrative mutation. Its selected item
positions/versions, catalog cache revision and success audit must commit together.
An invalid member, stale expected revision, failed audit or denied request must
leave no committed prefix. The existing permission is `catalog.update` plus
`ModuleCatalog`; this requirement does not grant new roles access.

`orderedItemIds` is a nonempty unique list containing every item UUID in the
catalog, including inactive items. The OpenAPI incomplete-set rejection is the
working authority pending product-owner clarification. Existing subset acceptance
violates that contract and can leave conflicting positions; it is not preserved.
Accepted positions form the contiguous sequence0..n-1. A correlation ID labels the audit; it is not an idempotency key.
A successful request increments each selected version and the catalog revision
once. Replaying its old expected revision returns409, including an identical body.
The success response remains200 with an empty body.

The handler locks/reloads the active `catalog_definition` row and compares
`expectedCatalogRevision` inside the same PostgreSQL transaction as item writes
and audit insertion. A typed HTTP rejection is thrown through the transaction
boundary to trigger rollback, then translated to the existing response. SQL
exceptions also roll back. No schema migration or provider effect is introduced.

The original implementation committed one transaction per item. An isolated
HTTP reproduction using a valid first UUID and nonexistent second UUID returned409
while advancing the first item's version and changing its position. That is a bug,
not a specification precedent. AUTHORITY-028 resolves this conflict.

## Executable evidence and scope

`scripts/test-booking-conformance.py` creates its own database and checks actual
handler denial, missing and foreign members, unchanged rows/revision/audit after
rejection, injected audit failure, two concurrent requests at the same expected
revision, one successful commit and a rejected stale retry. It shares the existing
fixture runner; these are catalog checks, not booking policy assertions.

`formal/event-operations/CatalogReorder.tla` abstracts two requests to one catalog,
both carrying expected revision0. Each has an arbitrary authorization bit and one
of three input outcomes: valid, foreign member, audit failure. Staging is an open
transaction; uncommitted writes are not externally visible. Commit/rejection are
atomic abstract steps. Counters range0..2. No fairness assumption or liveness
claim is made; the model proves bounded safety only. Negative configurations remove
rollback, fresh revision checking, authorization or mandatory audit independently;
each must violate its named invariant, not merely fail to parse or time out.

| Requirement property | Formal invariant | Implementation | Runtime check |
| --- | --- | --- | --- |
| Rejected requests have no committed effects | NoRejectedEffect | reorderHandler transaction | Missing/foreign item and injected audit failure |
| Revision, selected writes and audit commit together | AtomicEvidence | writeAuditDB in same transaction | Row/revision/audit snapshots |
| Same expected revision cannot succeed twice | NoStaleCommit | Locked revision comparison | Concurrent HTTP requests and stale retry |
| Only authorized callers mutate | NoUnauthorizedCommit | requireCatalogCapability | Anonymous401 and Fan403 |

The abstract model is not a verified translation of Haskell or PostgreSQL. It
excludes dynamic grant/session revocation during a request, other catalog mutation
handlers, trigger reconfiguration, transport ambiguity after a committed response,
server crashes, deletion/recreation, overflow and storage failure. Other catalog
writers have separate transaction boundaries and are not covered by the reorder
serialization claim. PostgreSQL deadlock/lock-timeout aborts must not be reported as
success; mixed-operation retry and lock-order guarantees remain open. No blanket
catalog correctness or global revocation guarantee is asserted.
