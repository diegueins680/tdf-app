# Studio booking authority and resource calendar

`AUTH-BOOK-001` and `BOOK-CALENDAR-001` govern the authenticated studio booking
boundary. On 2026-10-04 (America/Guayaquil), the product owner explicitly selected
Admin, Manager, Studio Manager and Reception for studio-wide booking access.
Other callers require the stored customer or assigned engineer Party identity.
Scheduling module admission alone grants no foreign-object authority. Roles
compose: membership in any of the four staff roles grants studio scope. An
assignment supplied in a request cannot authorize that same request.

The backend applies this scope to detail, unfiltered and filtered lists, mutations,
and CRM booking projections. Foreign detail reads return an empty list; foreign
updates return 404. Non-staff creation binds an omitted customer to the caller and
rejects a different customer with 403. Existing module admission still applies.
Public resource availability exposes occupancy, not private booking notes. The
same backend policy governs web and the exact shipped Mobile lineage; generated
clients describe the existing GET/POST `/bookings` and PUT `/bookings/{bookingId}`.
No offline acceptance or new Mobile payment flow is introduced.

The machine-readable role and fulfillment projection table is
[`booking-policy.json`](booking-policy.json); real HTTP role checks consume it.

## Atomic mutation contract

A mutation validates the current session within its database transaction. Updates
lock the booking before reading and replacing it; independent notes/cancellation
edits therefore preserve both accepted changes. Assignment is read from that
locked record. A database exclusion, constraint, serialization or deadlock failure
rolls the transaction back and returns a sanitized 409. The caller must reread
before retrying a conflict. Generic creation has no idempotency key: a lost response
requires reconciliation, not an automatic duplicate POST. Repeating a field-setting
update does not promise exactly-once notification effects. Explicit null update
fields mean no update; a body containing no non-null supported field is invalid.

ADR0109's resource calendar is `service_booking_resource_allocation`, with half-open
`[starts_at, ends_at)` intervals and exclusion for holding/reserved allocations.
Legacy booking creation/resource insertion and interval/status updates project
inside the same transaction. Cancelled/NoShow release; Completed and historical
past legacy rows project completed; other legacy rows reserve. Moving a past row
into the future must reacquire availability. Reactivation must reacquire its
resource and fails if another active allocation overlaps. The API currently
permits edits among the six legacy statuses subject to these constraints; this
is not a newly invented irreversible workflow.

Checkout-bound booking interval and offering must match their immutable runtime
snapshot. Generic booking updates cannot change fulfillment independently. Accepted
runtime fulfillment projects: on_hold -> Tentative; cancelled/expired -> Cancelled;
completed -> Completed; no_show -> NoShow; in_progress/balance_due/overtime_review/
disputed -> InProgress; confirmed/scheduled/reschedule_requested/
cancellation_requested -> Confirmed. Runtime transition/payment guards remain
mandatory. Runtime resource status is holding for on_hold, released for
cancelled/expired, completed for completed, otherwise reserved.

The additive migration never changes an applied migration. Existing bound snapshot,
lifecycle, missing/extra allocation, interval, expiry or allocation-status drift
requires explicit reconciliation and aborts migration. Legacy projections rebuild
under exclusion: pre-existing conflicts abort rather than selecting a winner.
Writers must be drained before the production migration. Rollback by deploying an
old handler is unsafe because old handlers permit foreign access and stale record
replacement; recovery must preserve these guards. There is no destructive down
migration or silent paid-evidence rewrite.

## Bounded formal models and executable refinement evidence

`StudioBookingProjection.tla` checks two bookings over three exclusive unit slots,
atomic edits and separate domain/calendar state. `ProjectionCurrent` and
`NoActiveOverlap` hold with projection and exclusion enabled. Three controlled
mutations remove edit projection, reactivation projection or exclusion; each must
violate its named invariant. Continuous timestamps, arbitrary overlap geometry,
checkout money, multiple resources and PostgreSQL itself are outside this model.

`StudioBookingScope.tla` checks four actor equivalence classes (owner, assigned,
other, staff), session revocation, request start and commit. `NoForeignAccess` and
`NoRevokedCommit` hold. Module-only authority, accepting requested assignment and
missing commit session validation each produce a named counterexample. Role and
assignment changes during a request, role grant transactions and UI rendering are
outside the model. Both models permit stuttering, impose no fairness and make no
liveness claim. These bounded state searches are not universal correctness proofs.

`scripts/test-booking-conformance.py` exercises the actual backend against a fresh
nonce PostgreSQL database: all canonical roles, private notes, owner/assignment
scope, forged assignments, overlapping intervals, cancellation/reactivation,
competing resource moves, a lock-controlled cancellation/notes race, revoked
sessions, request shapes, checkout projection and migration negative controls.
It applies the complete production migration batch twice and deletes only its
owned database. Results include source revision, dirty state and binary hash;
working-tree runs are exploratory, not final-candidate evidence. CI uses its
isolated PostgreSQL service; local runs require explicit loopback port and binary.
No external provider or real-money transaction is part of these checks.

Remaining coverage limits include concurrent role revocation, resource-relation
update/deletion, browser navigation/cache races and a full checkout lifecycle
refinement proof. Those gaps must not be hidden by passing these scoped checks.

The concurrency choice follows [PostgreSQL row-lock semantics](https://www.postgresql.org/docs/17/explicit-locking.html)
and [range exclusion constraints](https://www.postgresql.org/docs/17/rangetypes.html).
The separation of module admission and object authorization follows the
[OWASP authorization guidance](https://cheatsheetseries.owasp.org/cheatsheets/Authorization_Cheat_Sheet.html).
The exact four-role policy comes from the product owner, not those references.

## Customer-facing retrieval boundary

`PRIV-RAG-001` separately owns this privacy boundary. The public assistant and
Instagram/Facebook/WhatsApp automated replies have no
studio booking identity scope. `TDF.RagStore.selectRagChunks` therefore uses the
index only to rank existing public course IDs and renders current course metadata
from the database. It never returns cached raw chunk content. Historical private
availability, studio-brain, campaign and other unapproved sources remain excluded
without depending on reindexing. A deleted course has no eligible source row.
This is a backend knowledge-retrieval boundary, not a prompt instruction. Internal
index retention, embedding-provider sharing and curated conversation examples
remain separately scoped privacy obligations.
