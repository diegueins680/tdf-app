# AUTH-EVENT-WATCH-001 — service-only event ingestion

The user authorized a dedicated event-watching identity, historical imports with
real lifecycle/source state, confirmed public third-party listings and duplicate
avoidance. Credentials must not confer interactive administrative authority.

An import may commit only if its purpose-specific hashed session and its parent
client are current, active and unexpired. The client namespace is server-owned.
Publishing additionally requires the current client's publication grant and all
existing research eligibility checks. Rotation, revocation and permission changes
invalidate all prior sessions. The service cannot approve the pilot.

An existing materialization target must belong to the current curator and the
current service-provider namespace. Current event ownership and a qualifying
provider reference are checked under retained row locks before any target
reference/artist mutation or response association data. Sharing a curator Party
does not share client authority. A transferred or unexpected target requires
operator review, without a force-created duplicate. The separate administrative
entry point retains its existing authority in the same materialization engine.
An old run's creator is immutable: a reassigned curator must select a new run key.
When an exact provider reference selected the target before an event lock wait,
the service also locks and rechecks that same reference key after the wait. A
different alias does not validate a removed or changed selected mapping. The
canonical pilot trigger already serializes direct event-ID rebinding, but it
does not cover reference deletion; neither guard substitutes for the other.

Implementation uses an isolated `AuthProtect "event-watch"` context, separate
tables, parent-before-session row locks held through the import transaction, and
another expiry check after all event lock waits. Client updates conflict with
those locks. It reuses transactional research primitives without their public
administrative wrappers or intermediate commit. All error outcomes roll back.
An already admitted request may commit before a queued revocation; after the
revocation commits it cannot authorize another request. This is the lock-order
contract, not instantaneous interruption of a transaction already admitted.

Evidence: Hspec lifetime/hash/payload properties, provisioning-tool negative
tests, SQL fixture for schema and revocation triggers, and a bounded 128-case
admission model with three failing guard-removal controls. The abstract model is
not mechanically extracted from the Haskell implementation and does not model
the event-owner/provider-reference checks. Embedded PostgreSQL
can check the SQL fixture, but does not establish multi-connection concurrency.
Release still requires real PostgreSQL HTTP tests for concurrent rotation,
lock-wait expiry, namespace isolation and absence of interactive authority;
verified backend compilation; and privately provisioned job credentials.

The API's candidate list is bounded to 500 entries without pagination. Historical
backfill beyond that limit must use stable external IDs and the idempotent import
operation, rather than treating the response as an exhaustive catalog. Current
materialization preserves manual event edits; it does not update arbitrary
published fields or infer missing times/venues. Existing pilot/publication gates
remain authoritative. No credential, activation, production import or publication
is implied by merging this implementation.
