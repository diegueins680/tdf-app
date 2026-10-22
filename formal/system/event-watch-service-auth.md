# AUTH-EVENT-WATCH-001 — service-only event ingestion

The user authorized a dedicated event-watching identity, historical imports with
real lifecycle/source state, confirmed public third-party listings and duplicate
avoidance. Credentials must not confer interactive administrative authority.

An import may commit only if its purpose-specific hashed session and its parent
client are current, active and unexpired. The client namespace is server-owned.
Publishing additionally requires the current client's publication grant and all
existing research eligibility checks. Rotation, revocation and permission changes
invalidate all prior sessions. The service cannot approve the pilot.

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
not mechanically extracted from the Haskell implementation. Embedded PostgreSQL
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
