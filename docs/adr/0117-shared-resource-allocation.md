# ADR 0117: Route event reservations through the shared resource allocation calendar

Status: proposed

Date: 2026-09-14

## Context

TDF has people, rooms, venues, equipment/assets, service bookings, and commerce holds. Existing
service booking migrations already provide PostgreSQL exclusion-backed allocation and concurrency
tests, but event logistics also stores loosely typed places and assignees. A new event-only calendar
would allow the same person, venue, or asset to be confirmed twice through separate paths.

## Decision

- Bind every exclusively bookable event participant, room, venue, asset, or provider capacity to an
  existing canonical `resource` identity.
- Compute a half-open occupied interval including setup, travel, soundcheck, teardown, and recovery
  buffers before confirmation.
- Route tentative/confirmed event allocation through the shared exclusion/capacity primitive in the
  same transaction as booking/engagement state, command receipt, audit, and outbox.
- Provide an explicit privileged override command with policy, actor, non-empty reason, affected
  interval, and immutable audit. Ordinary confirmation cannot set an override flag.
- Do not infer or overwrite historical bindings. Backfill exact links where provable and report the
  rest for review.

## Consequences

Availability and conflict behavior become consistent across service booking and event staffing.
Capacity greater than one needs locked aggregate counters or slot resources in addition to exclusive
range constraints. Multi-resource confirmation must use a deterministic lock order and transaction
tests to prevent deadlock and partial allocation.

## Alternatives considered

### Event-specific availability table

Rejected because it would be unable to see reservations created by service booking, rooms, or asset
checkout.

### Application-only conflict check

Rejected because two concurrent transactions can both observe availability before either commits.
