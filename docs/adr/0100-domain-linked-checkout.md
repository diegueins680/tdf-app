# ADR-0100: Domain-linked provider-neutral checkout

Status: Accepted — 2026-08-13

## Context

Marketplace, services, courses, tickets, tips, and bookings have different fulfillment and currently
duplicate provider code. One universal order table would erase useful domain constraints.

## Decision

Create a checkout aggregate with `domain_type` and immutable `domain_id` linkage. It owns pricing
snapshots, attempts, events, refunds, disputes, holds, receipts, and financial postings. Each domain
keeps its order and fulfillment state and consumes verified checkout events idempotently.

## Alternatives

- Keep provider code per domain: rejected because verification and retries already diverge.
- Replace every domain order with one table: rejected because rentals, tickets, bookings, and
  distribution have incompatible fulfillment invariants.

## Consequences

Adapters and finance become consistent; domain migrations can be phased. Cross-domain carts remain
forbidden until a separately designed tax/fulfillment contract exists.
