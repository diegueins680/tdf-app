# ADR 0115: Extend the existing social event as the canonical operations aggregate

Status: proposed

Date: 2026-09-14

## Context

TDF already has social events, ticketing, RSVP, live broadcasts, directory opportunities, bookings,
resources, logistics, commerce, reputation, chat, calendar, and notifications. Their identifiers and
public behavior are in use, but no single operations workflow connects them. Introducing another
event table would create divergent ownership, visibility, schedules, tickets, and settlement facts.

## Decision

- Keep `social_event.id` as the event identity and add normalized relations for multiple ownership,
  scoped/time-bounded grants, revisions, sessions/spaces, plans/tasks/RACI, engagements, and audit.
- Reference existing Party, Venue, Room, Resource, directory, booking, checkout, provider inbox,
  ledger, review, chat, notification, and ticket records through explicit bindings.
- Translate legacy event states through a compatibility layer into the canonical lifecycle. Do not
  rewrite historical state IDs or infer approvals/ownership that cannot be proven.
- Use event revisions and version-conditioned commands for material collaboration. Search/UI read
  models are projections; authoritative transitions remain transactional server commands.
- Roll out behind an independent `event.operations` feature flag using expand/backfill/dual-read/
  compare/cutover/contract. The flag must default off in every environment until tests pass.

## Consequences

Existing clients can continue using stable event IDs while new clients adopt richer relations. The
implementation must temporarily maintain compatibility projections and explicitly report legacy
records with no trustworthy owner/timezone/resource mapping. Cleanup migrations occur only after
all consumers and rollback paths are verified.

## Alternatives considered

### Independent event-operations aggregate

Rejected because it would duplicate ticket ownership, RSVP visibility, organizer authority,
streaming identity, and public routes.

### Store the plan in event metadata JSON

Rejected because database constraints, conflict detection, contextual authorization, exact queries,
auditing, and migrations cannot be reliably enforced over an opaque mutable document.
