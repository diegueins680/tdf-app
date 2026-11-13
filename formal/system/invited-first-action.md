# First action after an invitation (GROWTH-FIRST-ACTION-001)

Source: a walk of the three campaign invitation links on production as new
phone-sized accounts on 2026-11-13 (organizer, fan, artist), requested by the
product owner. It records what stood between a signed-up visitor and the action
the invitation promised, and what this change guarantees instead.

## Observed before this change

| Journey | Observation |
|---|---|
| Fan | Signup landed on `/fans` in about six seconds. The first follow button was 7,892 px down a 14,570 px page on a 664 px viewport. |
| Organizer | Signup landed on the event form. After creating an event, the primary button opened the planning workspace, which answered 403 to an account without the Scheduling module, although the logistics API accepts the event's organizer. No screen offered a way to publish the event. A new account's event started in UTC. |
| Artist | The profile form on `/mi-artista` started 1,932 px down, below an events carousel, the organizer panel and the fan profile form. |
| Any signed-in visitor | The upcoming-events carousel preceded the content of a single event page. |

## Guarantees

1. A signed-in fan who follows nobody is offered up to five artists to follow
   before any other content of the fan hub. The list is chosen once per viewer
   and does not reorder or disappear after a follow.
2. The organizer or delegated administrator of an event that is not public can
   publish it from the event page. Publishing is a two-step action (request,
   then confirm), reads the event again immediately before writing, sets
   visibility on, and moves the event to the earliest active state that is
   publicly listable and that the lifecycle allows the organizer to enter
   directly without review. When no such state exists the action is not offered.
3. Links to the planning workspace are shown only to accounts whose access
   rules allow opening it.
4. A new event starts in the browser's time zone when the account carries the
   server placeholder zone (UTC) and never chose one. A chosen zone wins.
5. The upcoming-events carousel is not rendered on a single public event page,
   the artist onboarding page or the artist profile page. On the artist profile
   page the artist form precedes the fan profile and organizer sections.

## Boundaries

- No server behavior changes. Publishing uses the existing event update
  endpoint, whose organizer check, workflow-transition check and event lock
  remain authoritative.
- Access to the planning workspace is unchanged. Whether an event's organizer
  without the Scheduling module should be able to open it is an open product
  decision and is not decided here.
- Ticket checkout availability, pricing and refund policy are unchanged.

## Evidence

Component and page tests listed under the requirement, plus a read-only preview
of the local build against production data with the three test accounts on
2026-11-13: first follow control at 488 px; time zone field `America/Guayaquil`;
no planning links and a publish confirmation for the organizer; artist form at
626 px. The publish write itself was not executed against production.
