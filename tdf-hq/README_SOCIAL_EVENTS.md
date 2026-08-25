# Social Events API Reference

This document describes the current authenticated Social Events API implemented in `TDF.Server.SocialEventsHandlers`.

## Base Path and Auth

- Base path: `/social-events`
- Auth: bearer token (all routes are under protected API)

Example headers used in all cURL requests:

```bash
export API_BASE="https://your-api-host"
export TOKEN="your-bearer-token"

AUTH=(-H "Authorization: Bearer $TOKEN" -H "Content-Type: application/json")
```

## Event Model Extensions

Event records now use:

- `eventTypeId` (UUID of an active, effective, published item in the persisted `event-types`
  catalog)
- `eventStatus` (`planning`, `announced`, `on_sale`, `live`, `completed`, `cancelled`)
- `eventCurrency` (ISO-like code, default `USD`)
- `eventBudgetCents` (optional total budget cap in cents)

The type relationship is stored in `social_event.event_type_id`; `metadata.eventType`, codes,
labels, and slugs are not accepted as write identities. Load the public `event-types` catalog (and
its `social-event/global` scoped default) before constructing a create payload.

Filter support on list endpoint:

- `event_type_id`
- `event_status`

## Events

### List Events

```bash
curl -sS "${API_BASE}/social-events/events?city=Quito&event_type_id=11111111-1111-4111-8111-111111111111&event_status=planning&start_after=2026-03-01T00:00:00Z&limit=20&offset=0" \
  "${AUTH[@]}"
```

### Create Event

`eventEnd` is optional. Omit it or send `null` when the official end or
duration is not confirmed; the API does not infer a duration. When supplied,
the end must be after `eventStart`.

```bash
curl -sS -X POST "${API_BASE}/social-events/events" \
  "${AUTH[@]}" \
  --data '{
    "eventTitle":"TDF Summer Fest",
    "eventDescription":"2-stage live showcase",
    "eventStart":"2026-07-12T18:00:00Z",
    "eventEnd":"2026-07-13T02:00:00Z",
    "eventVenueId":"3",
    "eventPriceCents":2500,
    "eventCapacity":1200,
    "eventTypeId":"11111111-1111-4111-8111-111111111111",
    "eventStatus":"planning",
    "eventCurrency":"USD",
    "eventBudgetCents":4500000,
    "eventArtists":[]
  }'
```

### Update Event

```bash
curl -sS -X PUT "${API_BASE}/social-events/events/42" \
  "${AUTH[@]}" \
  --data '{
    "eventTitle":"TDF Summer Fest 2026",
    "eventDescription":"Lineup announced",
    "eventStart":"2026-07-12T18:00:00Z",
    "eventEnd":"2026-07-13T02:00:00Z",
    "eventVenueId":"3",
    "eventPriceCents":3000,
    "eventCapacity":1200,
    "eventTypeId":"11111111-1111-4111-8111-111111111111",
    "eventStatus":"on_sale",
    "eventCurrency":"USD",
    "eventBudgetCents":5000000,
    "eventArtists":[]
  }'
```

## Budget Lines

Budget lines are organizer-managed planning buckets (income/expense).

### List Budget Lines

```bash
curl -sS "${API_BASE}/social-events/events/42/budget-lines" "${AUTH[@]}"
```

### Create Budget Line

```bash
curl -sS -X POST "${API_BASE}/social-events/events/42/budget-lines" \
  "${AUTH[@]}" \
  --data '{
    "eblCode":"VENUE-RENT",
    "eblName":"Venue rent",
    "eblType":"expense",
    "eblCategory":"production",
    "eblPlannedCents":1200000,
    "eblNotes":"Main hall and security deposit"
  }'
```

### Update Budget Line

```bash
curl -sS -X PUT "${API_BASE}/social-events/events/42/budget-lines/7" \
  "${AUTH[@]}" \
  --data '{
    "eblCode":"VENUE-RENT",
    "eblName":"Venue rent (final)",
    "eblType":"expense",
    "eblCategory":"production",
    "eblPlannedCents":1300000,
    "eblNotes":"Final negotiated value"
  }'
```

## Finance Entries (Detailed Accounting)

Finance entries are organizer-managed ledger records linked to events and optionally to a budget line.

Notes:

- `ticket_sale` and `ticket_refund` entries are system-derived from ticket orders and cannot be manually created/updated.
- Manual entries accept sources such as `manual`, `operations`, `vendor_payment`, `sponsorship`, `merchandise`, `other`.

### List Finance Entries

```bash
curl -sS "${API_BASE}/social-events/events/42/finance-entries?direction=expense&status=posted" "${AUTH[@]}"
```

### Create Finance Entry

```bash
curl -sS -X POST "${API_BASE}/social-events/events/42/finance-entries" \
  "${AUTH[@]}" \
  --data '{
    "efeBudgetLineId":"7",
    "efeDirection":"expense",
    "efeSource":"vendor_payment",
    "efeCategory":"production",
    "efeConcept":"Stage rigging payment",
    "efeAmountCents":280000,
    "efeCurrency":"USD",
    "efeStatus":"posted",
    "efeExternalRef":"INV-STAGE-2031",
    "efeNotes":"50% advance",
    "efeOccurredAt":"2026-06-01T15:30:00Z"
  }'
```

### Update Finance Entry

```bash
curl -sS -X PUT "${API_BASE}/social-events/events/42/finance-entries/21" \
  "${AUTH[@]}" \
  --data '{
    "efeBudgetLineId":"7",
    "efeDirection":"expense",
    "efeSource":"vendor_payment",
    "efeCategory":"production",
    "efeConcept":"Stage rigging payment - adjusted",
    "efeAmountCents":300000,
    "efeCurrency":"USD",
    "efeStatus":"posted",
    "efeExternalRef":"INV-STAGE-2031-R1",
    "efeNotes":"Adjusted after scope increase",
    "efeOccurredAt":"2026-06-01T15:30:00Z"
  }'
```

## Finance Summary

Returns budget and accounting rollups, including ticket-derived accounting signals.

### Get Summary

```bash
curl -sS "${API_BASE}/social-events/events/42/finance-summary" "${AUTH[@]}"
```

Representative fields:

- `efsPlannedIncomeCents`
- `efsPlannedExpenseCents`
- `efsActualIncomeCents`
- `efsActualExpenseCents`
- `efsNetCents`
- `efsTicketPaidRevenueCents`
- `efsTicketRefundedRevenueCents`
- `efsTicketPendingRevenueCents`
- `efsBudgetVarianceCents`
- `efsBudgetUtilizationPct`

## Authorization Rules

- Event organizers can manage ticket tiers/orders, budget lines, finance entries, and finance summary.
- If an event has no organizer yet, organizer ownership is claimed on first organizer-level management action.
- Non-organizers are blocked with `403` on organizer-only management endpoints.

## Validation Rules (Selected)

- Event budget (`eventBudgetCents`) must be `>= 0` when provided.
- `eventTypeId` is required for create/update and must resolve to an active, effective,
  non-deprecated, published event type. Unknown UUIDs and legacy strings return `422`.
- Budget line planned cents (`eblPlannedCents`) must be `>= 0`.
- Finance entry amount (`efeAmountCents`) must be `> 0`.
- Finance entry direction must be `income` or `expense`.
- Finance entry status must be one of `draft`, `posted`, `void`, `pending`.
