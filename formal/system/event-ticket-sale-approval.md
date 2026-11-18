# Paid ticket sale requests and their approval

EVT-TICKET-SALE-001 describes how the manager of one event asks TDF to sell
its paid tickets and how a strict administrator decides. It is derived from
`tdf-hq/src/TDF/Server/EventTicketSale.hs` (`eventTicketSaleServer`,
`saleStateFromRows`, `standardTicketSaleTerms`) and the existing
`event_ticket_checkout_policy` table and guards. It is a description of the
shipped behaviour with finite tests, not a proof, and not a legal, tax or
commercial opinion on the terms.

Before this contract the only way to create an approved policy was a database
statement written by an operator. Public checkout reads nothing else, so an
organizer could create an event and its ticket tiers but never sell.

## Actors

- **Event manager**: the party stored in `social_event.organizer_party_id`, or
  a delegated administrator of that event (EVT-ADMIN-DELEGATION-001).
- **Strict administrator**: an account that passes `hasStrictAdminAccess`.
  Holding the Admin role together with a role outside the strict scope is not
  enough.
- **Other signed-in accounts**, which receive nothing here.

Every route requires an authenticated session. An event without a stored
organizer has no manager, so nobody can request its sale.

## Routes

- `GET /social-events/events/{eventId}/ticket-sale` reports the sale as
  `none`, `requested`, `approved` or `declined`, whether the caller may
  request and why not, and the terms. For an active approved policy the
  response uses its stored terms. New requests use the standard terms. Managers and strict
  administrators only; others receive 403.
- `POST /social-events/events/{eventId}/ticket-sale/request` records the
  request. Managers only.
- `GET /social-events/ticket-sale-requests` lists pending requests, oldest
  first, at most 200. Strict administrators only.
- `POST /social-events/ticket-sale-requests/{requestId}/approve` and
  `POST /social-events/ticket-sale-requests/{requestId}/decline` decide one
  request. Strict administrators only.

## Standard terms

Every request carries the same terms; neither the manager nor the approving
administrator edits them through these routes:

| Field | Value |
| --- | --- |
| Currency | USD |
| Buyer fee | 200 basis points |
| Organizer fee | 200 basis points |
| Tax | 0 basis points, not included |
| Seat hold | 15 minutes |
| Tickets per order | at most 100 |
| Refund policy | `Reembolso total.` |
| Terms version | `ticket-terms-v1` |
| Transfers | allowed |
| Bank transfer, tax invoice | not enabled |

They are the terms of the one policy approved by hand before this contract
(event 121). The product owner chose them as the fixed standard on 2026-11-15.
Different terms still require an operator.

## State

The request is one `draft` row of `event_ticket_checkout_policy`. The state of
an event is derived from its rows, newest first:

1. an active row with `approval_status = 'approved'` gives `approved`;
2. otherwise a compatible, undecided workflow draft gives `requested`;
3. otherwise, if the newest compatible workflow row was declined, `declined`;
4. otherwise `none`.

The reserved request version ties the draft to the event and the requesting
party. Legacy drafts and incompatible workflow drafts are excluded from the
sale state and approval queue. A manager may file a fresh request alongside
those drafts. A policy that was approved and later retired allows a new request.

Allowed transitions:

- `none` or `declined` to `requested`, by a manager, while the event has an
  active ticket tier with a price above zero in USD;
- `requested` to `approved`, by a strict administrator, under the same tier
  condition and while the event has no other active policy;
- `requested` to `declined`, by a strict administrator.

Approval sets `approval_status = 'approved'`, `active = TRUE`, `approved_at`,
`effective_from` and `approved_by = 'party:<id>'` on the draft row. Declining
sets `approval_status = 'retired'`. No row is deleted and no other column
changes. The existing history trigger records each write. The approval
validates the stored commercial terms, including transfer settings, payment
method limits and the effective window. Public checkout reads the same
approved row and snapshots its identity, price policy and terms for the order.

The policy version is
`ticket-v1-event-<eventId>-req-<partyId>-<UTC instant to the microsecond>`, so
the requesting party stays readable in the append-only history. The table has
no requester column and this contract adds no migration.

## What approval does not do

Approval only supplies the approved policy. Public checkout still requires,
unchanged: an event in a state that allows ticket purchase, the
`commerce.event_tickets` flag, inventory and a payment provider that is
enabled and configured. An approved sale on an unpublished event sells nothing.

## Concurrency and repetition

Request and decision lock the `social_event` row `FOR UPDATE` and read the
policy rows and tiers again under it. The decision also locks the target policy
row before reading its terms. Simultaneous requests create one eligible draft;
the others return it. Requesting while a request is pending or approved
returns the current state and writes nothing. Deciding a request the same way
twice returns the recorded decision; deciding it the other way is refused with
409. The partial unique index on active policies and the policy lifecycle
trigger remain the database's own guard.

## Notices

After the transaction commits the server sends e-mail on a separate thread:
to every strict administrator except the requester on a new request, and to
the organizer on a decision. Mail is sent only when e-mail is configured, is
not retried, and a failure is written to the log buffer; it never changes the
recorded request or decision. There is no in-app notification.

## Failures

- 403: the caller is neither a manager nor a strict administrator (state and
  request), or is not a strict administrator (queue and decisions).
- 404: the event is missing or hidden; the request identifier is not a UUID or
  names no workflow request.
- 409: no active paid tier in USD (request and approval); the stored terms
  are incompatible; the request was already decided the other way; the event
  already has another active policy.

## Tests

The test suite in `tdf-hq/test/TDF/EventTicketSaleSpec.hs` covers state
derivation and the scoped policy version. The integration cases run through
`scripts/test-event-ticket-sale.sh` against a fully migrated disposable
PostgreSQL database. They include legacy draft isolation, rejection of changed
terms, the public checkout policy reader, manager and administrator access,
missing tiers, concurrent requests, and idempotent decisions. Jest covers
the event card, the review queue, and eligibility after creating or editing
a ticket tier on the event detail page.

## Not covered

- No model and no proof.
- The terms are not reviewed as a legal or tax position.
- No expiry of pending requests and no reason recorded for a decline.
- Retiring an approved policy still requires an operator.
- Mail delivery is not verified; a strict administrator who receives no mail
  finds the request in the queue on the events page.
- Order creation, payment capture and refund are outside this approval test suite.
