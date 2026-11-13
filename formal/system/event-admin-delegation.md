# Delegated event administrators

EVT-ADMIN-DELEGATION-001 describes how the organizer of one event lets another
account help run it, and the courtesy tickets its managers issue. It is derived
from the handlers in `tdf-hq/src/TDF/Server/EventAdminDelegation.hs`
(`eventAdminsServer`, `eventManagerPartyFor`, `delegatedEventAdminExists`),
`tdf-hq/src/TDF/Ticketing/Admission.hs` and the migration
`tdf-hq/sql/2026-11-11_event_admin_delegation.sql`. It is a description of the
shipped behaviour with finite tests, not a proof and not a legal or commercial
policy for complimentary admission.

## Actors

- **Organizer**: the party stored in `social_event.organizer_party_id`.
- **Delegated administrator**: a party with an `event_admin_member` row for
  that event. The grant is per event, carries no global role and no module, and
  is read from the database on each request.
- **Invited person**: whoever holds an unused invitation link and signs in.
- **Other signed-in accounts**, including strict administrators, who receive no
  authority from this contract. A strict administrator is not admitted to the
  administrator-management routes unless they are the stored organizer.

Every route requires an authenticated session. An event without a stored
organizer has no delegated administration: its access is reported as `none`,
the organizer-only routes refuse, and a grant row alone admits nobody.

## Routes

- `GET /social-events/events/{eventId}/admin-access` reports the caller's
  access to the event as `owner`, `admin` or `none`.
- `GET /social-events/events/{eventId}/admins` lists the delegated
  administrators (party identifier, display name, grant time) for the organizer
  or a delegated administrator. Only the organizer also receives the latest 50
  invitations with a derived status (`pending`, `accepted`, `revoked`,
  `expired`); a delegated administrator receives an empty invitation list.
  Other callers receive 403.
- `POST /social-events/events/{eventId}/admin-invitations` (organizer only)
  issues an invitation link token for a named invitee.
- `DELETE /social-events/events/{eventId}/admin-invitations/{invitationId}`
  (organizer only) revokes a pending invitation.
- `DELETE /social-events/events/{eventId}/admins/{partyId}` (organizer only)
  removes a delegated administrator.
- `POST /social-events/admin-invitations/preview` returns the event identifier,
  title, start time, invitee name and expiry of a usable invitation whose token
  is sent in the request body.
- `POST /social-events/admin-invitations/accept` redeems the token sent in the
  request body for the signed-in account.
- `POST /social-events/events/{eventId}/courtesy-tickets` issues courtesy
  tickets (organizer or delegated administrator).

## Grant lifecycle

1. **Issue.** The organizer supplies an invitee name (trimmed, 1 to 120
   characters, no control characters). The server generates a 64-character
   hexadecimal token from two random version-4 UUIDs, stores only its SHA-256
   digest, and returns the raw token once in the response. The invitation
   expires 14 days after creation. At most 10 invitations of an event may be
   pending (not accepted, not revoked, not expired) at once; the eleventh is
   refused with 409 until one is revoked or lapses.
2. **Preview and accept.** The first account that accepts a usable invitation
   is recorded on the invitation and receives the `event_admin_member` row,
   with the issuing organizer as grantor. The same account accepting again
   changes nothing and succeeds while its grant still exists. The organizer
   accepting their own link is refused with 409 and the link stays usable.
3. **Revoke.** The organizer may revoke a pending invitation; revoking an
   already revoked one succeeds without change; revoking an accepted one is
   refused with 409 (the administrator must be removed instead).
4. **Remove.** The organizer deletes the grant row. The removed account loses
   access on its next request, and replaying its consumed link does not
   restore the grant.

An invitation is unusable, with one indistinguishable 404, when the token is
malformed or unknown, the invitation is revoked, it expired before being
accepted, another account already accepted it, the event is hidden, or the
party that issued it is no longer the stored organizer of the event. Deleting
the event deletes its invitations and grants (`ON DELETE CASCADE`).

## What a delegated administrator can and cannot do

While the grant row exists and the event has a stored organizer, a delegated
administrator passes the organizer check of these existing operations on that
event: editing the event, uploading its image, creating and updating ticket
tiers, listing ticket orders and tickets, creating a direct zero-price ticket
order for another buyer, viewing ticket QR credentials, admitting tickets at
the door, planning logistics with the `owner` role, and issuing courtesy
tickets. They can read the administrator roster but not the invitations.

Every other organizer check is unchanged by this feature and still compares
the stored organizer (or applies the strict-administrator rule it already had).
In particular issuing and revoking invitations, removing administrators,
promotional codes, refund requests and decisions, manual payment review, tax
documents, order status changes, waitlist, budget and finance entries remain
with the organizer, and no event deletion, RSVP management or memory
moderation check was widened. Ticket transfers are not delegated: creating a transfer of, or listing the
transfers of, a ticket the caller does not hold requires the stored organizer.

## Courtesy tickets

A courtesy issue names one ticket tier of the event, a quantity from 1 to 10,
a guest holder name (required) and an optional guest email. It creates one
order with amount zero, status `paid`, no buyer party, no payment method, the
tier currency, and metadata recording `courtesy: true` and the issuing party.
Inventory is reserved through the same tier and event capacity reservation a
purchase uses, inside the transaction that holds the event row lock; when not
enough tickets remain the request is refused with 409 and nothing is written.
One ticket per unit is issued and, as for any issued order, the ticket
confirmation is sent to the guest email when one was given. No checkout,
payment intent, invoice or promotional code is involved.

The tier must be active and inside its sales window: otherwise the request is
refused with 409 "Courtesy tickets need an active ticket tier inside its sales
window", distinct from the 409 "Not enough tickets available" of an inventory
refusal. Courtesy issuance has no idempotency key (a repeated request issues
another order), no per-event or per-administrator cap beyond tier and event
capacity, and writes no separate audit row; the order metadata is the record
of who issued it.

## Concurrency

Issue, revoke, remove, preview, accept and courtesy issuance take the event
row lock (`SELECT ... FOR UPDATE` on `social_event`) and re-read the organizer,
the invitation and the grant under it. Simultaneous acceptances of one link are
therefore ordered: exactly one account is recorded and the rest receive 404.
Acceptance and revocation, and courtesy issuance and removal of the issuing
administrator, are ordered the same way. Ticket admission reads the grant row
under the same event lock. `UNIQUE (event_id, party_id)` and
`UNIQUE (token_sha256)` back these checks in the schema.

The event edit and ticket-tier writes read the grant before their own
transaction, so a removal that commits between that read and the write does
not stop the one request already in flight.

## Privacy of the token

- The raw token is returned once, to the organizer, and is never stored; the
  database holds its SHA-256 digest (`token_sha256`, 64 lowercase hex).
- Token routes take the token in the JSON body, never in a path or query.
- The web client builds the link as the public event page with the token in
  the URL fragment only (`#admin=<token>`), so the token is not sent to the
  server or proxies when the link is opened. The token is removed from the
  address bar after it is spent or refused, is stripped from login return
  targets and from analytics URLs, and is kept across the sign-in round trip
  only in tab-scoped session storage keyed by event.
- The roster shows invitee names and administrator display names to the
  organizer; a delegated administrator sees administrators only.
- The link itself is a bearer capability until first use: anyone who obtains
  it before the invited person can accept it. The invitee name is a label and
  is not matched against the accepting account.

## Failure modes

- 400: blank, overlong or control-character invitee name; malformed
  identifiers; courtesy quantity outside 1 to 10; missing or invalid holder
  name; invalid holder email.
- 403: caller is neither organizer nor (where admitted) delegated
  administrator; event has no stored organizer.
- 404: event missing or hidden; invitation or administrator not found on this
  event; any unusable invitation link; ticket tier not on this event.
- 409: pending-invitation limit reached; revoking an accepted invitation;
  organizer accepting their own link; a courtesy issue on an inactive tier
  or outside its sales window; not enough tickets for a courtesy issue.

A failed request writes nothing. There is no automatic retry; the web client
keeps the token and offers a retry after a transient failure.

## Evidence and limits

`tdf-hq/test/TDF/EventAdminDelegationSpec.hs` holds six PostgreSQL examples
run by `scripts/test-event-admin-delegation.sh` against a fully migrated
disposable database: organizer-only issue and digest-only storage; acceptance
and its idempotence; binding to one account, one event and the issuer's
authority; eight accounts racing for one link; revocation and removal;
logistics, courtesy issuance, capacity refusal and admission by a delegated
administrator. The hosted workflow runs that script in
`.github/workflows/ci.yml` after the organizer runner. Web tests cover the client calls, the organizer and delegated
views, the invitation panel and the fragment handling.

Not established: a bounded model; per-route tests for each existing operation
a delegated administrator now passes (event edit, image upload, tier writes,
order and ticket listing, QR) beyond reading the code; a test of the courtesy
sales-window refusal; expiry at the exact
boundary; the OpenAPI document and the Mobile client, which do not describe
these routes; notification of the organizer when a link is accepted; and
whether courtesy issuance should have its own quota or finance reporting.
