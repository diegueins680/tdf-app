# WhatsApp consent confirmation — PRIV-WHATSAPP-001

A public form cannot prove control of a phone number. A public request therefore
records a pending request and asks the number to confirm; marketing consent becomes
active only when that number itself sends an affirmative keyword through the signed
Meta webhook. The product owner chose double opt-in on 2026-10-06 (AUTHORITY-052).
Staff capture and staff revocation stay admin-only; they keep the request instant
like every other writer, and staff capture is outside the model.

One `whats_app_consent` row per number carries the state:

| State | Row |
| --- | --- |
| Pending, delivered | `consent=false`, `confirmation_requested_at` set, note `pending_confirmation`, not withdrawn |
| Pending, undelivered | as above with note `pending_unsent` |
| Active | `consent=true` |
| Withdrawn | `consent=false`, `revoked_at` at or after `confirmation_requested_at` (or no request) |

Rules:

- **Request.** One conditional update claims the confirmation message when there is
  no previous request or the previous request is more than 24 hours old; the request
  then takes the current instant. Creating the row uses `ON CONFLICT DO NOTHING`, so
  simultaneous first requests cannot fail.
- **Undelivered.** When the provider does not accept the message, only that exact
  request (matched by its request instant) is marked `pending_unsent`. For 1 hour from
  the request the number can still confirm it by messaging first, and a new public
  request may retry the send. A retry keeps the request instant, so repeated retries
  cannot extend the time in which a number that never saw a prompt could confirm.
  After that hour the request waits for the 24-hour interval like any other.
- **Confirm.** One conditional update activates consent when the reply's Meta
  timestamp is no earlier than the request, the request is inside its window (7 days
  delivered, 1 hour undelivered) and no withdrawal happened at or after the request.
- **Withdraw.** Opt-out (public form, STOP/SALIR/BAJA/CANCELAR, staff revocation) sets
  `revoked_at` and keeps `confirmation_requested_at`. The pending request can no longer
  be confirmed and the 24-hour interval is not reopened. Confirmation and staff capture
  also keep the request instant. A handler reads its clock before it locks the row, so
  the withdrawal's transaction raises `revoked_at` to the request instant when a request
  committed first with a later clock.

Delivered requests to one number are therefore at least 23 hours apart: a new request
needs 24 hours since the last request instant, and a retried send is delivered at most
1 hour after it.

The opt-out note is caller-supplied text and may equal a pending marker. That is
harmless: a withdrawn row is neither confirmable nor reclaimable until the interval
ends, whatever its note.

## Defects this contract repairs (AUTHORITY-055)

1. Every opt-out and confirmation cleared `confirmation_requested_at`. Because the
   public opt-out needs no proof of control, alternating the two public forms sent
   an unlimited number of confirmation messages to any number.
2. A rejected send cleared the pending request, so the success page's "Enviar SI por
   WhatsApp" button could never activate consent in exactly the case it exists for.
3. Simultaneous first requests for one number raced on the unique row and one failed.

Independent review of the first repair found two further paths, closed here: an
opt-out whose clock was read before a concurrent request could leave that request
unwithdrawn, and unlimited re-requests could keep an undelivered request confirmable
indefinitely.

These were implementation bugs against the stated requirement, not policy. No schema
change was needed; existing rows keep their meaning.

## Executable evidence and scope

`formal/event-operations/WhatsAppConsent.tla` models one number with a logical clock
`1..6`, a 3-tick request interval, a 3-tick delivered window and a 1-tick undelivered
window. Requests, provider outcomes (arriving arbitrarily late), holder replies
(delivered late, duplicated or replayed), confirmations and withdrawals interleave
freely; each is one committed transaction. A withdrawal may write any note and may
carry any earlier clock value. There is no
fairness assumption and no liveness claim: the model checks bounded safety only.

| Requirement property | Formal invariant | Negative control (single guard removed) | Runtime check |
| --- | --- | --- | --- |
| Consent only by the number's own reply, no earlier than the request | ConfirmedByOwnLaterReply | WhatsAppConsentStaleReply | Earlier-stamped reply rejected (SQLite, PostgreSQL) |
| A withdrawal committed after a request defeats it | WithdrawalWins | WhatsAppConsentWithdrawnReply, WhatsAppConsentStaleWithdrawalClock | Withdrawal holding the row wins over a waiting confirmation although the note is a pending marker; withdrawal with an earlier clock still defeats the request |
| Delivered requests are at least the interval less the retry window apart | DeliveredRequestsSpaced | WhatsAppConsentWithdrawalReset, WhatsAppConsentUnboundFailure | Claim refused after withdrawal; superseded failure ignored; one of six simultaneous claims wins |
| A request confirms only in its window; an undelivered one only within the short window of its first attempt | ConfirmedInsideWindow | WhatsAppConsentLongUnsentWindow, WhatsAppConsentRenewedUnsent | Undelivered request refused after 1 hour; retry keeps the request instant |
| An undelivered request stays pending | UndeliveredStaysPending | WhatsAppConsentReleasedFailure | Undelivered request confirmed by a later reply |

`tdf-hq/test/TDF/WhatsAppConsentSpec.hs` runs the real statements against the fully
migrated disposable PostgreSQL fixture through `scripts/test-whatsapp-consent.sh`:
six simultaneous claims, a duplicated reply, a withdrawal that holds the row lock
while a confirmation waits, a withdrawal with an earlier clock, microsecond-rounded
request instants, and the retry, withdrawal and superseded-failure sequences above.
The simultaneous-claim and duplicated-reply cases overlap probabilistically; only the
withdrawal case uses a lock barrier. `tdf-hq/test/TDF/ServerSpec.hs` keeps the
keyword, window and public-lookup checks.

The model is not a verified translation of Haskell or PostgreSQL. It excludes staff
capture, message content and templates, several numbers, clock skew
between the server and Meta, webhook signature verification (AUTH-WEBHOOK-001), a
a process crash between claiming and recording the provider outcome (the request then
stays delivered-pending: conservative for the request interval, but an undelivered
request would keep the 7-day window), a request whose clock was read before a
withdrawal that committed first (it is sent but already withdrawn, which fails safe),
and a provider failure that hides an actual delivery. `WhatsAppConsentRenewedUnsent`
removes the retry bound and the kept instant together, since they are one rule; the
other seven controls each remove exactly one guard. Because opting out needs no proof of control, a third
party can withdraw someone else's pending request; that person must then wait for the
24-hour interval or ask staff. This is accepted in exchange for closing the repeated
message path. The 1-hour undelivered window is an engineering
default awaiting product-owner confirmation. Production has no WhatsApp Cloud API
configuration, so none of this has been exercised against the live provider.
