# WhatsApp consent confirmation — PRIV-WHATSAPP-001

A public form cannot prove control of a phone number. A public request therefore
records a pending request and asks the number to confirm; marketing consent becomes
active only when that number itself sends an affirmative keyword through the signed
Meta webhook. The product owner chose double opt-in on 2026-10-06 (AUTHORITY-052).
Staff capture and staff revocation stay admin-only and are outside this contract.

One `whats_app_consent` row per number carries the state:

| State | Row |
| --- | --- |
| Pending, delivered | `consent=false`, `confirmation_requested_at` set, note `pending_confirmation`, not withdrawn |
| Pending, undelivered | as above with note `pending_unsent` |
| Active | `consent=true` |
| Withdrawn | `consent=false`, `revoked_at` at or after `confirmation_requested_at` (or no request) |

Rules:

- **Request.** One conditional update claims the confirmation message when there is
  no previous request, the previous request is more than 24 hours old, or the previous
  request was undelivered and has not been withdrawn. Creating the row uses
  `ON CONFLICT DO NOTHING`, so simultaneous first requests cannot fail.
- **Undelivered.** When the provider does not accept the message, only that exact
  request (matched by its claim instant) is marked `pending_unsent`. It no longer blocks
  a new request, and the number can still confirm it by messaging first, but only for
  1 hour because the number never saw a prompt.
- **Confirm.** One conditional update activates consent when the reply's Meta
  timestamp is no earlier than the request, the request is inside its window (7 days
  delivered, 1 hour undelivered) and no withdrawal happened at or after the request.
- **Withdraw.** Opt-out (public form, STOP/SALIR/BAJA/CANCELAR) sets `revoked_at` and
  keeps `confirmation_requested_at`. The pending request can no longer be confirmed and
  the 24-hour interval is not reopened. Confirmation also keeps the request instant.

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

These were implementation bugs against the stated requirement, not policy. No schema
change was needed; existing rows keep their meaning.

## Executable evidence and scope

`formal/event-operations/WhatsAppConsent.tla` models one number with a logical clock
`1..6`, a 2-tick request interval, a 3-tick delivered window and a 1-tick undelivered
window. Requests, provider outcomes (arriving arbitrarily late), holder replies
(delivered late, duplicated or replayed), confirmations and withdrawals interleave
freely; each is one committed statement. A withdrawal may write any note. There is no
fairness assumption and no liveness claim: the model checks bounded safety only.

| Requirement property | Formal invariant | Negative control (single guard removed) | Runtime check |
| --- | --- | --- | --- |
| Consent only by the number's own reply, no earlier than the request | ConfirmedByOwnLaterReply | WhatsAppConsentStaleReply | Earlier-stamped reply rejected (SQLite, PostgreSQL) |
| A withdrawal at or after a request defeats it | WithdrawalWins | WhatsAppConsentWithdrawnReply | Withdrawal holding the row wins over a waiting confirmation |
| Delivered requests are more than one interval apart | OneDeliveredRequestPerInterval | WhatsAppConsentWithdrawalReset, WhatsAppConsentUnboundFailure | Claim refused after withdrawal; superseded failure ignored; one of six simultaneous claims wins |
| An undelivered request confirms only in its short window | ConfirmedInsideWindow | WhatsAppConsentLongUnsentWindow | Undelivered request refused after 1 hour |
| An undelivered request stays pending | UndeliveredStaysPending | WhatsAppConsentReleasedFailure | Undelivered request confirmed by a later reply |

`tdf-hq/test/TDF/WhatsAppConsentSpec.hs` runs the real statements against the fully
migrated disposable PostgreSQL fixture through `scripts/test-whatsapp-consent.sh`:
six simultaneous claims, a duplicated reply, a withdrawal that holds the row lock
while a confirmation waits, microsecond-rounded claim instants, and the withdrawal
and superseded-failure sequences above. `tdf-hq/test/TDF/ServerSpec.hs` keeps the
keyword, window and public-lookup checks.

The model is not a verified translation of Haskell or PostgreSQL. It excludes staff
capture and revocation, message content and templates, several numbers, clock skew
between the server and Meta, webhook signature verification (AUTH-WEBHOOK-001), a
process crash between claiming and recording the provider outcome (the request then
stays delivered-pending, which is the conservative side), and a provider failure
that hides an actual delivery. The 1-hour undelivered window is an engineering
default awaiting product-owner confirmation. Production has no WhatsApp Cloud API
configuration, so none of this has been exercised against the live provider.
