# Staff-verified bank transfer for public tickets

Decision of 6 October 2026 for PATCH CULTURE vol. 1 (event 141), built as a
reusable option of the versioned ticket policy.

## Contract

- A policy opts in with `manual_transfer_hold_minutes` (60–4320) and
  `manual_transfer_cutoff_at`; both or neither. Approved values are immutable.
- The buyer holds tickets through the normal checkout (10-minute hold), then
  chooses transfer. Choosing it is allowed only before the cutoff and only when
  the `bank_transfer` route is ready: an enabled `tdf-manual-settlement`
  provider account and `COMMERCE_BANK_TRANSFER_INSTRUCTIONS` configured.
- The checkout snapshot stays immutable. A separate
  `manual_hold_expires_at` can only grow while the hold is alive, after a
  bank-transfer attempt exists, never past `now + hold` or `cutoff + hold`, and
  the first extension must start before the cutoff. Selection extends to
  `min(now + hold, cutoff)`; reporting a transfer extends to
  `min(now + hold, cutoff + hold)` so staff can verify late reports.
  Expiry, payment validation and every API liveness check use the later of the
  two deadlines.
- The buyer reports a transfer reference (3–120 characters), optionally with a
  JPG, PNG or PDF receipt (maximum 5 MiB). That is evidence,
  never payment. The guest is recorded as a new unverified contact party, as
  public bookings do; the system never links a guest to an existing account
  by email.
- An event organizer or strict administrator approves or rejects with a note.
  The reviewer must differ from the submitting party and must not own the
  buyer's email. Approval binds the evidence, records the canonical
  staff-verified payment and then issues tickets through the same idempotent
  path as provider captures. Approval after the hold expired is refused and
  recorded as a reconciliation exception.
- A rejection note is shown to the buyer, who may report again while the hold
  is alive. Approval notes stay internal.

## Operator configuration

1. Store the account details in `COMMERCE_BANK_TRANSFER_INSTRUCTIONS` on the
   API host (`\n` renders as a line break). They are shown only after a buyer
   chooses transfer.
2. Enable the `bank_transfer` provider account for the checkout environment
   with merchant reference `tdf-manual-settlement`.
3. Create a new policy version with the manual transfer terms; approve it.

PATCH CULTURE uses a 24-hour hold and a 22 October 2026 18:00
America/Guayaquil cutoff.

## Evidence and limits

`tdf-hq/test/integration/ticket_manual_bank_transfer.sql` exercises the
database bounds, expiry with and without extension, settlement after the
original hold passed and rollback refusal. `scripts/test-ticket-manual-review.sh`
runs the actual staff review transaction on the production schema: reviewer
authority and independence, single settlement and replay, refusal after expiry,
rejection and re-report, and concurrent reviews. Web tests cover the buyer flow
and that a reported transfer is never shown as paid. The bounded model, its
mutation controls and the exclusions are in
[formal/system/manual-bank-transfer.md](../../formal/system/manual-bank-transfer.md).
Bank deposits are verified by people against the account statement; no bank API
is integrated.

## Private receipt files

The organizer opens **Comprobante: [número]** in the transfer card to preview
and download the attached file. Older transfers with only a reference show
that no image or PDF was attached; a reference is never treated as a file URL.

Set `TICKET_PAYMENT_EVIDENCE_DIR` to durable private storage before deploying
receipt uploads in production. It must be outside `assetsRootDir`, owned by the
API service account and included in private backup/recovery procedures. All API
replicas must have access to the same receipt storage. Production rejects file
uploads when this setting is absent. Development defaults to
`uploads/ticket-payment-evidence`.

Files are bound to their order using a SHA-256 object key in the existing
`commerce_manual_payment_evidence.evidence_object_key` column. Submitting an
identical reference and file is idempotent; evidence under review cannot be
replaced by another file. Only the authenticated event organizer or a strict
administrator can fetch receipt bytes, through the event/order scoped endpoint
`GET /social-events/events/{eventId}/ticket-orders/{orderId}/manual-payment/receipt`.
Responses use `Cache-Control: no-store`; the browser releases its temporary
preview URL when the dialog closes. No schema migration is needed.

For web/API rollout compatibility, the buyer form offers file attachments only
when the checkout response includes the `receiptAttached` Boolean. The previous
API omits that field and continues to accept reference-only submissions. This
keeps those transfers usable if the web release arrives before the API release.
