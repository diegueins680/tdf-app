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
- The buyer reports a transfer reference (3–120 characters). That is evidence,
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
original hold passed and rollback refusal. Web tests cover the buyer flow and
that a reported transfer is never shown as paid. Bank deposits are verified by
people against the account statement; no bank API is integrated.
