# Staff review of a ticket bank transfer

Requirement `EVT-TICKET-MANUAL-001`, decision `AUTHORITY-064`.

## What was checked

`reviewTicketManualPayment` (`tdf-hq/src/TDF/Server/TicketManualPayments.hs`) is
the transaction behind the organizer's approve and reject buttons for a bank
transfer. It had no test of its own; the requirement recorded that it was
exercised only through deployed HTTP verification. It is now run directly
against PostgreSQL.

## What was wrong

The function reads the checkout, attempt and evidence rows with `FOR UPDATE`,
decides from what it read, and then called `transactionSave` before writing.
That commit released the row locks, so a second reviewer could read the same
`submitted` evidence while the first was writing.

The database refuses an evidence change that does not start from the state
under review, so the outcome was never wrong: one decision stood and at most
one payment was recorded. But the second reviewer's write was refused as a
database error, which the handler answers with 500 instead of telling them the
transfer was already decided. With two approvals, the second reviewer saw an
error for an order that had in fact been paid.

## What changed

The `transactionSave` is removed. The locks taken by the read now last until
the review commits, so a second reviewer waits and then reads the decision:
a repeated approval is answered as success, anything else as a refusal that
says why. Nothing else in the function changed; a refusal that follows a failed
binding or payment record still undoes the whole review.

## Model

`formal/event-operations/TicketManualReview.tla`: three reviewers (organizer,
administrator, and the buyer who submitted the reference), each reading,
deciding from what was read and writing.

| Configuration | Switch | Result |
| --- | --- | --- |
| `TicketManualReview.cfg` | lock held, transition guard, independent reviewer | 232 distinct states, no violation |
| `TicketManualReviewLockReleased.cfg` | lock released after the read (the code before this change) | `EveryoneAnswered` violated |
| `TicketManualReviewSubmitterReviews.cfg` | submitter may review | `NoSelfApproval` violated |
| `TicketManualReviewLockReleasedGuarded.cfg` | lock released, safety invariants only | 496 distinct states, no violation |
| `TicketManualReviewLockReleasedUnguarded.cfg` | lock released and no transition guard | `OnePayment` violated |

The fourth and fifth rows show what the database guard was carrying before
this change: with it, releasing the lock cost an error but not a wrong
payment; without it, a second payment is reachable.

Bounded model checking of an abstraction. The payment recorder's own
idempotency, the seat hold, time and issuance are not modelled.

## Tests

`tdf-hq/test/TDF/TicketManualReviewSpec.hs`, run by
`scripts/test-ticket-manual-review.sh` against a database built from the
production snapshot and the migration manifest. Each case builds an event with
an approved bank transfer policy, a held ticket order, its checkout and a
submitted reference, and calls the function the handler calls.

Run locally on 2026-10-11 against PostgreSQL 16: 12 cases, 0 failures, five
runs in a row, and no row left in any table afterwards. Before the change the
two race cases failed (one reviewer received a database error); the other ten
passed.

- approval by the organizer records one payment; a repeated approval records
  no second one; an administrator who is not the organizer may approve;
- a reviewer who is neither, the submitter (even as administrator), and a
  reviewer whose email is the buyer's are refused and nothing changes;
- rejection records no payment and cannot then be turned into approval; an
  approved transfer is not changed by a rejection;
- approval after the seat hold has passed is refused and recorded for
  reconciliation;
- an order reviewed under another event is refused;
- approval racing rejection, and approval racing approval, eight times each:
  one outcome, at most one payment, and both reviewers answered.

## Not covered

- Ticket issuance after approval (`finalizePaidTicketOrder`) and the HTTP
  handler around the review are not run by this test.
- The fixture inserts the order, checkout and reference directly; the buyer's
  path that creates them is covered by the existing checkout tests.
- No real transfer was made or verified.
