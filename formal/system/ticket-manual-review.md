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

The database refuses an evidence change that does not start from `submitted`,
so in the two-reviewer interleavings tested and modelled one decision stood and
no second payment was recorded. But the second reviewer's write was refused as
a database error, which the handler does not catch, instead of telling them the
transfer was already decided. With two approvals, the second reviewer saw an
error for an order that had in fact been paid.

One interleaving the guard did not cover: a reviewer who read the evidence,
lost the lock, and wrote only after a colleague's rejection and the buyer's
resubmission would have approved a reference they had not read. The window was
small and is closed by this change for the write; see "Left open" for the same
gap at the level of what the reviewer saw on screen.

## What changed

The `transactionSave` is removed. The locks taken by the read now last until
the review commits, so a second reviewer waits and then reads the decision:
a repeated approval or a repeated rejection is answered as done, anything else
as a refusal that says why. Nothing else in the function changed; a refusal that follows a failed
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
payment; without it, a second payment is reachable in the model. In the code
the payment recorder and unique indexes would also refuse one; they are not
modelled.

Bounded model checking of a coarse abstraction: deciding and writing are one
step and the lock is a flag, so it says nothing about PostgreSQL's re-read
after a wait, deadlocks, the buyer resubmitting, the seat hold, time or
issuance.

## Tests

`tdf-hq/test/TDF/TicketManualReviewSpec.hs`, run by
`scripts/test-ticket-manual-review.sh` against a database built from the
production snapshot and the migration manifest. Each case builds an event with
an approved bank transfer policy, a held ticket order, its checkout and a
submitted reference, and calls the function the handler calls.

Run locally on 2026-10-11 against PostgreSQL 16: 12 cases, 0 failures, and no
row left in any table afterwards (row counts of all 701 tables equal to the
template's). Against the code before the change the two overlap cases fail
with `Invalid manual evidence transition: approved -> under_review` (or
`rejected -> under_review`) and the other ten pass.

- approval by the organizer records the payment; a repeated approval is
  answered without another; an administrator who is not the organizer may
  approve;
- a reviewer who is neither, the submitter (even as administrator), and a
  reviewer whose email is the buyer's are refused with the stated reason and
  nothing changes;
- rejection records no payment, can be repeated, and cannot then be turned
  into approval; an approved transfer is not changed by a rejection;
- approval at a time after the seat hold is refused and recorded for
  reconciliation;
- an order reviewed under another event is refused;
- approval overlapping rejection, and approval overlapping approval, four times
  each. The overlap is forced: the checkout row is held until both reviews are
  waiting for it. Both reviewers are answered and the final state is one
  decision.

The schema allows one posted capture per attempt, so the payment count in these
cases can only be 0 or 1; a second payment would appear as an error, which the
overlap cases would report.

## Not covered

- Ticket issuance after approval (`finalizePaidTicketOrder`) and the HTTP
  handler around the review are not run by this test.
- The fixture inserts the order, checkout, attempt and reference directly. It
  creates no payment intent, so the trigger that moves an intent to captured on
  approval is not reached; the buyer's path that creates these rows is covered
  by the existing checkout tests.
- Not reached by any case: the undo after a failed binding or payment record,
  evidence left under review by another reviewer, a checkout already paid by
  another attempt, and a hold that has really expired (the case passes a later
  time instead).
- No real transfer was made or verified.

## Left open

Found by the independent review, present before this change and not fixed by
it:

- A reviewer and the buyer acting on the same order at the same moment take
  the checkout, attempt and evidence locks in different orders
  (`EventTicketCheckout.hs`, the reference submission and the bank transfer
  selection). PostgreSQL resolves it by failing one of them with a deadlock
  error. Reproduced with two sessions and an artificially widened window.
- The hold is compared with the handler's clock, read before the transaction.
  An approval in the instant the hold expires is refused by the database with
  an error rather than by the function with its reason.
- The review request carries only the action and notes. If a colleague rejects
  and the buyer resubmits a different reference while a reviewer has the old
  one on screen, that reviewer's approval applies to the new reference.
