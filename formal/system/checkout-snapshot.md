# Checkout line snapshot (PAY-CHECKOUT-002)

Slice of 2026-10-12 on main `86b6611fc`. PAY-CHECKOUT-002 states that every
committed canonical checkout has a nonempty, immutable line snapshot whose
exact sum is its immutable payable total. The 2026-10-04 migration enforces
the sum at commit (deferred triggers), freezes the header's monetary terms
and refuses any update or delete of a line. This slice takes the part the
statement claims and the rules did not hold: the snapshot itself.

## What was wrong

Probed on a copy of the production schema (2026-10-12):

1. a line whose total is zero was appended to a committed checkout by a
   later transaction: the deferred sum check passed (the sum did not change),
   the line-immutability trigger does not fire on INSERT, and the checkout's
   snapshot gained a line nobody paid for (a free product on a paid
   checkout; refund allocation and receipts read the lines);
2. a line could be appended to another transaction's checkout as soon as its
   header committed: the reference check waits for that commit and then
   passes, and a zero line again passes the sum check.

The 2026-10-04 note calls the first case out as "zero appends preserve the
invariant": true of the sum, not of the snapshot. Both Haskell writers (the
checkout store, the merchandise checkout) create the header and all its lines
in one transaction, so neither reaches these paths; the rules did not hold
against any other writer.

## The fix (`tdf-hq/sql/2026-10-12_checkout_line_snapshot_fixed.sql`, manifest entry 203)

`trg_commerce_checkout_line_fixed` fires `BEFORE INSERT` on
`commerce_checkout_line_item`. It reads `age(xmin)` of the checkout row: zero
or negative for a row written by this transaction or one of its
subtransactions, positive for a row written by an earlier transaction, and
"infinitely old" for a frozen row. A positive age is refused ("is committed;
its line snapshot is fixed"), and a checkout that is not visible to the
transaction is refused before the reference check ("is not visible to this
transaction"), so an insert never waits on another transaction's header.
Both refusals raise SQLSTATE 23514. The deferred sum check of 2026-10-04 is
unchanged and keeps validating the snapshot at commit; the 2026-10-04 line
and header triggers are unchanged. The release gate now expects this trigger
as a `BEFORE INSERT` row trigger on its function, with no column list and no
`WHEN` clause, next to the three 2026-10-04 triggers. Rollback drops the
trigger and function and leaves the 2026-10-04 rules in place. No Haskell
change. Decision in AUTHORITY-070.

`age(xmin)` is stable within a transaction: it is measured against the
transaction's own id once one is assigned, otherwise against the next id at
first use. A header inserted earlier in the same transaction (the writers'
order) therefore has an age of zero or less, and a header committed before
the transaction a positive one, under every isolation level.

## Model

`formal/event-operations/CheckoutSnapshot.tla`: one checkout with a payable
total in 1..3, lines as a bag of amounts 0..2 (at most three), a creating
transaction that adds lines then commits or aborts, one later transaction
that may append lines and commit (after the deferred check) or abort, and a
change of the committed total. Invariants: `CommittedSum` (committed =>
lines, and their exact sum is the total), `SnapshotFixed` (the lines are
those of the commit), `TermsFixed` (the total is the one of the commit). 200
states generated, 85 distinct, depth 7. Three controls remove one guard each:
`LateLines` (`LinesFixed` off: `SnapshotFixed` fails through a zero line, the
sum check still passing), `Unvalidated` (`CommitValidated` off:
`CommittedSum`), `MutableTerms` (`TermsImmutable` off: `TermsFixed`).
Abstractions: lines are a bag of small amounts (the exact `numeric` sum is a
property of PostgreSQL, tested, not modelled); one late transaction and no
isolation levels (the six racing appends and the pre-enforcement snapshot
fence are tested, not modelled); line update, delete and header delete are
refused by the 2026-10-04 and 2026-08-13 rules and are not actions.

## Evidence

| Check | Where | Result (2026-10-12, local) |
| --- | --- | --- |
| Model and three controls | `scripts/verify-event-operations-formal.sh` | PASS; controls fail on their named invariant |
| `checkout-amount-postgres.mjs` (2026-10-04 cases unchanged, run in a window without the new rule where they append to a committed checkout; new: header and lines in one transaction with a subtransaction; zero-total append refused, also as the first statement of a transaction; line for an unknown checkout refused before the reference check; line for another session's uncommitted checkout refused without waiting, and that session still commits; the sum check alone admits a zero append (negative control); the schema gate rejects the new trigger disabled) | CI step "Verify checkout monetary snapshots and invalid-schema controls" | PASS locally on a schema copy |
| Release-gate unit tests | `scripts/__tests__/production-release.test.mjs` | 61 pass |
| Migration twice, rollback, forward again; probes before and after | schema copy | clean; both gaps refused after, admitted after the rollback script |
| Automatic-migration production-schema test | `scripts/test-automatic-migrations-production-schema.sh` | see PR |

## Left open

- Production was not queried: whether any committed checkout already carries
  a line appended after its commit is unknown. Such a line is detectable only
  by its `xmin` relative to the header's and by audit; the migration does
  not look for it.
- The deferred checker's "checkout not found" branch (2026-10-04) still
  returns silently; with the new rule a line whose checkout is invisible is
  refused before it, so the branch is unreachable through a line insert.
- Header deletion is refused by the line reference (`ON DELETE RESTRICT`)
  only while lines exist; a committed checkout always has lines, so a header
  cannot be deleted. Not modelled.
- `commerce_checkout_amount_boundary` and the SQLSTATE 40001 retry remain the
  2026-10-04 obligations; unchanged.
