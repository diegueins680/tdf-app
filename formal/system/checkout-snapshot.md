# Checkout line snapshot (PAY-CHECKOUT-002)

Slice of 2026-10-12 on main `86b6611fc`. PAY-CHECKOUT-002 states that every
committed canonical checkout has a nonempty, immutable line snapshot whose
exact sum is its immutable payable total. The 2026-10-04 migration enforces
the sum at commit (deferred triggers), freezes the header's monetary terms
and refuses any update or delete of a line. This slice takes the part the
statement claims and the rules did not hold: the snapshot itself.

## What was wrong

Probed on a copy of the production schema (2026-10-12): a line whose total
is zero was appended to a committed checkout by a later transaction. The
deferred sum check passed (the sum did not change), the line-immutability
trigger does not fire on INSERT, and the checkout's snapshot gained a line
nobody paid for; refund allocation and receipts read the lines. The
2026-10-04 note calls this out as "zero appends preserve the invariant":
true of the sum, not of the snapshot. Both Haskell writers (the checkout
store, the merchandise checkout) create the header and all its lines in one
transaction, so neither reaches this path; the rule did not hold against any
other writer.

A first version of this slice refused a line whose checkout row had a
positive `age(xmin)`. The independent review broke it twice: an UPDATE of
any header column (the lifecycle and counter updates the requirement allows)
re-stamps `xmin` with the updating transaction's id, so "update the header,
then append" was admitted; and under READ COMMITTED a transaction whose id
(or cached reference id) predates the header's could append after the
commit, because numeric xid order is not commit order. Both are now test
cases, and the mechanism is a different one.

## The fix (`tdf-hq/sql/2026-10-12_checkout_line_snapshot_fixed.sql`, manifest entry 203)

Two triggers. `trg_commerce_checkout_created` fires `AFTER INSERT`, per
row, on `commerce_checkout_session` and appends the new id to the
transaction-local setting `tdf.commerce_checkouts_created`
(`set_config(..., true)`: SET LOCAL semantics, gone at commit or rollback,
undone by a rollback to a savepoint). An AFTER row trigger fires only for
rows actually inserted, so an `INSERT ... ON CONFLICT` that matches an
existing header (and inserts nothing, or updates it) registers nothing; the
review found that a BEFORE trigger did.
`trg_commerce_checkout_line_fixed` fires `BEFORE INSERT` on
`commerce_checkout_line_item` and refuses a line whose `checkout_id` is not
in that setting ("was not created by this transaction; lines are created
with their checkout", SQLSTATE 23514). So a header committed by any other
transaction, whenever that transaction began; a header this transaction only
updated; a header another session has not committed; a header this
transaction inserted and rolled back to a savepoint; a header an upsert
matched without inserting; and an unknown id are all the same case. One
shape is refused that the 2026-10-04 rules accepted: a header and its line
in one statement (a data-modifying CTE), because AFTER row triggers run at
the end of the statement. No writer in the repository uses it; the test
documents it. The deferred sum check of 2026-10-04 is unchanged and
keeps validating the snapshot at commit; the 2026-10-04 line and header
triggers are unchanged. The release gate expects both new triggers (`AFTER
INSERT` on the header, `BEFORE INSERT` on the line) as row triggers on their
functions, with no column list and no `WHEN` clause, next to the three
2026-10-04 triggers. Rollback drops the two
triggers and functions and leaves the 2026-10-04 rules in place. No Haskell
change. Decision in AUTHORITY-070.

The setting is per transaction, not per connection: a pooled connection
carries nothing from one transaction into the next (the test reads it back
empty outside a transaction; the review confirmed it after COMMIT, ROLLBACK,
a failed transaction, DISCARD ALL and a prepared transaction). Ids are
stored comma-delimited and matched with their delimiters; UUID text has a
fixed width, so a match cannot straddle two ids. The registry is not a
privilege boundary: any role that can INSERT can also `set_config` the
setting by hand and append a line (the review did, with a table-privileges
role that could not disable the triggers). It guards against mistaken
writers; a hostile SQL writer is outside the 2026-10-04 scope as before. A
writer that issues `RESET ALL`, resets the setting, or calls a function
declared with `SET tdf.commerce_checkouts_created` between its header and
its lines refuses its own lines; neither Haskell writer does.

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
`CommittedSum`), `MutableTerms` (`TermsImmutable` off: `TermsFixed`). The
model states the guard "only the creating transaction inserts lines" and
checks that it, with the deferred check and the frozen terms, implies the
invariants; it does not model the mechanism that enforces the guard (the
transaction-local registry), nor header updates, nor the order in which
transactions began. Those are what the PostgreSQL test exercises.
Abstractions: lines are a bag of small amounts (the exact `numeric` sum is a
property of PostgreSQL, tested, not modelled); one late transaction and no
isolation levels (the six racing appends and the pre-enforcement snapshot
fence are tested, not modelled); line update, delete and header delete are
refused by the 2026-10-04 and 2026-08-13 rules and are not actions.

## Evidence

| Check | Where | Result (2026-10-12, local) |
| --- | --- | --- |
| Model and three controls | `scripts/verify-event-operations-formal.sh` | PASS; controls fail on their named invariant; full runner exit 0 |
| `checkout-amount-postgres.mjs` (2026-10-04 cases unchanged, run in a window without the new rule where they append to a committed checkout; new: header and lines in one transaction with a read first, a self-update and a subtransaction; header re-inserted after a subtransaction rollback; zero-total append refused, also as the first statement of a transaction, after a header update in the same transaction, and under repeatable read; unknown checkout and a header rolled back to a savepoint refused; the setting reads back empty outside a transaction; another session's uncommitted header refused while that session still commits with its own line; a session that began and wrote before the checkout was created and committed cannot append to it; two negative controls (the sum check alone admits a zero append, with and without a header update); upserts that insert nothing or update the header refused, an upsert of a new header commits, header and line in one statement refused; the schema gate rejects either new trigger disabled) | CI step "Verify checkout monetary snapshots and invalid-schema controls" | PASS locally on a schema copy |
| Release-gate unit tests | `scripts/__tests__/production-release.test.mjs` | 61 pass |
| Migration twice, rollback, forward again; probes before and after | schema copy | clean; the gap refused after, admitted after the rollback script |
| Independent review probes (header updates, xid order, savepoints, EXCEPTION blocks, connection reuse, prepared transactions, 3,000 checkouts in one transaction, upserts, CTEs, a table-privileges role) | reviewer's scratch databases | two mechanisms rejected (`age(xmin)`, BEFORE registration); the final one holds except the documented forgery and CTE shape |
| Automatic-migration production-schema test | `scripts/test-automatic-migrations-production-schema.sh` | see PR |

## Left open

- Production was not queried: whether any committed checkout already carries
  a line appended after its commit is unknown, and nothing in the rows
  identifies such a line reliably (`xmin` is re-stamped by header updates).
  Only audit and the domain records can tell; the migration does not look.
- The deferred checker's "checkout not found" branch (2026-10-04) returns
  silently; it is unreachable through a line insert only while the new line
  trigger is enabled.
- `tdf-hq/test/integration/artist_merch_runtime_fixture.sql` inserts headers
  and lines as separate autocommitted statements. Its runner applies it
  before the 2026-10-04 rules and never applies this migration, so it still
  passes; it must be wrapped in one transaction before that runner moves to
  a manifest-built database.
- Header deletion is refused by the line reference (`ON DELETE RESTRICT`)
  while lines exist; a committed checkout always has lines. Not modelled.
- `commerce_checkout_amount_boundary` and the SQLSTATE 40001 retry remain the
  2026-10-04 obligations; unchanged.
