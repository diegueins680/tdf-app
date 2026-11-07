# Ledger posting integrity (SYS-MONEY-003)

Slice of 2026-10-12 on main `86b6611fc`. SYS-MONEY-003 required exact
per-currency sums in the Haskell ledger validation; its note left "SQL ledger
correctness" as a separate open obligation. This slice takes that obligation:
the `commerce_ledger_transaction` / `commerce_ledger_entry` rules of the
2026-08-13 migration, which every writer reaches (the checkout and refund
stores post through plain SQL, the 2026-08-17 deposit settlement posts from a
trigger function).

## What was wrong

The 2026-08-13 rules validated a transaction only on the status change
`draft -> posted` (entries present, zero sum per currency) and protected its
entries only against `UPDATE` and `DELETE`. Probed on a copy of the production
schema (2026-10-12), each of these passed:

1. an entry inserted into a posted transaction (its sum became 999);
2. a posted transaction returned to `draft` (where its entries were editable
   again, and a second posting replaced the first);
3. a transaction inserted already `posted` with no entries;
4. a transaction inserted already `reversed`, or reversed by itself, by a
   draft, or by nothing in particular;
5. (found by the independent review) an entry of a draft moved into a posted
   transaction by changing its `transaction_id`, since the entry rule looked
   only at the old transaction; and a reversed transaction re-pointed at any
   `reversal_of`.

The sum itself was already exact: PostgreSQL sums `bigint` as `numeric`. The
Haskell writers insert a draft, its entries, then post, so they did not hit
these paths; the rules did not hold against any other writer.

## The fix (`tdf-hq/sql/2026-10-12_ledger_posting_integrity.sql`, manifest entry 203)

`trg_commerce_ledger_posting` now fires `BEFORE INSERT OR UPDATE OF status,
reversal_of`. It refuses: creation in any status but `draft`; `posted ->
draft`; `draft -> reversed`; any status change of a `reversed` transaction;
`posted -> reversed` unless `reversal_of` names another transaction that is
`posted`; and `draft -> posted` without entries or with a non-zero sum in any
currency (`SUM(amount_minor::numeric)`). Every refusal raises SQLSTATE 23514.
`trg_commerce_ledger_entry_immutable` now fires `BEFORE INSERT OR UPDATE OR
DELETE`, refuses a change of `transaction_id`, and locks the transaction row
`FOR NO KEY UPDATE`, so an entry and a posting of the same transaction
serialise: either the entry lands in the
draft and the posting sees it, or the posting lands first and the entry is
refused. A reversed transaction changes neither status nor `reversal_of`. The
migration adds the missing index on `commerce_ledger_entry(transaction_id)`,
which posting, the entry rule and the foreign key all read by. Posted
transactions found unbalanced or empty, and reversed transactions with a self,
draft or reversed target, cause migration to fail before replacing guards.
History must be reconciled explicitly; the migration does not alter amounts.
Validation and guard installation hold table locks against concurrent writers. No Haskell change. The historical rollback restores the 2026-08-13 functions and
triggers and must not be used for application rollback after integrity guards are installed. Decision in AUTHORITY-069.

## Model

`formal/event-operations/LedgerPosting.tla`: one transaction, two
currencies, entries of amounts -2..2, at most four entries, a reversing
transaction with its own draft/posted/reversed lifecycle. Actions: create (as draft, or
in any status when `DraftOnly` is off), add entry, post, reopen, advance the
reverser, reverse. Invariants: `PostedBalanced` (posted or reversed =>
entries and zero sum per currency), `PostedImmutable` (the entries are those
of the posting), `PostedHistory` (of the first posting), `ReversedByPosted`.
4,444 states generated, 995 distinct. Five controls remove one guard each:
`Append` (`EntriesFixed` off: `PostedImmutable`), `Unvalidated`
(`PostValidated` off: `PostedBalanced`), `Reopen` (`NoReopen` off:
`PostedHistory`), `InsertPosted` (`DraftOnly` off: `PostedBalanced`); `ReverserMutable` violates `ReversedByPosted`
by allowing a referenced posted reverser to become reversed.
Abstractions: entries are a per-currency balance and a count, so an entry's
update, deletion or move between transactions are not actions (the rule is
tested, not modelled); amounts are small integers (the exactness of the SQL
sum is a property of `numeric`, tested, not modelled); the reverser's own
entries are not modelled; concurrency is not modelled (the row lock is
tested, with two sessions, by the independent review and by the spec's racing
case).

## Evidence

| Check | Where | Result (2026-10-12, local) |
| --- | --- | --- |
| Model and five controls | `scripts/verify-event-operations-formal.sh` | PASS; controls fail on their named invariant |
| `ledger-posting-postgresql` (9 cases: balanced post; unbalanced and empty refused; two `maxBound` entries and +2 refused, which a 64-bit sum would accept; no insert, update, delete or move once posted; no reopening; draft-only creation; reversal rules incl. no re-pointing and stable posted reversers; an entry racing a posting leaves a draft with the entry or a posted balanced transaction; cleanup needs table ownership for `DISABLE TRIGGER`) | `scripts/test-ledger-posting.sh`, CI step with the credential lifecycle spec | see PR |
| Migration twice, rollback twice, forward again; the ten probes above after the migration | schema copy | clean; every gap refused, valid paths pass |
| Automatic-migration production-schema test | `scripts/test-automatic-migrations-production-schema.sh` | see PR |

## Left open

- Production was not queried: whether any posted transaction has drifted is
  unknown until migration preflight validates the deployed data.
- `reversed` is not used by any writer today; the reversal rule is a boundary
  for a future writer.
- The Haskell `validateLedgerBalance` (Integer sums) still has no runtime
  caller; the database rule is the enforced one.
- A `commerce_ledger_entry` of a currency other than the checkout's is
  accepted; balance is per currency, so a stray currency needs its own
  counter-entry to post.

The reversal guard writes the target row without changing its `created_at` value.
This serializes a new reference with reversal of its target and forces a stale
REPEATABLE READ snapshot to abort; a read lock alone permits write skew there.
The database regression witnesses the blocked session before committing the
reference, then checks refusal under READ COMMITTED and serialization failure
under REPEATABLE READ. Five legacy fixtures (empty, unbalanced, self-reference,
draft target and reversed target) refuse migration without replacing guards;
clean repeated application passes. These cases run in `ledger-posting-upgrade.py`.

The provider retry/refund harness also applies the posting-integrity and fixed
checkout-line migrations before exercising its actual payment and refund stores.
This prevents that integration gate from silently omitting the new constraints.
Its reduced domain fixtures do not establish compatibility with every production
domain trigger; see `docs/payments/consolidation-2026-10-15.md` for that boundary.

## Snapshot refinement follow-up (2026-10-16)

The lock-only entry fence above is insufficient for an older REPEATABLE READ
snapshot. A real two-session regression posted an unbalanced transaction on the
#551 head. The additive `2026-10-16_ledger_entry_write_fence.sql` physically
updates the parent row without changing its timestamp value. READ COMMITTED
posting validates refreshed entries; stale snapshot writers abort with 40001.
The historical migration is unchanged. Retain this forward-compatible guard
through application rollback.

The detailed refinement ledger, reproducible cases, negative controls, source
identities and unresolved obligations are in
[ledger-refinement-2026-10-16.md](ledger-refinement-2026-10-16.md).
`LedgerPosting.tla` treats posting as atomic and does not model PostgreSQL
snapshots; passing it never established this missing implementation refinement.
SYS-MONEY-003 remains PARTIAL.
