# Posted ledger headers

Requirement: PAY-LEDGER-HEADER-001

For an SQL UPDATE whose existing transaction is posted or reversed, every
column except `status` and `reversal_of` must retain its value. Source identity,
primary key and audit metadata are part of the posted accounting record.
Corrections use compensating transactions. Draft header edits and identical
updates remain valid; the latter preserve the existing physical parent-write
fence used by entry mutations. Status and reversal transitions retain their
independent posting-validator rules.

Authority: the user's financial-correctness mandate explicitly requires
immutable posted accounting records; accepted ADR-0104 requires immutable
financial events and compensating corrections. This scoped requirement adds no
refund authority or provider activation rule.

## Counterexample and repair

On the complete 220-migration financial candidate
`fa5a34792cee86cdf309a81a362de52032aa0b9c`, an ordinary non-owner with only
schema USAGE and ledger-header SELECT/UPDATE can change `transaction_type`,
`source_type` or `source_id` after balanced posting. No entry UPDATE, public
CREATE, table ownership or superuser permission is needed. This loses the
original source/reconciliation anchor; it is not evidence of a duplicate charge.

The additive `2026-10-25_ledger_posted_header.sql` migration installs a BEFORE
UPDATE trigger comparing the old and new header projections with null-safe
`IS DISTINCT FROM`. Only the two separately validated lifecycle columns are
excluded. The function runs as SECURITY INVOKER with `search_path=pg_catalog`.
Its original introduction commit is retained in the migration registry. No
historical SQL or source attribution is rewritten. Keep the guard through
application rollback; deployment and production migration require their existing
approvals. Installation takes a table lock with a ten-second lock timeout.

## Scope and verification

Preconditions and environmental assumptions: the registered posting, entry
write-fence and namespace guards remain enabled; the existing lifecycle cannot
reopen a draft; the actor cannot change DDL, bypass triggers, change replication
mode or replace trusted functions. Superusers/owners and historical attribution
repair are outside this claim. PostgreSQL enforces row-write serialization.

Postcondition: an accepted update to an existing posted/reversed row preserves
the header projection; a differing projection fails with SQLSTATE 23514. A writer
whose repeatable snapshot predates a concurrent posting may instead fail with
SQLSTATE 40001. Whole-transaction retry remains the caller's responsibility.

`ledger-event-identity.mjs --negative-controls` checks all eight protected
columns for posted and reversed fixtures under an ordinary role, valid draft
edits, identical writes, four installed startup-guard mutations and one
guard-removal counterexample. It compares exact definitions, trigger settings
and migration receipts after rollback. The startup verifier checks the binding,
BEFORE ROW UPDATE event, enabled state, absence of column/WHEN restrictions,
trusted search path and invoker signature; receipts alone are insufficient.
It does not prove arbitrary installed function bodies equivalent to the SQL.

`ledger-posting-snapshot.py --negative-controls` additionally exercises a
draft-era ordinary writer blocked behind posting at READ COMMITTED, REPEATABLE
READ and SERIALIZABLE. `pg_blocking_pids` must witness the actual wait. Removing
only the header guard must admit the invalid source change at READ COMMITTED.
Existing balance, entry-write, reversal and migration controls remain active.
The backend CI lane executes these full-schema regressions.

`LedgerHeader.tla` abstracts one transaction, two header values and two queued
writers. The finite positive configuration preserves `PostedHeaderFrozen` over
48 distinct states (138 generated); disabling the guard must violate that exact
invariant. Posting and the non-reopening lifecycle are model assumptions.
The formal CI lane enforces both configurations. This is BOUNDED_VERIFIED for
the model and TESTED for named SQL executions, not a proof of PostgreSQL
refinement, all callers, all interleavings, liveness or deployed correspondence.
Complete implementation/refinement obligations remain UNPROVEN.

Source-bound commands, tool versions, counterexamples and qualification receipts
are retained in `formal/system/evidence/2026-10-25-ledger-header/`. Evidence is
valid only for its recorded source identities and hashes.
