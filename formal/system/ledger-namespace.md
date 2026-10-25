# Ledger triggers resolve trusted relations

On financial source `59df1dd2a17952b906cf89d1c5502725d51239e1`, a synthetic
non-owner with ledger DML and the database's default TEMP privilege reproduced
two violations of `SYS-MONEY-003` on the complete 219-migration schema:

- A temporary `commerce_ledger_entry` supplied a balanced view while posting
  a real transaction whose entries summed to one.
- A temporary `commerce_ledger_transaction` supplied a draft parent while
  changing an entry belonging to a real posted transaction.

Both ordinary attempts without shadows were rejected. The actor was neither
superuser nor table owner and could not create objects in public. These are
local counterexamples; no production exploit or role audit was performed.

The additive `2026-10-24_ledger_guard_namespace.sql` migration locks both real
tables, refuses inconsistent posted/reversed history, and sets both trigger
functions' search path to `pg_catalog, public, pg_temp`. The existing function
bodies, physical-write fences, SECURITY INVOKER behavior and historical SQL are
preserved. PostgreSQL searches temporary tables implicitly first unless
`pg_temp` is explicitly placed later; see the official
[CREATE FUNCTION security guidance](https://www.postgresql.org/docs/17/sql-createfunction.html#SQL-CREATEFUNCTION-SECURITY).
Keep this guard through application rollback. Do not repair historical money
silently or grant runtime CREATE privileges on the trusted public schema.

`scripts/__tests__/ledger-posting-namespace.py --negative-controls` exercises
the installed guards using a synthetic ordinary role. It rejects unbalanced
and empty posting, posted entry INSERT/UPDATE/DELETE and caller-schema shadows,
and permits posting real balanced entries while shadows contain no matching
entries. Two controls remove one function's lookup setting at a time and must
reproduce the original failures. Each session rolls back roles, schemas, rows
and function changes; exact function definitions are compared afterward.
The canonical ledger runner invokes it after legacy-upgrade and MVCC coverage;
the legacy and snapshot replays restore the additive namespace successor too.
The full-schema fixture grants the synthetic actor the archive-table read needed
by the existing identity-reference trigger; that trigger remains enabled.
Explicit verification failures remain active under Python optimization.

`LedgerNamespace.tla` separately abstracts one non-empty currency to zero/nonzero
balance and an actor capable of supplying entry/parent shadows. The positive
configuration preserves `PostedBalanced`; each of two disabled lookup guards
must violate it. This finite model assumes trusted lookup semantics. It does
not establish PostgreSQL name-resolution refinement, arbitrary SQL/MVCC behavior,
caller retries, missing-history repair, a production privilege policy or deployed
correspondence. Table owners/superusers can still change DDL or disable guards.
Existing complete implementation/refinement and release holds remain open.

The startup schema gate independently checks both installed trigger functions for
`search_path=pg_catalog, public, pg_temp`, trigger return type and SECURITY INVOKER.
A matching migration receipt alone is insufficient: replaying a predecessor can
remove a function setting while retaining its receipt. The full-schema
`ledger-startup-namespace.mjs` regression runs the actual rendered startup verifier
with rollback-scoped removal, reordering, caller-path, signature and definer
mutations for each function, and permits an unrelated function timeout setting.
It compares exact definitions and migration receipts afterward. These controls
establish detection of the named configuration drift; they do not verify arbitrary
function bodies, trigger bindings or privileged-owner behavior.
