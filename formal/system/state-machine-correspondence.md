# Lifecycle state-machine correspondence: SYS-STATE-001

Declared lifecycle machines live in the domain models
(`docs/revenue-platform/formal-model.yaml`, `docs/music-directory/formal-model.yaml`,
`docs/operations-control-center/formal-model.yaml`). They are not restated here.
[state-machine-bindings.json](state-machine-bindings.json) binds each machine to the
table column that stores it and to the authority for that binding.

`scripts/check-state-machines.py --database-url URL` runs after CI applies the full
production migration manifest to PostgreSQL 17. For each bound machine it compares:

- the declared state set with the column's effective enumerated `CHECK` constraint;
- where a trigger encodes the transition relation as `OLD.col = 'a' AND NEW.col IN (...)`,
  the declared transitions with the trigger's.

Every difference must equal the machine's reviewed `deviation` exactly. New drift
fails, and so does a repaired drift whose deviation was not removed.
`--static` (formal workflow, every PR) rejects unbound declared machines and bindings
to undeclared ones; declared machines are discovered from every `docs/*/formal-model.yaml`
and requirement-defined state machine, independently of the bindings, and exclusions must
name a declared, unbound machine. `scripts/test-state-machines.py` runs twelve negative controls on
the migrated database, including a disposable table that drops a declared state.

## Result at audit baseline

With all 184 manifest migrations applied (local PG16 rehearsal, 2026-10-06), 15 of 17
bound machines match exactly. The Domo `quote` machine matches on states and SQL
transitions after AUTHORITY-047 corrected the model to ADR-0114: deposit refunds belong
to the `refund` machine, and accepted terms may expire. `distribution` and
`royalty_statement` differ from their schema-only tables, which no backend code uses;
AUTHORITY-048 records them as reviewed SPECIFICATION AMBIGUITY.

## Limits

Only state sets and SQL-trigger transitions are checked. Fourteen machines enforce
transitions in Haskell; their transition relations need separate evidence (for
example `TDF.Directory.Policy` for directory claims). Guards, actors and side effects
are not checked. A matching CHECK constraint does not prove every writer is legal.
`PAY-INVOICE-001` is abstract receipt presence and is explicitly unbound.
