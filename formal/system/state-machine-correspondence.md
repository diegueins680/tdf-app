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

State sets, SQL-trigger transitions and bound Haskell validators are checked
(sections below). Machines whose Haskell transition relation is still unbound
need separate evidence (for example `TDF.Directory.Policy` for directory claims). Guards, actors and side effects
are not checked. A matching CHECK constraint does not prove every writer is legal.
`PAY-INVOICE-001` is abstract receipt presence and is explicitly unbound.

## Backend validators (AUTHORITY-053)

Machines whose transitions are enforced by a Haskell validator may bind it as
`code: {file, validator, renderer}`. The checker reads the `allowedTransitions`
list following the validator definition, maps constructors to stored names
through the renderer's `case` expression and compares the pairs with the
declared transitions in both `--static` and `--database-url` modes. Identity
transitions accepted by the validators are not declared and are not compared.
A difference must equal `code.deviation` exactly. Bound: event ticket
fulfillment, Domo quote, service booking fulfillment and marketplace rental
fulfillment. Course enrollment is enforced in code but has no declared model;
distribution renders states with `show` and waits on AUTHORITY-048.

## Targets functions and clause renderers (AUTHORITY-080)

A validator may also be a targets function, bound as `code: {file, targets,
renderer, rendererFile?}`: one clause per source constructor returning the
literal list of admitted targets (`allowedTargets WorkNew = [WorkSeen, ...]`).
The renderer may be `case` form or one literal clause per constructor, in
`rendererFile` when it lives in another module. The checker requires a clause
for every constructor the renderer names (a wildcard or missing clause would
hide edges), exactly one clause per constructor, literal lists only, and reads
comment-stripped source. Bound this way: the operations work item
(`TDF.Operations.Model.allowedTargets`, rendered by
`TDF.Operations.Types.workStatusText`): its twenty implemented transitions equal
the declared `operations.control-center` machine, no deviation. Sixteen
database-free controls cover the new shape (edge removed or added, clause
missing, doubled, non-literal, continued on the next line, tab-separated or a
wildcard, unnamed constructor, edge kept in a comment, renderer clause repeated,
non-literal or removed, both binding kinds at once, targets not found, required
binding removed). The machine's edges also declare roles, guards and reason
requirements; SYS-STATE-001 compares transition pairs only, so those (enforced by
`validateTransition`) are not compared. As with CHECK constraints, the binding
proves the validator's relation, not that every writer passes through it: the
first view (`new` to `seen`) and assignment (`new`/`seen` to `assigned`) are
written directly in `TDF.Operations.Server`; both are declared edges.

## Membership relations and parse tables (AUTHORITY-081)

The directory machines are enforced by membership predicates in
`TDF.Directory.Policy` (`allowed<Machine>Transition from to = from == to ||
(from, to) `Set.member` transitions`, with `transitions = Set.fromList [ ... ]`
in the `where`), bound as `code: {file, relation, renderer, rendererFile?}`. The
checker requires the predicate to be a single clause whose decision is exactly
one of the two admitted shapes, its `where` to bind `transitions = Set.fromList`
once at the direct `where` declaration column as a single literal list of pairs, and the renderer to be a single
parse-table clause `name value = lookup value [("stored", Constructor), ...]`
(no stored name mapped twice) or `identity` when the pairs are string literals
(the claim relation). Identity transitions (`from == to`) are not declared and
not compared, as for the validators; the claim relation's `states` set, which
gates same-state requests, and extra parse-table entries beyond the declared
states are not compared either (the CHECK-constraint comparison makes an
undeclared stored name unreachable). Bound and required: profile, claim,
classified, application and invitation; on 2026-10-13 each relation equals its
declared machine (15, 11, 22, 17 and 12 transitions), no deviation. The claim
relation was already exhausted through HTTP under ID-CLAIM-REVIEW-001; the
static comparison is new for all five. Nineteen database-free controls cover
the shape (edge removed or added, unnamed constructor, set extended, built from
a decoy set, nested `let`/`where` shadows of `transitions`, or holding a named entry, predicate with an extra disjunct, deciding
on a primed set or keeping membership only in a helper, a specific clause before
the relation or before the parse table, edge kept in a comment, parse table
doubled, non-literal or missing an entry, identity renderer on constructor
pairs, relation not found, required binding removed). Actor permissions over these edges (which party may request
which status, in `TDF.Server.Directory`) are outside the compared relation.
