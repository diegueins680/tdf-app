# Operations read policy (AUTH-OPS-READ-001)

Slice of 2026-10-12 on main `7d4375550`. One-line Haskell fix (the approval handler's linked-item check now strips
ReadOnly like the other write sites), a harness case, the model and its
evidence.

## What the code does (read 2026-10-12)

`visibleEntityTypes roles assigned` (`tdf-hq/src/TDF/Operations/Model.hs`)
unions a per-role grant list: Admin `["*", "security_incident"]`; Manager,
StudioManager and ReadOnly `["*"]`; Accounting, Reception and Maintenance
fixed lists; Teacher and Engineer their lists only when `assigned`; every
other role nothing. `canViewEntityType` accepts a type that is in the union,
or any type but `security_incident` when `"*"` is. Detail reads call that
predicate with the item's assignment. List, metrics and stream reads share
`scopeFilterSql` with `scopeFilterParams`: `(wildcard AND entity_type <>
'security_incident') OR entity_type in the unassigned list OR (assignee =
actor AND entity_type in the assigned list)`, where the wildcard flag and the
lists come from `visibleEntityTypes` with `assigned` false and true
respectively; the stream applies it through an `EXISTS` on the work item and
additionally honours `visible_to_party_id`. Writes call
`canViewEntityType` on the roles without ReadOnly at three sites (locked
work-item commands, manual work items, the mutation gate). The fourth, the
approval handler's check of a linked work item, did not strip ReadOnly
(found by the independent review of this slice): Accounting plus ReadOnly
could link an approval request to a work item that Accounting alone cannot
see, the requirement's forbidden "ReadOnly plus narrow writer becomes broad
writer". This slice strips ReadOnly there too (`tdf-hq/src/TDF/Operations/Server.hs`,
`createApprovalHandler`) and adds the Accounting-plus-ReadOnly case to the
conformance harness (`scripts/lib/operations_conformance.py`): Accounting
alone gets 404, with ReadOnly it reads the item (200) and still gets 404 on
the link. The metrics aggregate shares the scope filter for its work-item
counts; its `integrationFailures` count is gated by a separate manager-only
boolean, outside the policy and not modelled. By reading, the two evaluators
agree and the other forbidden transitions (unknown-role fallback,
non-admin security disclosure, hidden domains counted or streamed) are not
reachable; the model below checks that exhaustively over the finite policy.

## Model

`formal/event-operations/OperationsReadPolicy.tla`: one state. The grant
table is copied from `Model.hs` (ten roles, `Other` standing for every
unlisted one) and both evaluators are written out: `HsVisible`
(`canViewEntityType`) and `SqlVisible` (`scopeFilterSql`). Invariants
quantify over every subset of the roles, both assignment flags and fifteen
entity types (the fourteen granted ones, `security_incident` and an
unlisted `other_domain`): `ReadersAgree` (Haskell and SQL give the same
answer, so detail, list, metrics and stream agree), `NoSecurityDisclosure`
(only an Admin sees security incidents, by either evaluator),
`ReadOnlyNoWriteGain` (adding ReadOnly to any role set changes no write
visibility; with the guard on this holds by construction, so it records
the rule each write site must apply rather than checking the table), `SpecialistsAssigned` (Teacher, Engineer and unlisted roles see
nothing unassigned), `UnknownNothing`. Five controls remove one guard each:
`WildcardSecurity`, `SqlDrift` (the SQL wildcard branch without the security
exclusion: the two evaluators disagree), `Unassigned`, `ReadOnlyWrites`,
`UnknownGrant`. TLC reports a constant-level failure ("The invariant of X is
equal to FALSE") for these, and the runner expects that string. This is an
exhaustive check of the copied table, not of the Haskell or SQL text: a
change to either must be mirrored in the module, and `ModelSpec` and the
conformance harness remain the execution evidence. Branch membership and
the enabled-scope precondition are not modelled (they are the
`resolveScope` admission, outside the policy). Decision in AUTHORITY-073.

## Evidence

| Check | Where | Result (2026-10-12) |
| --- | --- | --- |
| Model and five controls | `scripts/verify-event-operations-formal.sh` | PASS locally; controls fail on their named invariant |
| Accounting cannot link an approval to a hidden item, with or without ReadOnly | `scripts/lib/operations_conformance.py` via the booking harness, CI | CI evidence on this branch (the fix and the case are compiled and run there; no local server build for this slice) |
| `ModelSpec` (security never exposed to non-admins, specialists limited to assigned domains, grants compose without list precedence) | `tdf-hq/test/TDF/Operations/ModelSpec.hs`, registered in `Spec.hs` on main | CI evidence; the local test binary of 2026-10-12 predates its registration and does not contain it |
| List/detail/stream/metrics conformance | `scripts/lib/operations_conformance.py` via the booking harness, CI | CI evidence; not re-run locally for this slice |
| Specification gates | inventory, traceability, state machines, new surfaces | PASS |

## Left open

- Producer, A&R and live-producer domain policy stays undeclared and denies
  (as the requirement says); the model's `Other` role encodes that.
- The grant table lives in two places (Haskell and the module); nothing
  checks them against each other mechanically.
- Per-request snapshots: list, detail and stream read in separate
  transactions; no linearizable multi-query read is claimed.
