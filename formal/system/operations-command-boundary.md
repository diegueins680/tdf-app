# Operations command and approval authority

Requirements: OPS-COMMAND-001, OPS-APPROVAL-001, OPS-WORKER-001.
This contract refines the existing operations architecture and lifecycle; it does
not certify all Operations routes, source business actions, or production enablement.

## Work-item commands

`seen`, `transition`, `assignment` and `priority` authenticate the current token,
lock its row, derive roles exclusively from locked canonical assignments and role
definitions, then lock the work item. Newly inserted role assignments outside that
snapshot apply to a later request. Disabled grants cannot authorize a command.
Organization enablement, active branch, actor membership and any new assignee's
membership are checked under locks retained through commit. Visibility follows
`TDF.Operations.Model.canViewEntityType`; priority override additionally requires
Admin, Manager or StudioManager. The other commands require an operations mutating
role. A frontend control is never this authorization boundary.

Request/source identifiers must be nonempty; version must be positive. JSON unknown
fields are ignored for forward compatibility, including derived transition and
assignment commands. A command's `expectedVersion` must equal the locked item version. Exactly one row
must change, with version incremented once. The row, SLA timer changes, event,
stream notification record and audit record commit together, including successful
response decoding. A stale contender returns409 with no effects. Transaction
failure rolls back all those effects. A deadlock/serialization failure returns409;
other database failures return500 without raw SQL/exception payloads. Invalid
transition returns422, inactive session401, missing item404, denied scope/role403.
An opaque missing response is not promised for every unauthorized UUID.

`requestId` identifies diagnostics; it is not a success-replay receipt. A repeated
successful work-item request with its old version returns409. The client must
reload authoritative state; retrying with an increased version is a new command.
The existing transition table in `TDF.Operations.Model` remains the executable
lifecycle. The historical YAML's abbreviated transition list is descriptive, not
an exhaustive executable table. Assignments and mark-seen have their explicit
handler effects; they do not execute a payment or rewrite the source entity.

## Approval receipt and decision

Approval creation requires a current Admin, Manager or Accounting grant and active
organization/branch membership. Omitted branch selects the earliest eligible active
membership; it must still be active after locking. `idempotencyKey` is unique within
an organization and binds requester, resolved branch, linked work item, action,
target kind/id, optional minor-unit amount/currency, trimmed reason and expiry at
PostgreSQL timestamp precision. Key and reason must not be blank; amount when
present must be nonnegative and currency when present exactly three uppercase
ASCII letters. Currency exponent/provider arithmetic is outside this receipt.
A linked item must be visible in exactly the selected scope. New expired requests
are rejected422. Exact authorized replay returns the stored approval201 and adds
no audit; changed binding returns409 without revealing the retained response.
Source-client/request-id fields are diagnostics, not semantic key fields.

Decision requires an independently authorized actor in the same active scope,
`expectedDecision=pending`, stored decision pending, a different requester, and an
unexpired deadline evaluated with `clock_timestamp()` after lock waits. One
approved/rejected decision and its audit commit together. Terminal decisions cannot
reopen; terminal retries, self decisions and expiry return409 with no effects.
The receipt records approval; it does not execute money movement or deletion.
Cancellation and expiry materialization workers, downstream execution authority,
and provider outcomes are not established by this contract.

## Worker failure semantics

The worker iteration logs fixed error text or numeric counters. Arbitrary exception
messages, SQL payloads and customer values never enter its failure log. Synchronous
failure is reported; asynchronous cancellation propagates. A failing log sink does
not repeat the business tick. Malformed SQL count results fail rather than becoming
an apparent zero-work success. These rules do not establish outbound delivery,
exactly-once processing or production monitoring.

## Evidence and model boundary

`scripts/lib/operations_conformance.py`, invoked by the existing isolated full HTTP
runner, exercises actual Servant handlers against PostgreSQL with synthetic actors.
It witnesses lock waiters, checks eight-way version races, timer/audit rollback,
revocation, role-snapshot insertion, replay binding, separate actors, terminal
protection and expiry after waiting. `WorkerLoggingSpec` invokes the actual worker
iteration with property-generated exception text, log failures and cancellation.

`OperationsCommandFence.tla` abstracts two contenders with expected version0,
at most two commits and one revocation per actor; locks/SQL transactions are assumed
to realize atomic command admission. It checks no stale/revoked commit and atomic
effect counts. Mutants admit stale/revoked commits or leak effects on rejection.
`OperationsApproval.tla` bounds one key, two actors/branches/payloads, two subsequent
attempts and a two-valued clock. It checks bound replay, guarded decisions and one
audit per creation/decision. Mutants remove binding, duplicate replay audit, reopen
terminal state, ignore expiry, or permit self-approval. Both models are safety-only,
allow stuttering, assume no fairness and make no liveness guarantee. They abstract
row-lock scheduling, role composition, JSON/SQL decoding, actual money/provider
execution and distributed clock errors. No implementation refinement proof is claimed.

## Open boundaries

Manual creation, notes, saved views, push subscription, failure replay, read/search
projection, worker SQL internals and UI/offline command reconciliation still require
separate transactional and authorization review. Existing broad YAML invariants
are obligations, not proven guarantees. No production feature flag is activated by
this repair. Historical applied SQL remains unchanged; no migration is needed for
these handler transaction changes.
