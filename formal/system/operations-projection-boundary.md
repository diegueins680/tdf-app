# Operations projection, read and auxiliary command authority

Requirements: OPS-MANUAL-001, AUTH-OPS-READ-001, OPS-AUX-WRITE-001;
complements OPS-COMMAND-001, OPS-METRICS-001 and OPS-WORKER-001.

## Manual creation and replay

A manual creation requires current locked session and canonical role grants,
enabled organization, active branch and active membership. Its **write** role
must permit the resulting unassigned entity. Teacher and Engineer grants alone
cannot manufacture assigned work; ReadOnly never broadens another role's write
permission. A manual item is an operational projection, not an instruction to
execute a payment, claim a customer identity or modify a source booking.

The trimmed client `correlationKey` is an organization-scoped receipt key. It binds
actor, resolved branch and all typed semantic input, excluding diagnostic
`requestId` and `sourceClient`. A different binding returns409 without returning
another actor's result. The stored event uses `manual:<server-generated UUID>` as
its projection key; current source capture/backfill namespaces cannot produce it.
The generic database recording function is trusted internal infrastructure and
accepts arbitrary text: this separation is not a universal database permission
proof. Legacy manual receipts lacking the binding are not silently adopted.

Metadata must be an empty object. It is not a free-form channel for `terminal`,
identity or other projection-control fields. An optional amount is nonnegative
integer minor units and requires a three-letter uppercase currency. Such amounts
remain manual operational facts, not captured/reconciled provider evidence.
Responsibility, customer, service and monetary fields are stored with the item.
Existing source-reference constraints remain authoritative; invalid correlated
source references roll back, including deferred constraint failures.

One transaction writes the receipt and its outbox row, processes **only that
specific event**, requires exactly one successful projection, updates supported
fields and decodes the visible result before commit. A blocked predecessor,
projection failure, missing result or conflicting replay rolls back. No claimed
placeholder or failed outbox count is a successful creation. Exact authorized
replay returns the current associated work item and creates no additional
receipt, audit, stream event or version increment. Later authorized item changes
may therefore appear in the replay response. Network ambiguity requires replay
with the same actor/scope/payload; it does not authorize a new key.

## Projection ownership and compatibility

The additive scoped-projector migration retains the two-argument worker entrypoint
as a wrapper over the same three-argument implementation, whose third parameter
selects an optional event. It retains predecessor order and `SKIP LOCKED` behavior.
Unbound legacy manual events fail into the existing retry/dead-letter workflow;
operators must review their source/ownership before any migration to bound input.
Arbitrary SQL error text is excluded from stored worker diagnostics and projected
failure summaries; SQLSTATE and a fixed description remain observable.

A conflicting key cannot change branch or unrelated entity domain. The one
explicit compatibility transition is the existing trusted
`communication.whatsapp.received` / `whatsapp:<sender>` capture: in the same branch,
an uncorrelated thread may become a Party thread when a subsequent message has a
Party identity. A later unidentified message does not erase that association.
Manual events cannot enter this exception. Customer communication is not emitted
by these tests or by manual projection.

## Read authority

`TDF.Operations.Model.visibleEntityTypes` is the shared read policy used by detail,
list, stream filtering and metrics. Read grants compose by union. Admin may see
security; Manager, StudioManager and ReadOnly see other operational domains.
Accounting, Reception and Maintenance retain the domains in the existing RBAC
contract. Teacher sees assigned registration/booking/project/event work; Engineer
sees assigned booking/maintenance/project/event work. Producer/A&R/live-producer
roles have no documented domain grant and receive no incidental fallback access.
They can gain access through another explicit role. No new producer business
policy is inferred from the previous inconsistent list implementation.

ReadOnly does not supply mutation visibility. Combining it with Teacher permits
broad reads but leaves mutations limited to Teacher's assigned domains. Each
query has PostgreSQL snapshot semantics; multi-query detail responses are not
claimed to be linearizable with concurrent revocation. Failure records and counts
are manager-only and branch-scoped. Reads lacking a branch selector choose the
first active membership; they do not aggregate every branch in an organization.

## Auxiliary mutation lifecycles

Notes lock the current item, its assignment and actor/mentioned memberships, then
commit note, mentions, stream and audit together. There is no note idempotency
receipt: blindly retrying an ambiguous append can duplicate it. Invalid body
returns422; a missing active mentioned membership denies the command403.

Failure retry locks the current failure and current session/role/scope. Only a
retryable `open` or `dead_letter` row may become `retrying`; `retrying` and
`resolved` reject409. Exactly one contender increments attempts and records the
actual previous status. This is retry admission, not proof of provider dispatch,
delivery or downstream reconciliation. Audit/decode failure rolls back the retry.

Saved views remain owned by actor and name; explicit shared layouts are visible
within the organization. Filters and columns cannot confer source permissions.
Push subscription ownership remains actor plus token digest; ciphertext is stored
only when the existing encryption key is available. Both commands now retain
current session/role/scope locks and decode within their audit transaction. Their
request IDs remain diagnostic, so replacement/re-registration may add an audit.
Provider push delivery, token rotation across owners and deletion are not proven.

`OPERATIONS_WORKER_ENABLED` is a strict `true`/`false` startup switch, defaulting to
true to preserve existing behavior. Invalid values fail worker startup rather than
silently changing policy. Pausing it does not activate/deactivate the Operations
API or pause synchronous creation. HTTP transaction tests explicitly pause this
background loop; separate SQL controls exercise projection and the worker wrapper,
and Hspec exercises actual worker logging and configuration admission.

## Formal and empirical scope

`OperationsManualReceipt` bounds two actors, two branches, two payloads, one key
and at most two attempts. It assumes current admission and atomic transaction
primitives. It checks separation from a trusted source key, bound replay and
all-or-nothing receipt/evidence. Three negative controls reuse the source key,
remove replay binding or retain partial effects. It allows stuttering, assumes no
fairness and claims no liveness. UUID/hash collision resistance, database function
correctness, actual SQL scheduling, role combinations, WhatsApp progression and
provider effects are outside this model. It is not an implementation refinement
proof. SQL/HTTP tests exercise those specified concrete boundaries separately.

The HTTP suite uses synthetic actors and an owned disposable PostgreSQL database;
barriers observe actual lock waiters. Evidence must bind the source and executable
hashes. Prior-runtime reproductions show concrete disclosure/corruption, not fixed
runtime success. Full candidate validation and deployed correspondence remain
required. Production enablement is not changed by these repairs.
