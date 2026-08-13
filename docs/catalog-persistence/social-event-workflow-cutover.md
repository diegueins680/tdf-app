# Social event workflow-state cutover

Status: candidate implementation and disposable PostgreSQL 17 rehearsal complete; production has
not been modified.

## Classification and authority

Social-event lifecycle states are a dynamic business workflow catalog. They are selectable,
localized, filterable, reportable, ordered, and determine allowed transitions. Their assignments
to executable behavior boundaries are a security/system registry. Neither the former
`metadata.eventStatus` strings nor frontend unions are authoritative.

`workflow_definition`, `workflow_state`, `workflow_transition`, `workflow_default_state`, and
`workflow_state_capability` are authoritative. `social_event.workflow_state_id` is the canonical
foreign key. The initial bootstrap persists these bilingual states:

- `planning`, `announced`, `on_sale`, `live`, and `postponed`;
- `unavailable`, `out_of_scope`, `completed`, and `cancelled`.

It also persists one `initial` default, 43 allowed transitions, and the `public-listable` and
`ticket-purchase` capability assignments. Additional states may be administered without changing
the executable. Code retains only provider/parser outputs it can emit and the two capabilities it
can execute. Startup requires every code-recognized parser state to exist, rejects unknown enabled
capabilities, and therefore denies unknown behavior by default.

Workflow/state codes are immutable identities; names, translations, descriptions, ordering,
activation, defaults, transitions, capability assignment, and cache revisions remain persisted.
Database constraints validate same-workflow relationships, effective windows, one active initial
default, non-self transitions, permission foreign keys, active references, and public/sensitive
separation. Referenced states and workflow definitions cannot be deactivated or hard-deleted.

## Contracts and consumers

- Event create omits `eventWorkflowStateId` to use the persisted initial default, or supplies the
  UUID of that initial state. It cannot choose a later lifecycle state at creation time.
- Event update accepts only a canonical state UUID and checks a current persisted direct
  transition. Transitions requiring a permission, review, or distinct approver cannot execute
  through this direct endpoint.
- List filtering compares `workflow_state_id` directly. Event DTOs return the state UUID, stable
  code, both localized names, and derived behavior booleans. They never accept a copied state
  code, label, or `eventStatus` field as a write relationship.
- Public listing and ticket purchase are decided from persisted capabilities. Hiding controls in a
  client is not authorization.
- Ticketmaster/Buen Plan imports normalize provider status only at their parser boundary, resolve
  exactly one active state, store its UUID, and retain the provider's raw `source_status` as
  observation evidence. They do not write `metadata.eventStatus`.
- Web event filters load `GET /catalogs/workflows/social-event-lifecycle/states?locale=...`, not the
  administrative endpoint. Spanish/English labels come from the response.
- OpenAPI exposes a strict `WorkflowStates` envelope with typed states, defaults, capabilities,
  transitions, revision, and conditional ETag behavior. Web and mobile generated type clients are
  byte-identical at SHA-256
  `e5bdd157aee4a5b3b1dc4ccba4253986e0f257e36d4b7ef61552852708ce1d7f`.

Public access requires an active `public_read` workflow that is not sensitive. A public request
for `sensitive-publication` or internal `catalog-publication` returns 404. Administrative workflow
reads require `catalog.read`; an unscoped administrative list excludes sensitive workflows, and a
specific sensitive workflow additionally requires `security.read`. Backend capability checks are
authoritative.

The endpoint revision changes when a definition, state, transition, default, or capability
changes. The representative response uses six bounded queries (locale, definition, states,
defaults, capabilities, and transitions), independent of the number of states, so it introduces no
N+1 behavior.

## Offline mobile behavior

Catalog snapshot schema v6 stores workflow snapshots separately from item catalogs, including a
workflow ETag. Refresh batches the normal catalogs and the public social-event workflow in
parallel, supports partial 304 responses, and publishes a new local snapshot only after strict
validation succeeds.

Validation requires UUID identities, one workflow identity, unique state IDs/codes, exactly one
`initial` default, unique transition targets, valid target IDs, and valid effective timestamps.
Legacy v5 snapshots upgrade in memory with empty workflows and cleared ETags; synchronization must
fill the missing workflow before workflow-dependent behavior is enabled. Emergency data contains
no invented workflow states. A failed refresh preserves the last-known-good snapshot.

## Migration and rollback

- `tdf-hq/sql/2026-08-11_social_event_workflow_cutover_dry_run.sql` is read-only. It strictly casts
  metadata to JSONB, enumerates every legacy value and UUID, reports normalization evidence and
  candidate counts, and describes the persisted workflow.
- `tdf-hq/sql/2026-08-11_social_event_workflow_cutover_apply.sql` uses a transaction-scoped
  advisory lock, reviewed safety threshold, bounded `SKIP LOCKED` batches, deterministic upserts,
  a specialized immutable source table, `workflow_migration_mapping`, a validated foreign key,
  indexed filtering, and a final canonical gate.
- `tdf-hq/sql/2026-08-11_social_event_workflow_cutover_rollback.sql` restores the exact previous
  UUID and metadata only while each row still equals its recorded cutover target. It refuses to
  overwrite a later edit and retains catalogs, mappings, source evidence, and run history.

An absent or blank historical status maps to `planning` because that reproduces the legacy API's
documented read/default behavior. Reviewed aliases map `canceled` to `cancelled` and `onsale` or
`on-sale` to `on_sale`. Other values must match one active normalized state code. Zero matches,
multiple matches, or conflict between an existing UUID and metadata abort the whole transaction;
the migration does not guess.

## Disposable PostgreSQL evidence

The positive rehearsal inserted four controlled rows: absent status, `" canceled "`,
`" On-Sale "`, and `"unavailable"`. The final run was:

- run code: `social-event-workflow-postgres-integration-607b4a2a-90f7-41da-a5be-0a97048f837e`;
- candidate revision: `integration-workflow-v1-20260812`;
- run ID: `dc506806-e636-4a1a-b3c3-a95f2e15c47b`;
- scanned/mapped/rejected/ambiguous: `4/4/0/0`;
- specialized source evidence and workflow mappings: `4/4`;
- batch size: 2.

Apply removed all three legacy metadata keys and wrote four UUIDs. A no-op rerun retained exactly
one run and four rows in each evidence table. Hard deletion of source evidence was rejected.
Rollback restored every original UUID and metadata value exactly; reapply returned all four rows
to canonical state. Synthetic event rows were then removed while immutable run/mapping evidence
was retained in the disposable database.

The executable PostgreSQL invariant test rejected an undeclared transition, reopening a terminal
state, copied metadata status, a state from another workflow, code mutation, referenced-state
deactivation, and a public-sensitive workflow. A label edit increased the cache revision from 61
to 62 inside a transaction that was rolled back. A separate script ran the real apply migration
with (1) threshold zero, (2) `provider-mystery-status`, and (3) an existing `planning` UUID plus
legacy `announced`; all three aborted atomically without a run row or source mutation.

Focused verification rebuilt the backend and passed 132/132 Haskell social examples after two
SQLite fixtures were updated for the new column. The web catalog API test passed 8/8 and web
typecheck passed. Mobile workflow/API/snapshot tests passed 6/6 and mobile typecheck passed. The
hardcoded-list audit now covers 844 files and 548 candidates with no unreviewed or stale decision.

The candidate HTTP smoke returned 200 with nine bilingual states and ETag
`"workflow-social-event-lifecycle-61"`; the matching conditional request returned 304. Sensitive
and internal workflow requests both returned 404. No candidate server remains running.

## Production boundary and compatibility

The sanitized production inventory did not capture distinct `metadata.eventStatus` values. That
is missing evidence, not proof of an empty migration. Production remains fail-closed until a fresh
read-only dry-run follows a recoverable backup and every row is reviewed.

1. Record exact deployed revisions and back up `social_event`, workflow tables, run/mapping tables,
   and source evidence.
2. Rehearse the exact candidate against a restored production copy, including interruption,
   resumption, rollback, and later-edit rollback refusal.
3. Deploy additive schema and bootstrap, quiesce old event/import writers, and review the dry-run
   against an explicit threshold.
4. Apply in bounded batches, then verify counts, FK/trigger state, representative event create,
   transition, list/filter, import, ticket purchase, web labels, and mobile synchronization.
5. Reject old clients that write copied status strings through an explicit minimum-client-version
   gate. Do not preserve a legacy write path.
6. For rollback, restore the compatible application first and run the guarded rollback while
   writers remain quiesced. Never delete workflow rows or evidence.

The specialized workflow state editor, production backup/dry-run, deployment, screenshots, and
release monitoring remain completion gates for the wider repository mission. No production write,
deployment, backup, external publication, commit, push, or pull request was performed in this
slice.
