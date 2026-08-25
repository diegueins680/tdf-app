# Operational pipeline workflow cutover

Status: candidate implementation and disposable PostgreSQL 16 rehearsal complete. Production has
not been modified.

## Classification and authority

Recording, mixing, mastering, rehearsal, class, and event-production stages are dynamic business
workflows. Their labels, ordering, defaults, transitions, and service applicability are persisted
in `workflow_definition`, `workflow_state`, `workflow_default_state`, `workflow_transition`, and
`pipeline_workflow_binding`. `pipeline_card.service_offering_id` and
`pipeline_card.workflow_state_id` are the canonical relationships.

The former `ServiceKind`, `PipelineType`, `pipelineStages`, frontend `STAGES`, mobile
`PIPELINE_STAGES`, `PipelineKind`, `service_kind`, and `stage` values are not authorities. The
backend hardcoded stage module was removed, new writes clear and reject both copied columns, and
the fresh-install SQL makes them nullable migration evidence. New cards require a service-offering
UUID and may supply a state UUID; omission uses the one persisted active `initial` default.

Bootstrap persists six internal, non-public, non-sensitive workflows, 35 bilingual active states,
11 explicit service-offering bindings, six initial defaults, and 180 direct transitions. The
complete directed transition graph intentionally preserves the former Kanban ability to move a
card between any two distinct columns. A future workflow governance revision may narrow those
transitions without changing clients. Startup requires exactly this registry and rejects any card
whose service, workflow, or state relation is incomplete or inconsistent.

## Contracts and consumers

- Pipeline routes capture a workflow UUID, never a type slug. Create accepts
  `serviceOfferingId` and optional `workflowStateId`; patch accepts only `workflowStateId` for a
  move. Unknown fields, including legacy `type` and `stage`, are rejected.
- Responses return service/workflow/state UUIDs, stable read-only codes, both state translations,
  ordering, and card data. Codes and labels are presentation metadata, not write identities.
- The backend verifies that the offering is bound to the requested workflow, the state is active
  in that workflow, and a persisted direct transition exists. Database triggers enforce the same
  invariants and reject deactivation or hard deletion of referenced definitions.
- `GET /pipelines/snapshot` loads bindings, definitions, services, states, and cards with five
  bounded queries and returns one typed revisioned payload. Mobile therefore performs one request
  instead of one request per workflow. Web uses persisted definitions and stages for board tabs
  and columns; drag operations write UUIDs.
- The backend checks persisted `pipeline.read`, `pipeline.create`, `pipeline.update`, and
  `pipeline.delete` capabilities independently after verifying Scheduling module access. Artist,
  student, customer, and fan roles receive none by default. Admin, manager, and studio manager
  receive all four; execution roles receive only the read/write subset documented below.
- Workflow cache invalidation is transactional. State, transition, default, capability, service
  binding, and card changes increment the affected workflow revision; the snapshot revision is the
  monotonic sum across its six workflows.
- Web and mobile generated OpenAPI types are byte-identical at SHA-256
  `62d30a99cd9cfe4985791bf79e17c4d68d3c6598318057e32623de6385bbb361`.

## Offline mobile behavior

The mobile pipeline snapshot has schema version 1 and stores the server revision, synchronization
time, definitions, services, stages, and cards. Strict parsing rejects a card whose workflow,
service, or state is absent from the same snapshot. A valid network payload atomically replaces
the cache; a failed or invalid refresh retains last-known-good data. No sample or emergency cards
are invented. A move sends only `workflowStateId`, validates the response against the cached
definition, and updates the cached card after success.

If an offline draft later refers to an inactive or replaced value, the invalid UUID remains in the
draft as evidence but cannot be submitted until the user selects a valid published replacement.
The pipeline client never converts labels or codes into a write identity.

## Authorization matrix

| Persisted role | Read | Create | Update/move | Delete |
| --- | --- | --- | --- | --- |
| admin, manager, studio-manager | yes | yes | yes | yes |
| reception, live-sessions-producer, producer | yes | yes | yes | no |
| engineer, teacher | yes | no | yes | no |
| a-and-r | yes | no | no | no |
| artist, student, customer, fan and all unlisted roles | no | no | no | no |

The module grant alone is insufficient. The code recognizes only these four stable capability
identifiers; labels, role grants, and availability are persisted. Startup fails if any recognized
permission row is absent or misbound, or if an unknown active permission could grant access.

## Migration and rollback

- `2026-08-11_pipeline_workflow_cutover_dry_run.sql` is read-only and reports every source row,
  normalized stage, target service/state/workflow UUID, candidate count, and deterministic
  evidence.
- `2026-08-11_pipeline_workflow_cutover_apply.sql` uses an advisory lock, reviewed safety
  threshold, bounded `SKIP LOCKED` batches, immutable specialized source evidence, workflow
  mapping rows, and a final canonical integrity gate. A rerun scans only remaining noncanonical
  rows and does not duplicate evidence.
- `2026-08-11_pipeline_workflow_cutover_rollback.sql` restores exact source columns and UUIDs only
  while every card still equals its recorded cutover target. It refuses to overwrite later edits
  and retains catalogs, source evidence, mappings, and run history.

Service identity resolves from an already canonical offering or the reviewed legacy service-kind
mapping. Stage identity requires exactly one active state code in that offering's explicitly bound
workflow. Punctuation aliases are limited to reviewed deterministic forms: `make-up-needed`,
`pre-prod`, and `post-prod`. An unresolved stage, multiple candidates, or disagreement with an
existing state UUID aborts the transaction; the migration never guesses.

## Disposable PostgreSQL evidence

The repository integration inserted four controlled rows: mixing `Brief`, mixing `Prep`,
mastering `v1`, and mastering `Approved`. The final positive run was:

- run code: `pipeline-workflow-postgres-integration-2e4b345b-ff7b-4fbd-857b-264ee30c1444`;
- candidate revision: `integration-pipeline-v1-20260812`;
- run ID: `715b2678-d2cf-4ef7-8cd7-4d3d65d0afbb`;
- scanned/mapped/rejected/ambiguous: `4/4/0/0`;
- specialized source evidence and workflow mappings: `4/4`;
- batch size: 2.

Apply cleared both legacy columns and wrote four state UUIDs. A no-op rerun preserved the digest,
one run, and four evidence/mapping rows. Hard deletion of source evidence was rejected. Rollback
restored all original strings and UUIDs exactly; reapply returned all rows to canonical state.
The three negative cases—threshold zero, `provider-mystery-stage`, and existing `brief` UUID plus
legacy `approved`—all aborted atomically without changing the source or committing a run row.

The candidate startup first persisted the registry then intentionally exited non-zero while four
legacy cards remained. After dry-run/apply/rerun/rollback/reapply it passed every runtime registry
gate and returned HTTP 200 `{"db":"ok","status":"ok"}`. The candidate process was then stopped.

Focused verification passed the full executable and 138-module test-suite build, four Haskell
canonical-contract examples, three web adapter examples, four mobile snapshot/API examples,
generated-client regeneration, web and mobile typechecks, directed ESLint, and the PostgreSQL
positive and negative suites. A disposable HTTP smoke returned 200 for a role with
`pipeline.read` and 403 without it. The security registry contained exactly four known pipeline
permissions; default role grants matched the matrix, and a seed rerun left all six workflow
revisions unchanged.

The final reproducible PostgreSQL cutover run was
`pipeline-workflow-postgres-integration-105a1917-1427-4d7d-9d3d-4909678ca4e1`, with run ID
`a24a6bb5-8c9e-41ab-ad87-e32638270ffe`; it additionally proved that a card mutation increments
the affected workflow revision exactly once. The security-registry transaction and the
`pipeline_authorization_http.sh` 200/403 smoke both passed and removed or rolled back all fixtures.

## Production boundary and compatibility

The sanitized production inventory observed four cards: mixing `Brief`/`Prep` and mastering
`v1`/`Approved`. This local rehearsal used an earlier production-like copy with the same four
values, but it is not a current production dry-run.

1. Record exact deployed revisions and create a recoverable backup of `pipeline_card`, workflow,
   service, binding, run, mapping, and source-evidence tables.
2. Rehearse the exact candidate against a restored production copy, including interruption,
   resumption, rollback, and later-edit rollback refusal.
3. Quiesce old pipeline writers. Apply additive schema/bootstrap first; the candidate remains in
   its `starting` state and exits if legacy cards survive validation.
4. Review the read-only dry-run, apply the bounded backfill, verify all counts and constraints,
   then start the candidate and check snapshot, create, move, web, mobile, reports, and exports.
5. Require a compatible web/mobile revision before enabling writes. Do not accept legacy strings,
   slugs, or service-kind aliases after cutover.
6. For rollback, restore the compatible application first, keep writers quiesced, and run the
   guarded rollback. Never delete persisted workflows or evidence.

Production backup, current dry-run, deployment, monitoring, screenshots, and the wider remaining
catalog cutovers are still completion gates. No production write, deployment, commit, push, or PR
was performed in this slice.
