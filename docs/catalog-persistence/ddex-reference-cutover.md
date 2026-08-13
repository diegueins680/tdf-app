# DDEX governed-reference and lifecycle cutover

Status: candidate implementation and disposable-database rehearsal complete; production rollout
is not authorized until the repository-wide release gates pass.

## Classification and source snapshot

Standards, versions, message types, allowed-value vocabularies, codes, and external applicability
are governed reference data. Document, validation, import-plan, import-run, export, and job
lifecycle states and transitions are sensitive system registries. Partner eligibility is sensitive
integration configuration. Parser roots, namespaces, validation result/severity/layer
constructors, and render discriminants remain genuine technical constants only where exhaustive
decoding or execution requires them. Persisted validation registries own their stable identity and
presentation; constructors do not define selectable lifecycle options.

The bootstrap snapshot was reviewed on 2026-08-11 against DDEX Knowledge Base release pages. It
stores source URI/version/synchronization metadata for:

- ERN 4.3.2 — detection, validation, import, and export enabled for deployment `default`.
- RIN 2.1 — detectable family metadata, with validation/import/export disabled.
- MEAD 1.1 — detectable family metadata, with validation/import/export disabled.
- DSR architecture 1.4 — reference metadata only; XML detection is disabled.

Capability constraints require validation to imply detection and import/export to imply
validation. Startup compares the complete persisted support matrix with executable capability
identifiers and refuses unknown or missing runtime support.

## Canonical model and contracts

`ddex_document.standard_version_id`, `message_type_id`, and `workflow_state_id` are the canonical
relationships. The document lifecycle has 12 bilingual states, one `received` initial default,
and validated explicit transitions. Five additional sensitive workflow definitions own 23
bilingual operational states and their initial defaults and transitions. Validation runs, import
plans, import runs, exports, and jobs reference those states by UUID. Jobs reference one of four
persisted `ddex_job_operation` rows, and import changes reference one of three persisted
`ddex_import_operation` rows. The operation tables are read-only technical registries in the
generic catalog service; labels, ordering, activation, and revisions are database-authoritative.

`ddex_partner_standard_version` is the ordered many-to-many policy and
`ddex_export.standard_version_id` is its canonical standard relationship. Legacy text columns
remain nullable only for the guarded rollback window. Database triggers reject string-based
writes, wrong-workflow states, inactive operations, invalid transitions, referenced-state
deactivation, and hard deletion in normal operation.

Three validation-result rows, three severity rows, and four layer rows are specialized technical
registries. `ddex_validation_run.result_id`, `ddex_validation_issue.severity_id`/`layer_id`, and
`ddex_export.validation_result_id` are canonical UUID relationships. The old constructor/text
columns are nullable rollback evidence only. Runtime issue creation resolves parser constructors
through the active persisted registry and fails closed if a code is missing.

`GET /ddex/references?locale=es|en` returns one permission-protected revisioned snapshot containing
standards, message types, document states, five operational workflows with their states, both
operation registries, three validation results, three validation severities, and four validation
layers. Document filtering accepts `workflowStateId`; partner creation accepts only
`partnerAllowedStandardVersionIds`; export creation accepts only `exportStandardVersionId`.
Unknown JSON fields are rejected. The server requires persisted `catalog.read`, `catalog.import`,
or `catalog.export` capabilities and denies access when a persisted grant is absent.

The web inbox, document view, and partner editor use only generated OpenAPI DTOs and the typed
reference snapshot. Upload, preview, import-commit, conflict-resolution, raw-download, and export
controls were removed because the backend does not implement them. Partner administration exposes
creation only: there is deliberately no fake update action or hard-delete control while the
backend lacks the corresponding governed revision/deactivation workflow. The mobile application
has no DDEX runtime consumer, so only its generated contract changes; no emergency or offline DDEX
list was invented.

## Migration behavior

The dry-run, apply, and rollback scripts are:

- `tdf-hq/sql/2026-08-12_ddex_reference_cutover_dry_run.sql`
- `tdf-hq/sql/2026-08-12_ddex_reference_cutover_apply.sql`
- `tdf-hq/sql/2026-08-12_ddex_reference_cutover_rollback.sql`
- `tdf-hq/sql/2026-08-12_ddex_operational_cutover_dry_run.sql`
- `tdf-hq/sql/2026-08-12_ddex_operational_cutover_apply.sql`
- `tdf-hq/sql/2026-08-12_ddex_operational_cutover_rollback.sql`
- `tdf-hq/sql/2026-08-12_ddex_validation_reference_cutover_dry_run.sql`
- `tdf-hq/sql/2026-08-12_ddex_validation_reference_cutover_apply.sql`
- `tdf-hq/sql/2026-08-12_ddex_validation_reference_cutover_rollback.sql`

They normalize only reviewed legacy representations, require one active supported candidate,
verify partner/export compatibility, preserve every source value and ordinal, bound batches,
hold a transaction advisory lock, apply statement/lock timeouts, and abort on the configured
safety threshold. Duplicate legacy partner entries are preserved as separate evidence records
while converging to one junction relation. Rollback restores the exact ordered duplicate array,
legacy document/export values, and prior membership state; evidence is append-only.

The operational scripts normalize only explicitly reviewed historical constructor forms. They
require a unique active state/operation match, preserve the original strings and IDs in immutable
evidence, apply bounded `SKIP LOCKED` batches, refuse an unsafe mutation or rollback, and are safe
to resume or rerun with the same run/revision identity.

The validation-reference scripts use the same guarded protocol for run results, issue severities,
issue layers, and export validation results. They accept only reviewed constructor or wire forms,
require exactly one active persisted match, preserve prior IDs and source text, and withhold every
unresolved or conflicting value before any mutation.

## Verification evidence

The PostgreSQL integration fixture used one `FamilyERN`/`432`/`NewReleaseMessage`/
`StatusReceived` document, one `432` export, and a partner array `['4.3.2','4.3.2']`.

- Dry-run: document 1 mapped, export 1 mapped, partner values 2 mapped; zero unresolved,
  ambiguous, or conflicting values.
- Apply with batch size 1: four immutable source rows, four catalog mappings, and one workflow
  mapping; all copied values cleared.
- Rerun: identical canonical digest and evidence counts.
- Rollback: exact legacy values and duplicate order restored; new membership deactivated.
- Reapply: canonical relationships restored.
- Database negatives rejected legacy document strings, an invalid lifecycle transition,
  deactivation of a referenced state, and document hard deletion.
- Atomic negatives rejected a zero safety threshold and an unknown version without committing a
  run record or changing the source fixture.
- Fresh and repeat startup health checks returned `{"db":"ok","status":"ok"}` with four
  standards, one runtime message type, 12 document states, five operational workflows, 23
  operational states, four job operations, three import operations, and validated DDEX foreign
  keys.
- The operational fixture mapped six source records deterministically. Apply in batches of one
  recorded six immutable source rows, five workflow mappings, and two operation mappings; rerun
  preserved the same canonical digest and evidence counts; rollback restored every original
  constructor; and reapply restored the UUID relationships.
- Operational negatives rejected legacy plan/job/import-change strings, an invalid plan
  transition, referenced-state deactivation, and operation hard deletion.
- An authenticated snapshot smoke check returned all five workflows, both operation registries,
  three validation results, three validation severities, and four validation layers; the same
  endpoint returned `401` without credentials.
- A validation-reference fixture mapped run result, issue severity, issue layer, and export result.
  Batch-size-one apply recorded four immutable sources and four mappings; rerun, exact rollback,
  and reapply passed. Seven negatives rejected legacy writes, deactivation, hard deletion, and
  evidence deletion.
- Catalog smoke checks returned 3 results, 3 severities, and 4 layers with localized names. The
  DDEX snapshot included them at revision 147; web reports render Spanish or English database
  labels instead of Haskell constructor names.
- Haskell compilation passed; four JSON contract tests passed; web and mobile typechecks passed;
  both TypeScript clients were regenerated from the canonical OpenAPI contract.

## Rollout and rollback

Before production, take a backup, record its identifier, rerun the read-only production distinct
inventory, run the DDEX dry-run with the exact candidate SHA, require zero unresolved/ambiguous
values, and test the scripts on a production-like restore. Deploy the coordinated backend/web/
mobile contract before applying in bounded batches. Stop on unexpected counts, policy mismatch,
foreign-key failure, permission regression, or latency regression.

Rollback deploys the pre-cutover application and runs the rollback script only if its mutation
guard proves every target still equals the cutover result. It deliberately disables canonical
DDEX guards for the legacy application. No governed reference, evidence, audit, document, export,
or partner row is deleted.

## Known limits and required decisions

- Production DDEX distinct-value and usage counts are not present in the 2026-08-07 sanitized
  baseline and must be refreshed before any write.
- Upload storage, raw download, preview, conflict resolution, import commit, and export rendering
  remain unimplemented and return `501`; they are documented as such in OpenAPI.
- Validation result, severity, and layer constructors remain closed Haskell parser discriminants;
  persisted UUID records now own labels, ordering, availability, report references, and API
  metadata. Partner-specific validation policy still needs a strict persisted model before that
  feature can be called complete.
- No production backup, migration run ID, deployment revision, screenshots, draft PR, or commit is
  claimed. Those remain gated by the full repository program and the second coherent emergency
  administrator path.
