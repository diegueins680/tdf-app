# Social event type cutover

Status: candidate implementation and disposable PostgreSQL 17 rehearsal complete; production has
not been modified.

## Classification and model decision

Social event types are a dynamic business catalog. They are selectable, localized, ordered,
filterable, reportable, and govern both user-created events and provider imports. The former web
arrays, backend string allowlist, metadata field, and query-string codes were therefore not
technical constants.

`event_type` is the specialized authority. It owns a UUID, stable code, Spanish and English names
and descriptions, current slug, ordering, active/publication lifecycle, creator/editor/approver,
effective dates, replacement/deprecation, usage, publication revision, and optimistic version.
`social_event.event_type_id` is the canonical foreign key. `metadata.eventType` is migration
evidence only and is rejected on every new insert or update.

The idempotent bootstrap supplies these initial published codes without overwriting later
administrative edits:

- `party`, `concert`, `workshop`, `festival`, `recording-session`;
- `conference`, `rehearsal`, `livestream`, `showcase`, `meeting`;
- `wedding`, `corporate`, `retreat`, `photo-session`, `other`.

The one active `social-event/global` scoped default initially references `party`. Database and
startup checks require exactly one active, effective, published default, prevent competing
defaults, and prevent deactivation of the selected default.

## Contract and consumers

- Authenticated event create and update require `eventTypeId`; arbitrary strings, labels, codes,
  and slugs are not accepted. The backend validates catalog ownership, active/publication state,
  workflow ownership, effective dates, and deprecation before writing.
- Event list filtering uses `event_type_id`. It compares the foreign key directly and never
  decodes metadata or infers a relationship from a slug.
- Event responses return the canonical UUID. The strict DTO rejects the obsolete `eventType`
  field as unknown input.
- The collaborative event creator obtains the localized options and `social-event/global` default
  in a bounded public batch. It disables creation when policy cannot be loaded and preserves an
  invalid historical draft until the user selects a currently valid item.
- The authenticated events list loads protected items including inactive rows for historical
  presentation, but offers only active, non-deprecated, published rows for a new filter. Event
  detail resolves the display label by UUID and active locale.
- Mobile snapshot schema v6 batches `event-types`, validates UUID identity and exactly one active
  published `social-event/global` default, and clears a legacy v2/v3/v4 ETag during in-memory
  upgrade; it also carries the separately versioned social-event workflow. The event creator sends only a catalog UUID, exposes accessible 44-pixel radio targets,
  and disables submission while offline emergency data or a stale/deactivated choice cannot prove
  validity. It preserves the rest of the user's draft and offers explicit resynchronization; no
  bundled event-type list can become a second authority.
- Ticketmaster/Buen Plan discovery resolves its normalized event-type observation to exactly one
  active, effective, published UUID before any graph write. Code, Spanish name, English name, and
  current slug are deterministic aliases inside this boundary. Providers that supply no type use
  the explicit persisted `other` identity; an unknown supplied value fails the import instead of
  creating an opaque value. The importer stores the foreign key and no longer writes
  `metadata.eventType`.
- OpenAPI defines `eventTypeId` and `event_type_id` with UUID format. Web and mobile clients were
  regenerated from the same canonical document and are byte-identical at the current SHA-256
  `e5bdd157aee4a5b3b1dc4ccba4253986e0f257e36d4b7ef61552852708ce1d7f`.

Event workflow status remains a separate relationship and is now covered by the persisted
workflow-state cutover documented in `social-event-workflow-cutover.md`; a type UUID is never used
as a status identity.

## Migration and rollback

- `tdf-hq/sql/2026-08-11_social_event_type_cutover_dry_run.sql` runs read-only and reports schema
  readiness, every source value, candidate counts, selected UUIDs, all persisted items, and the
  scoped default.
- `tdf-hq/sql/2026-08-11_social_event_type_cutover_apply.sql` uses a transaction-scoped advisory
  lock, configurable safety threshold, deterministic mapping, immutable source evidence, normal
  catalog mapping evidence, a validated foreign key, indexed filtering, and a final canonical
  gate before commit.
- `tdf-hq/sql/2026-08-11_social_event_type_cutover_rollback.sql` restores the exact original UUID
  and metadata only when the current row still matches the recorded cutover target. It preserves
  the new column, foreign key, catalogs, mappings, evidence, and run history.

Resolution accepts only an existing canonical UUID or one unique normalized match against code,
Spanish name, English name, or current slug inside the active `event-types` catalog. The item must
also be active, effective, non-deprecated, and published by that catalog's workflow. Zero matches,
multiple matches, or a conflict between an existing UUID and legacy metadata abort the complete
transaction. No default is inferred for a historical row.

## Disposable PostgreSQL evidence

The rehearsal inserted two synthetic legacy rows: `" Fiesta "` and `"concert"`. Dry-run reported
two source rows, two deterministic matches, and zero unresolved, ambiguous, or conflicting rows.

Final identifiers and counts:

- run code: `social-event-type-final-20260811`;
- candidate revision: `local-candidate-v1`;
- run ID: `d9dbe3b3-3389-435e-9093-04f012b2b3b1`;
- scanned/mapped/rejected/ambiguous: `2/2/0/0`;
- source evidence rows: 2;
- catalog mapping rows: 2.

Apply removed both metadata keys and stored the two canonical UUIDs. A no-op rerun retained one
run, two source rows, two mappings, and zero invalid rows. Rollback restored null UUIDs and the
original metadata byte-for-byte; reapply returned both rows to canonical state.

Transactional negative checks rejected a missing ID, a copied metadata value, an unknown UUID, an
inactive event type, and deactivation of the active default. A zero-row safety threshold and an
unresolved `unknown-event-kind` value both aborted atomically without leaving a run or changing the
fixture. Disposable source events were removed after rehearsal; catalog, run, mapping, and source
evidence remain for verification.

Focused code verification compiled the backend executable and exercised strict social-event
parsing, handler authorization order, event filtering, discovery imports (including atomic
rejection of an unknown provider type), web API serialization, and collaborative-event payload
construction. The focused Haskell run passed 114 examples, the web slice passed 15/15 assertions,
and the focused mobile slice passed 72/72 assertions. The complete mobile suite then passed
232/232 tests in 44/44 suites with typecheck and zero-warning lint. The complete web suite passed
1,533/1,533 tests in 131/131 suites, its typecheck passed, and its full lint completed with zero
errors and 89 inherited warnings. The complete backend suite passed 2,245/2,245 examples after two
test fixtures were brought up to the already-canonical UTF-8 and locale/currency-reference
contracts. Focused reruns passed 2/2 service-offering examples, 12/12 username/session examples,
and 3/3 affected web fan-profile examples.

The final candidate executable started successfully against the disposable PostgreSQL 17 fixture.
`GET /health` returned HTTP 200 with database status `ok`; the Spanish public `event-types` batch
returned HTTP 200 with 15 items, the single `social-event/global` default, and `ETag:
"catalog-1"`; a matching conditional request returned HTTP 304. Post-smoke SQL checks found 15
active event types, exactly one active scoped default, zero invalid social-event references, and
the event-type integrity trigger installed. Repository-wide production and completion gates still
remain required before any rollout.

## Domo quote dependency

`DomoVenuePage.tsx` still has a six-entry structure combining a presentation label with base price,
per-guest price, minimum hours, and included guests. That structure is not merely another copy of
the global event-type catalog. It is a contextual, effective-dated venue quote policy.

Its correct successor is a specialized model such as `venue_event_quote_profile` referencing at
least `venue`, `event_type`, `service_offering`, and `currency_reference`, with integer-cent pricing
dimensions, included guests, minimum hours, lifecycle/review metadata, and effective dates.
Optional catering, production, setup, and transport charges require typed quote components or
service-offering relationships. The current `photo` key must map deliberately to the global
`photo-session` identity. Until that model and the booking relationship exist, substituting the
event-type list alone would leave authoritative prices in frontend code and could change quotes
without audit. The detector therefore continues to report this consumer as outstanding.

## Production boundary and sequence

The sanitized baseline did not capture production `social_event.metadata.eventType` distinct
values. That absence is not proof of zero rows or safe mappings. Production rollout remains
fail-closed until a current read-only dry-run follows a recoverable backup and reports every row.

1. Record the exact release SHA, deployed revisions, and backup identifier for `social_event`,
   `event_type`, scoped defaults, run/mapping tables, and audit evidence.
2. Deploy the additive schema and bootstrap, then review every dry-run row and candidate count.
3. Quiesce old social-event and discovery writers. Legacy clients sending strings are incompatible
   and must be upgraded or refused by the coordinated minimum-client-version gate.
4. Apply with a reviewed threshold, validate counts/FK/default/trigger state, and start the exact
   backend candidate before releasing the matching web and mobile contracts.
5. Verify create, update, list filtering, historical display, provider import, cache invalidation,
   and representative reports. Monitor invalid-ID responses and import failures.
6. If rollback is required, restore the compatible application first and run the guarded rollback
   while writers remain quiesced. Do not delete event types, aliases, mapping evidence, or audits.

No production backup, migration, deployment, event write, or external publication was performed
during this rehearsal.
