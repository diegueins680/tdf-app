# Fan Club content-reaction cutover

Status: candidate implementation and disposable PostgreSQL rehearsal complete. Production writes,
deployment, and migration remain blocked by the repository-wide release gates, the current
production dry-run, a verified backup, and a second coherent emergency-administrator path.

## Classification and canonical model

Fan Club post and memory reactions are a dynamic business catalog, distinct from event-moment
reactions. The specialized `content_reaction_type` table owns stable UUIDs, internal codes,
Spanish/English names and descriptions, visual symbols, search metadata, manual order, lifecycle,
slug, deprecation/replacement metadata, usage aggregate, and optimistic version. Shared catalog
infrastructure supplies drafts, review, approval, publication, auditing, cache revisions, import,
and permission-aware administration.

The former polymorphic `content_reaction(target_type,target_id,reactor_party_id,reaction)` table had
neither a target foreign key nor a reaction-type foreign key. It is replaced by two strict
relationships:

- `fan_club_post_reaction(post_id, reactor_party_id, reaction_type_id)`;
- `fan_club_memory_reaction(memory_id, reactor_party_id, reaction_type_id)`.

Both target columns and the actor column are foreign keys. `reaction_type_id` references the
content-reaction catalog. New writes cannot copy a code, label, symbol, target type, target slug, or
generic target ID. Triggers require an active, published, non-deprecated catalog item; referenced
items cannot change identity, be deactivated, or be hard-deleted before governed replacement.
Transactional toggles maintain the catalog's aggregate usage count, and startup reconciles that
aggregate from the two relationship tables.

Bootstrap seeds the exact five values previously embedded in the web surface:

- `50900000-0000-4000-8000-000000000001`: `fire`, `Fuego` / `Fire`, `🔥`.
- `50900000-0000-4000-8000-000000000002`: `heart`, `Me encanta` / `Love`, `❤️`.
- `50900000-0000-4000-8000-000000000003`: `clap`, `Aplauso` / `Applause`, `👏`.
- `50900000-0000-4000-8000-000000000004`: `mic_drop`, `Mic drop` / `Mic drop`, `🎤`.
- `50900000-0000-4000-8000-000000000005`: `skull`, `Me muero` / `I'm dead`, `💀`.

The separate `reaction-types` event-moment catalog remains narrower and cannot acquire Fan Club
values merely because symbols overlap.

## Contracts, clients, and administration

The post and memory reaction endpoints accept only `crrReactionTypeId`, a canonical UUID. Strict
JSON decoding rejects the former `crrReaction` string and every unknown field. The backend verifies
the typed target belongs to the requested club and validates catalog selection before toggling the
relationship. Responses contain an ordered `rsItems` collection with UUID, code, both persisted
names, symbol, and count, plus the aggregate total and the viewer's selected UUID. Feed,
leaderboard, spotlight, and discovery queries use the typed post table; memory summaries use the
typed memory table.

OpenAPI defines both reaction endpoints and strict discriminated request/response schemas. The web
and mobile generated clients were regenerated from the same contract and are byte-identical. The
web `ReactionBar` renders only server-provided options and submits their UUIDs. The central web
catalog index routes `content_reaction_type` to a bilingual typed editor. Mobile reuses the same
strict editor and snapshot validator.

Mobile snapshot schema 8 batches `content-reaction-types` with the prior boot and domain catalogs.
Schema 7 remains accepted only as last-known-good upgrade input: its ETags are cleared and the new
catalog must synchronize before catalog-dependent writes can be enabled. There is no bundled Fan
Club reaction list. If the network fails, mobile preserves the last-known-good snapshot; without a
valid page it must not invent selectable reaction values.

## Migration and rollback

The guarded scripts are:

- `tdf-hq/sql/2026-08-12_content_reaction_cutover_dry_run.sql`;
- `tdf-hq/sql/2026-08-12_content_reaction_cutover_apply.sql`;
- `tdf-hq/sql/2026-08-12_content_reaction_cutover_rollback.sql`.

Dry-run is read-only. It reports schema shape, target existence, exact normalized mappings, and
unsupported target kinds. Apply uses a transaction-scoped advisory lock, lock/statement timeouts,
a caller-set safety threshold, deterministic catalog seeds, stable canonical row UUIDs, unique
target/actor constraints, and immutable source/mapping evidence. It aborts atomically on an
unknown reaction, missing target, unsupported target kind, threshold excess, or conflict with an
existing canonical relationship. In particular, legacy `release` targets are withheld rather than
guessed into a Fan Club model.

The legacy table is renamed to `catalog_content_reaction_legacy_source` and made immutable; it is
not deleted. Rerunning apply reads that preserved source and performs deterministic no-op upserts.
Rollback is allowed only if every canonical row is covered by the selected run's evidence and no
row has drifted. It restores the exact legacy table name and drops the new typed junctions while
retaining catalog entities, migration mappings, run metadata, and immutable evidence for audit.
If post-cutover writes exist, rollback stops and requires the documented backup/forward-repair path.

## Disposable PostgreSQL evidence

The PostgreSQL 17 fixture used one post value (`FIRE`) and one memory value (`mic_drop`).

- Dry-run mapped 2/2 values with valid typed targets and no unsupported or unresolved row.
- First apply produced one post and one memory FK relationship plus two mappings and two evidence
  rows.
- Same-run apply rerun retained an identical digest and created no duplicates.
- Rollback restored the exact original table, target kinds, values, casing, and relationships.
- Reapply restored the two canonical UUID relationships.
- Negative guards rejected an unknown UUID, deactivation or hard deletion of a referenced reaction
  type, and deletion of cutover evidence.
- Haskell fast build and focused strict-request tests passed. Web typecheck and 7 focused tests
  passed. Mobile lint, typecheck, and 21 focused snapshot/administration tests passed.
- Web/mobile OpenAPI outputs compare byte-for-byte equal.

This is disposable-database evidence only. No current production distinct-value result, backup ID,
production run ID, deployment revision, or health-check result is claimed. Production must first
run dry-run against the deployed schema, resolve any `release` or unknown target rows, rehearse on a
verified restore, satisfy the security readiness gate, and use the exact candidate revision.
