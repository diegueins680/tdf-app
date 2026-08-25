# Creator badge type cutover

Status: candidate implementation and disposable-database rehearsal complete. No production write,
backup, deployment, migration run, or health-check result is claimed.

## Classification and model

The former `creator_badge.badge_type` values (`trendsetter`, `regular`, `og`) are a dynamic
business catalog. They are displayable, translatable, ordered, assignable, reportable, and
governed product data; a SQL `CHECK`, Persistent `Text`, and a leaderboard `[Text]` response were
not an acceptable authority.

`creator_badge_type` now owns stable UUIDs, internal code, Spanish/English names and descriptions,
manual order, publication state, slug, deprecation/replacement metadata, usage aggregate, and
optimistic version. `creator_badge.badge_type_id` is the only runtime relationship. The
leaderboard returns typed badge objects containing canonical UUID, code, bilingual names, award
time, and optional expiry instead of copied strings.

Bootstrap preserves exact legacy identity with deterministic rows:

- `50a00000-0000-4000-8000-000000000001`: `trendsetter`, Marcador de tendencia / Trendsetter.
- `50a00000-0000-4000-8000-000000000002`: `regular`, Miembro frecuente / Regular.
- `50a00000-0000-4000-8000-000000000003`: `og`, Miembro fundador / Founding member.

The central catalog service exposes this specialized table through the normal draft, review,
approval, publication, audit, pagination, search, cache, import/export, activation, replacement,
and merge infrastructure. Web and mobile route it to strict bilingual flat-catalog editors.
Mobile snapshot schema 9 batches `creator-badge-types`; schema 8 is accepted only as upgrade input
with cleared ETags until the new catalog has synchronized. There is no emergency badge list.

## Migration safety

The guarded scripts are:

- `tdf-hq/sql/2026-08-12_creator_badge_cutover_dry_run.sql`;
- `tdf-hq/sql/2026-08-12_creator_badge_cutover_apply.sql`;
- `tdf-hq/sql/2026-08-12_creator_badge_cutover_rollback.sql`.

Dry-run reports schema shape, exact values, normalized variants, target UUIDs, unresolved values,
and duplicate canonical assignments. Apply uses an advisory lock, bounded lock/statement timeout,
safety threshold, deterministic seeds, immutable per-row evidence, and migration mappings. It
stops on unknown values, conflicting existing UUIDs, duplicate canonical assignments, or excess
rows. It drops the copied text only after all UUIDs are non-null and verified, then enforces the FK,
active/published trigger, referenced-item identity/deactivation protection, and no-hard-delete
rule.

Rollback is permitted only when every current assignment is covered by unchanged evidence for the
selected run. It restores exact original text including legacy casing and reinstates the uniqueness
contract. The historical exact-value check is restored only when every preserved value satisfies
it; preserving a real legacy casing variant takes precedence over fabricating rollback data.
Catalog rows, evidence, mappings, and run metadata remain for audit.
Any new or changed post-cutover assignment blocks rollback and requires forward repair or the
verified backup path.

## Candidate verification

The complete forward/rerun/rollback/reapply rehearsal passed against disposable PostgreSQL 16 as
run `creator-badge-postgres-5a21b4ef-a96c-4694-a999-7754270b4b35` for candidate revision
`integration-creator-badge-v1-954442f8-7856-412a-8c28-0af526f69154`. The fixture contained two
legacy assignments, including the controlled casing variant `TRENDSETTER`. Dry-run found two
resolvable rows, one normalized variant, zero duplicates, and zero unresolved values. Rollback
restored both source strings exactly; reapply restored the UUID-only model. Four negative guards
rejected an unknown value, an identity conflict, deactivation of a referenced type, and hard
deletion.

The candidate also passed the full Haskell build, the focused leaderboard response contract, four
focused web tests, 23 focused mobile tests, both TypeScript typechecks, web lint with no new errors,
mobile lint, generated-client byte comparison, and the hardcoded-list audit. These results are
candidate evidence only and do not replace production backup, restored-copy rehearsal, preflight,
or post-deployment verification.

The notification-type registry and notification destination relationships are deliberately not
folded into this cutover. They are a separate governed operational model with multiple producers
and typed navigation targets, and require their own caller inventory and release gate.
