# Event-moment reaction catalog cutover

Status: candidate implementation and disposable-database rehearsal complete; production rollout
remains blocked by the repository-wide release gates and a current production dry-run.

## Classification and canonical model

Reaction choices are a dynamic business catalog. The specialized `reaction_type` table owns the
stable UUID, code, Spanish/English names and descriptions, visual symbol, manual order, lifecycle,
slug, deprecation/replacement metadata, usage aggregate, and optimistic version. The shared catalog
workflow owns drafts, review, approval, publication, audit history, imports, and cache revision.

`event_moment_reaction.reaction_type_id` is the only runtime relationship. Its legacy `reaction`
column is nullable rollback evidence and is rejected whenever a new row is inserted or updated.
An event moment no longer stores or relates through a label, code, emoji, or slug. The primary key
is a stable UUID and a unique constraint permits only one actor/reaction-type pair per moment.
Referenced reaction types cannot change code/catalog identity or be deactivated before replacement.

Bootstrap adds only the three values required to map the previous mobile moment surface:

- `50800000-0000-4000-8000-000000000001`: `fire`, `Fuego` / `Fire`, `🔥`.
- `50800000-0000-4000-8000-000000000002`: `love`, `Me encanta` / `Love`, `❤️`.
- `50800000-0000-4000-8000-000000000003`: `applause`, `Aplauso` / `Applause`, `👏`.

An existing row with the same unique code keeps its established UUID; the migration records that
resolved target. Existing published reaction types such as `like`, `celebrate`, or `insightful`
are preserved rather than silently deleted. Fan Club reactions deliberately use their own
`content_reaction_type` catalog and typed post/memory junctions; the follow-up cutover is documented
in `content-reaction-cutover.md` and does not silently widen this event-moment domain.

## Contracts and administration

`POST /social-events/events/{eventId}/moments/{momentId}/reactions` accepts only
`emrrReactionTypeId`, a canonical UUID. Unknown JSON fields, code/label writes, missing UUIDs,
inactive rows, unpublished rows, wrong-catalog rows, and invalid workflow ownership are rejected.
The response supplies UUID, code, symbol, and both persisted names as presentation metadata.
Moment list/detail loading batches all referenced types and fails closed on a missing relationship.

The generic catalog envelope gained optional `displaySymbol`, but only the strict
`reaction_type` adapter accepts it and requires one visible value of at most 16 characters.
The web route `/configuracion/catalogos/reacciones` and mobile catalog editor both support
bilingual draft creation/editing plus submit, approve, and reject. The central persisted catalog
index discovers this editor from `entityKind = reaction_type`; a client route never grants backend
authority.

Mobile snapshot schema 7 adds `reaction-types` to the batched, revisioned last-known-good cache.
The event and onboarding surfaces derive UUID, symbol, localized label, and ordering from that
snapshot. Offline reactions are keyed by reaction-type UUID, not by code. Legacy local code-keyed
reaction maps are withheld during sanitization rather than promoted to a false identity. No
reaction emergency list is bundled: without a valid synchronized page the draft/feed remains
readable, reaction controls explain that synchronization is required, and no alternate source of
truth is invented.

## Migration, mapping, and rollback

The guarded scripts are:

- `tdf-hq/sql/2026-08-12_event_moment_reaction_cutover_dry_run.sql`
- `tdf-hq/sql/2026-08-12_event_moment_reaction_cutover_apply.sql`
- `tdf-hq/sql/2026-08-12_event_moment_reaction_cutover_rollback.sql`

Dry-run is read-only and reports both pre-cutover and already-canonical shapes. It includes the
deterministic rows that apply would seed, so a new `fire` or `applause` target is not incorrectly
reported as unresolved. Apply holds an advisory transaction lock, uses statement/lock timeouts,
enforces a configurable row threshold, creates deterministic source-row UUIDs, requires exactly
one active published target, rejects conflicting existing UUIDs and duplicate target identities,
and records original values, prior IDs, target IDs, and evidence before clearing copied strings.

Two reviewed aliases are deterministic migration evidence only: `heart` resolves to `love`, and
`clap` resolves to `applause`. No runtime parser or compatibility writer accepts them. Any other
unresolved or ambiguous value aborts the whole transaction for review.

Rollback restores exact original text and prior UUID values only when every current row still
matches its cutover target and no post-cutover row lacks source evidence. It restores the legacy
composite primary key and removes the canonical write triggers without deleting catalog rows,
aliases, mappings, audit data, or immutable source evidence. A backup restore is required if
post-cutover writes make those safety predicates false.

## Disposable PostgreSQL evidence

The integration fixture used three legacy values: exact `fire`, case variant `HEART`, and reviewed
alias `clap`.

- Dry-run: 3/3 uniquely mapped; zero unresolved or ambiguous values.
- First apply: three source records and three catalog mappings; all copied strings cleared.
- Rerun: identical UUID digest and evidence counts; no duplicate records.
- Rollback: exact original casing/values and composite identity restored.
- Reapply: canonical UUID relationships restored.
- Negative guards rejected a legacy string write, an unknown UUID, deactivation of a referenced
  reaction type, and deletion of cutover evidence.
- Backend fast build and the focused strict request-contract example passed; web/mobile typechecks,
  generated-client regeneration, 3 focused web tests, and 24 focused mobile tests also passed.

The sanitized 2026-08-07 production inventory estimated zero event-moment reaction rows, but it did
not capture a current distinct-value report. Before production, rerun the read-only script with the
exact release SHA, require zero unresolved/ambiguous/conflicting identities, rehearse on a restored
copy, take and verify a backup, and only then execute the coordinated backend/web/mobile cutover.
No production backup, write, migration run, deployment, or health result is claimed here.
