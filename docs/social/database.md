# Additive social authority and read models

Depends on the audit/model PR. This is a **draft, inactive account-only foundation**,
not a replacement for every legacy privacy or chat boundary.

## Apply and pause

Apply `tdf-hq/sql/2026-09-14_social_v2_foundation.sql`, then
`2026-09-14_social_v2_read_models.sql` with the application's migration owner.
They are not wired into production boot. Explicitly grant the app role EXECUTE on
only the reviewed API functions if migration owner and application role differ.
Never grant untrusted users direct SQL access: actor parameters are trusted handler
inputs and the functions are SECURITY INVOKER, not a replacement for authentication.
The database runtime gate starts false and reapplying preserves its value.
The backend process flag and UI flag also default off. None were activated remotely.

Pause with `2026-09-14_social_v2_pause.sql`. This keeps **all** post-migration writes,
including blocks, closed profiles and retry records. Reapplying migrations does not
reactivate the gate. Retain the new authorization boundary when rolling the app
back; routing to old endpoints that ignore these records is unsafe. There is no
data-dropping down migration. No destructive legacy edge cleanup/backfill.

## Canonical ownership and concurrency

Pair rows have ordered existing Party IDs, one primary key and independent intent,
follow, block, mute and dismissal fields for each direction. Creation time stays
immutable; revision advances under actor/credential/pair locks. Requests never
write another person's intent. Disconnect/block clear bilateral consent; unblock
cannot restore it. A unique actor/request-key result log suppresses duplicate
mutations; a mismatched key payload conflicts. Replays revalidate current eligibility
and return current state, so a replay cannot present an old connection as current.
Old expected revisions conflict. Rate limit is 60 accepted new commands per actor
per minute. Shared ordered actor/credential locks also fence credential disable.
Account token revocation and managed-entity context are **not yet refined**.

`social_v2_close` closes social participation, retains a tombstone and clears edges;
it is not account erasure, is not exposed in the HTTP pilot and does not implement
the app's entire export/deletion workflow. Do not backfill consent from PartyFollow:
its origin may have been a unilateral vCard or automatic club follow.

## Following publication and consistency

Existing `fan_club_post` remains the content owner; `social_v2_publication` owns only
the derived immutable publication position. A runtime-row lock assigns positions
in commit order in batches of at most 500. New/backdated source posts are published
at a new position. Editing content/dates does not reorder an existing publication.
Source deletion cascades to the projection. A rebuild must **not** renumber existing
positions; reconcile missing source IDs, then call `social_v2_publish_batch` until
it returns zero before a pilot. Do not lower the runtime counter. Later batches
may be called on first-page reads; an external worker is not required at this scale.

Descending keyset pages filter hidden posts, blocks, mutes, live accounts and the
existing FanFollow/officer club authority before LIMIT. Following an author alone
never grants club access. Source timestamps are returned separately from publication
time. Requests after an eligibility revocation see current denial; an overlapping
read may finish from its earlier SQL snapshot. Newly eligible older content requires
a fresh traversal. The cursor is a decimal bigint string, is not an access token,
and is reauthorized for each principal/page. Reply and reshare product behavior is
not added: replies are excluded, reshares remain unsupported.

## Discover baseline and known limits

Only explicitly discoverable accounts enter a daily rotating sample of at most 200.
Current live/block/mute/dismiss/follow/connection eligibility is checked before
returning anything. Public artist genre IDs and the viewer's own declared genre
IDs yield `shared_interests`; otherwise `public_profile`. Opt-out removes interest
scoring and actor-specific tie ordering. No popularity/transaction/private-network
score. The bounded sample reduces recall and can return fewer than a full page.
This is a testable baseline, not evidence of relevance or conversion improvement.
The first unbounded 10,000-candidate/100,000-edge synthetic run exceeded 5s; the
bounded version is measured separately. No graph infrastructure was added.

## Refinement evidence / incomplete criteria

TLC's action-labelled ConsentTraces graph generates 29 distinct PostgreSQL transition
assertions via `generate-model-cases.py`; source DOT and generated SQL are committed.
This covers Request/Withdraw/Block/Unblock only. Feed fixtures additionally cover
ordering ties, edits, late inserts, deletion, membership revocation and exclusions.
A real two-session test holds block's locks while acceptance waits.

Still required before activation: full application-schema migration/rollback;
authenticated HTTP and end-to-end tests; complete legacy/block/media/notification
coverage; private-follow requests; managed entities; session-token fencing; mobile
cutover; general abuse reporting; complete long relationship-list pagination;
account lifecycle integration; benchmark thresholds and post-release instrumentation.
SQL fixture success does not satisfy those criteria. New prototype behavior is
kept behind inactive gates until this work is finished.
