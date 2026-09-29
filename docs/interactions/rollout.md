# Interaction rollout and recovery

Production schema migration is additive and separate from activation. The live
baseline was 115 reviewed migrations on PostgreSQL 17.8; eight existing social
compatibility migrations and the canonical interaction migrations, including
additive review repairs, are registered in order. Existing social-v2 rollout gates remain disabled. No feature activation
is a schema-install side effect.

## Release gates

1. Preserve the Cloudflare Pages project settings, then temporarily pause only
   automatic production deployments for `tdf-app`; keep its current deployment
   serving traffic and preserve preview builds. Use the granular
   `source.config.production_deployments_enabled` setting, preserving
   `preview_deployment_setting` ([Cloudflare project API](https://developers.cloudflare.com/api/resources/pages/subresources/projects/)).
   Otherwise a protected main merge
   could publish the new web client before its backend/schema are ready.
   Merge through normal protected review/CI; build an immutable backend image.
   Apply only the exact merged manifest using the existing Hetzner release lane.
2. Capture a fresh encrypted/off-host production backup and rehearse its restore.
   Capture exact legacy row counts and parent/reaction validation in a read-only
   transaction; earlier table estimates are not conversion evidence.
3. Rehearse the complete manifest plus first activation on a restored isolated
   PostgreSQL 17 database. Verify cutover source/migrated counts agree, references
   and known IDs persist, counters match authoritative rows, and repeated
   activation is a no-op. Exercise pause and resume. Never send real notifications
   or provider requests from a rehearsal.
4. Stage schema with `RUN_MIGRATIONS=false` and install the new backend on every
   TDF writer before activation. Confirm source/target adapters, authentication,
   existing events/feed/records flows, credentials and webhook health. Do not
   touch Trader or the shared Fly database. Old Fly TDF writers stay fenced.
5. Deploy web and signed native releases after the backend is ready, then restore
   the saved automatic-production-deployment setting. Configure existing Cloudflare association
   functions with the verified public certificates in implementation.md. Check
   HTTPS association responses without redirects and installed-app deep links.
6. In the existing deployment lease, enable `interaction_runtime.enabled` in a
   guarded transaction. The activation trigger locks all legacy engagement
   sources, validates/converts nonempty engagement and installs permanent write
   fencing atomically. Inspect `interaction_legacy_cutover`: source/migrated counts
   must match. No engagement rows are silently discarded. Keep source tables as
   read-only mapping evidence; author/moderator erasure scrubs their private body.
7. Verify owned canary accounts on real web/native clients: four reactions,
   comment/reply/edit/delete, bounded expansion, mentions, notifications and exact
   link focus; ownership/moderation, blocking, private-event access and denied
   legacy writes. Remove canary discussion bodies through author deletion.
8. Capture exact deployment digest/commit/ledger, checks, and recovery evidence.

## Recovery

Before first activation, keep the gate disabled and repair forward. Installation
alone does not modify legacy engagement or stop its existing writers.

After activation, `UPDATE interaction_runtime SET enabled=false WHERE singleton`
is the non-destructive emergency pause. New writes and delivery stop; canonical
read privacy and permanent legacy fences remain. Preserve the new backend and
all canonical tables/mappings/receipts/counters/audits. Never downgrade to an old
backend that can read frozen source bodies without current blocks. Never reset
`activated_once`, remove the fences, truncate interactions, reopen the old Fly
TDF role, or restore a stale snapshot over accepted writes. Fix forward and
resume by enabling the gate; the conversion ledger makes resumption idempotent.

Disaster recovery restores the whole current database and compatible immutable
application together using the existing backup runbook. A backup from before
activation is not a safe routine rollback once new interactions have been accepted.
External notification delivery is limited to the existing in-app inbox; no email
or push distribution is newly activated by this migration.

## Review repair migration

`2026-09-29_interaction_review_repairs` is an additive forward migration; every
previously registered SQL file retains its original bytes. It introduces bounded
mention-recipient snapshots and a notification event watermark, replaces the
command/delivery/block/read functions, and indexes recent open reports. Reapplying
the migration does not refill delta-only edit events. Roll back application changes
only with compatible canonical readers; retain this schema and repair forward.

First activation refuses legacy event comments with missing or unresolved author
IDs. Reconcile those identities from trusted provenance before retrying; never
invent an account or accept a count-only conversion that hides existing text.
The failure is atomic and leaves legacy readers/writers available before activation.

New mention recipients are captured by stable ID in the same transaction as edits.
Delivery picks the highest enabled applicable reason (mention, reply, comment),
refreshes unread state once per new event, and ignores desired-reaction no-ops.
Unblocking never recreates either legacy follow direction. Existing authors can
edit visible comments after creation policy changes, subject to current access,
blocking, account and version checks. Moderator queues expose at most the latest
20 open report reasons per comment, labeled with the total; reporter identities
remain private. The legacy moment array retains every accessible moment with
bounded discussion previews fetched in one batch.

Directory-profile blocks are included in account block lists and revision checks.
Unblocking removes only rows owned by the actor's subject profiles; reverse blocks
remain effective. After activation, directory block changes share the account locks
and increment the canonical pair revision, and block creation severs both follow
stores. Reaction writes require an explicit selectable-choice row; historical
nonselectable reactions remain readable and removable.

`2026-09-29_interaction_publication_authority` disables the reserved artist_update
kind and makes its resolver unconditionally unavailable, including if a capability
flag is accidentally enabled. Social-sync ingestion has no publication/review
state and must remain private. Imported rows are preserved. Published artist
releases and ordinary club posts retain their existing publication paths.

`2026-09-29_interaction_reaction_withdrawal` permits an actor to withdraw their
existing reaction from a still-accessible target after losing domain write
eligibility. New selections remain forbidden. The existing canReact/selectable
contract exposes only withdrawal; no native binary or API schema change is needed.
Current visibility, blocking, suspension, session, idempotency and count guards remain.

`2026-09-29_interaction_moderation_block_boundary` separates scoped enforcement
from ordinary social blocking. On currently accessible targets, content managers
may hide/restore and current moderators may remove/resolve reports despite an
author block. Only authorized queues expose moderation bodies and actual state;
normal lists still exclude blocked authors. A scoped deep link yields redacted
context so an authorized moderator can open the queue without ordinary body or
identity disclosure. Owner queues include blocked visible comments and both
clients offer the existing hide operation there; owners gain no administrative
removal/report-decision powers. Current role/access/version checks and audits remain.

`2026-09-29_interaction_moderation_access` shares one scoped source adapter between
ordinary access and platform enforcement. A current strict moderator may ignore
source-owner/organizer social blocks for moderation, while publication, source
lifecycle and private-event logistics grants remain mandatory. Ordinary reads and
social writes continue to use the original block-aware wrapper. A moderation-only
summary lets current clients reach the queue without reaction/comment permissions
or interaction counts. Commands select that authority only for administrative
remove/restore/report resolution, rechecking under the existing locks. Active
reaction catalogs and their workflow identity also govern projected selectability;
historical choices remain readable and removable after catalog deactivation.

`2026-09-29_interaction_legacy_reply_aliases` preserves numeric legacy reply
identities without duplicating source posts. Nested legacy DTOs retain the requested
immediate parent. Alias reactions use independent canonical targets, inherit the
containing publication's current access/write grants, and validate the artist path.
They never change the root post's reactions. Source/target locks serialize reply
erasure with reactions; author deletion or moderator removal clears alias reactions
and retires their target, while owner hiding remains reversible. Historical
reaction slots and URLs are preserved; no engagement is reassigned to the root.

`2026-09-29_interaction_moderation_delivery` uses the strict moderation context
for queued moderation events when ordinary actor visibility is blocked. Owner
hiding retains ordinary authority, and social events never gain this fallback.
Recipient access, blocks, preferences, deduplication and read-state watermarks
remain enforced. A moderator who loses the necessary current scoped authority
cannot use the fallback to deliver an otherwise inaccessible event.

`2026-09-29_interaction_event_destinations` links private event and event-moment
discussions to the existing protected event page. Route selection checks current
public-view membership, independently of share eligibility: cancelled public
events retain their readable public page. Existing grants and visibility checks
remain authoritative; changing a destination never grants access.
