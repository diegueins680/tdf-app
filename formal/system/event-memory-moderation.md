# Source moderation for event memories

Authority: the event-community request of 25 October 2026. This successor is
separate from approved PR #608. It does not authorize production activation.

The source EventMoment remains the sole publication. A database source state
(`visible`, `hidden`, `removed`) governs every canonical read, including legacy
previews, reactions, comments, destination resolution and notification visibility.
Pausing interaction writes cannot restore a hidden source. Existing public assets
and external copies are not erased or made private by source moderation.

Reuse interaction_report, interaction_audit, interaction_request, the existing
session/write-budget boundary and existing commands endpoint. A null comment_id
identifies a report about an event-moment source, never a synthetic comment or
duplicate post. Original comment reports and their default inbox remain unchanged.

- Only an active authenticated reader can report a visible source by another
  author. One report per source/reporter; replay cannot duplicate reports/audits.
  A resolved report reopens only after a real content change. Content versions
  exclude hide/restore and no-op edits, and store no body hashes. Reviewers are
  told when an open report refers to an earlier version of the source.
- A current organizer can hide and restore hidden sources in their own event.
  Logistics membership, authorship and RSVP alone grant no enforcement authority.
- The existing platform moderation capability permits removal and restoration of
  removed sources only within the existing parent-event visibility boundary.
  A block cannot revoke scoped enforcement, but private-event access is retained.
- Source writes lock the actor/owner, parent event, source and interaction target,
  recheck current authority, and require both target version and source revision.
  A concurrent caption edit or moderation action must conflict before mutation.
- New commands preserve all existing commands through an exact captured helper.
  Their request keys share the canonical actor ledger; payload/key conflicts fail.
- The source review inbox is an opt-in view of the existing reports endpoint,
  scoped to one event and cursor, with bounded pages and no reporter identities.
  Ordinary comment-inbox callers retain the historical response. Authenticated
  report responses are private/no-store.
- An authorized reviewer can dismiss a report without hiding the publication.
  That decision requires the same current target/content revisions and audit.
- Author edits cannot publish through hidden-source restrictions. Existing source
  deletion retires its canonical target and keeps the existing audit policy.

Installation changes no publication state and sends no communications. Reapply
must preserve the captured authorities; an unexpected successor fails closed.
Rollback before any source report/action restores those exact authorities and
retains data/columns. After evidence exists rollback refuses; use a forward repair.

Verification must include actual PostgreSQL and authenticated HTTP, malformed and
cross-event requests, blocks/private parents, current-role revocation, caption and
target revision conflicts, retries, paused reads, source deletion, aliases, legacy
discussion/notification denial and concurrent competing moderation. A bounded
model or an actor without a session is not a substitute for the HTTP boundary.

Pilot fact: Diego confirmed in this conversation that event 141 took place and
finished. Its production lifecycle must only be corrected once completed public
events remain readable in the deployed system; consent stays a separate gate.


## Absent organizer and explicit authority

A NULL organizer is absence of authority. Normalize organizer/moderator flags to
false and reject permission/access predicates unless explicitly true, before
request replay and before source effects. Ordinary readers retain reporting,
but receive no review rows/reasons and cannot hide, restore, remove or resolve
source reports. Setting a former organizer to NULL revokes recorded replay.

Migration 216 is an immutable forward repair of migration 215. It checks exact
predecessor/repaired function bodies, language, volatility and invoker/config
attributes before replacing the same functions and retaining their OIDs. Unknown
successors are refused. The public command wrapper records the successor so the
215 reapply/rollback guard cannot silently restore nullable authorization. The
repair retains all content, reports, request keys, audit and captured authorities;
recovery must retain the repair or use another reviewed forward repair.

The actual-schema regression first observes an unauthorized synthetic hide on
the predecessor, then verifies denial without effects, false capability flags,
private review rows, legitimate scoped moderation and former-organizer replay
revocation. Its fixtures roll back; HTTP, CI and production are separate scopes.
