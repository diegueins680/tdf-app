# Event memories and internal service invitations

Authority: product owner request of 25 October 2026, Event Memories, Community
Engagement & Post-Event Activation. Implementation is incremental; publication,
independent review, integration, deployment and provider activation are separate.

## Publication boundary

The existing EventMoment is the only content authority. Image/video clients keep
an unchanged default feed; `?extended` explicitly includes audio and text. Text
requires a nonempty caption and no media URL. Media URLs must be absolute HTTPS;
caption limit remains 280. New clients omit absent optional fields, never null.
Author edits require the current stored update timestamp, current parent access,
a live session and source authorship. Deletion erases the source and uses existing
interaction deletion triggers; public asset erasure/third-party copies are not
promised. Existing reaction, discussion, mention and moderation authorities remain.

Files use the existing assets store and event lock. New media uploads require a
public parent, a live session, MIME/extension agreement, recognized signature and
byte bounds: raster 10 MiB, audio/video 25 MiB. Signatures are not full container
validation, transcoding or verified duration. Private event files are unavailable
until authenticated media delivery and lifecycle revocation are implemented.
Historical public URLs cannot be made private by changing a UI label.

## Invitation invariants

- A current, active social-event lifecycle must say completed, with an explicit
  end at least one hour ago and less than 30 days ago. An elapsed on-sale event,
  cancellation or missing/invalid end never qualifies.
- Campaigns and recipient in-app preferences start disabled. Only the current
  organizer can change its campaign; a logistics role is insufficient.
- Audited check-in on a paid valid ticket is verified attendance. Artists,
  organizers and logistics are professional participation. Accepted RSVP is
  intention; copy never claims attendance for this group. Ticket purchase without
  admission evidence grants no verified-attendance claim.
- The existing active-account, archive, block and parent-access authorities apply.
  Identity is exact Party identity; no name/email matching or consent transfer
  across archived aliases. Signals deduplicate with verified/professional/RSVP
  precedence. Account membership never grants marketing consent.
- Only the existing internal inbox is delivered. Email, WhatsApp and push are
  not sent or implicitly authorized. Operations push subscriptions are scoped to
  that domain and cannot be reused as event-memory consent.
- Unique (event, account, step), a dispatcher transaction lock and recipient locks
  enforce at most one invitation plus one reminder, at least seven days apart
  across all memory campaigns. The notification and delivery record commit
  together. Repeated ticks and replica contention cannot duplicate effects.
- A successful publication suppresses reminders; suppression survives deletion
  of that moment. Opt-out and live source eligibility are rechecked. Overlapping
  source edits follow PostgreSQL statement visibility; source locks and global
  account-deletion ordering are not universally proved by the bounded model.
- The same notification visibility function filters list, count and detail, also
  for older backend recovery images. Revocation does not recall external copies.
- Open attribution is recipient/event/UUID bound and is never an access grant.
  Organizer metrics are aggregates; delivery means committed inbox placement,
  not read receipt, email delivery or conversion/retention improvement.

## Verification limits

`formal/event-memories/check.py` explores a bounded abstract policy and detects
three deliberately broken controls. It is not SQL refinement or a distributed
systems proof. PostgreSQL fixtures exercise the real migrated functions, source
relations, notification privacy and rollback. HTTP, browser, device, full-suite,
CI and production results must each retain their actual execution identity.

The dispatcher also respects the existing global interaction delivery pause.

The feature requires the appended reviewed migration. Installation creates no
campaign or preference and sends nothing. Before any delivery, rollback restores
the recorded notification/view authorities and prior public-listable capability,
retains data, disables campaigns and removes the dispatch function. After delivery
or an authority successor it refuses; use a reviewed forward repair.


## Native publication confirmation and ownership

The native image/video client validates a persisted ID and supported media before
acknowledging publication. HTML, missing identity and a supplied different event
are errors, never synthetic remote IDs. Reactions/comments validate their source;
legacy responses may omit optional parent/event IDs. This is response-shape
validation, not a proof of storage commit or general request idempotency.

Reuse the native session version, usePartyOwnership and AuthQueryBoundary. A
publication captures event, actor, mode, query key and owner predicate in mutation
variables. Recheck before upload/create and after asynchronous results; an old
response cannot restore its caption over a new draft. A→B→A does not revive the
old session occurrence. Key event content by route event to retire the previous
editor. That pinned client does not include native audio/text; physical-device acceptance remains unverified.


## Canonical community entry

The canonical social event URL first waits for session bootstrap. Anonymous
visitors use the existing public Directory event page and its server-authorized
public projections. Verified signed-in visitors retain the existing Shell and
workspace. Explicit compose, moment or task intent requires authentication and
retains its complete internal return target. A public401/403/404 uses the same
login fallback for private and unavailable events; it reveals no private source.
Transient failures offer explicit retry. The publication CTA retains query
attribution and requests compose without granting consent or other authority.
This entry does not make the authenticated moment endpoint public.

Native image/video confirmation above describes the pinned7000 client. Separate
#172/#173/#174 add text/audio and author actions with their own immutable receipts;
they are not active merely because the root still pins7000. Physical-device and
store acceptance remain separately unverified.

Canonical event signup and discussion-login links retain their complete internal
event target, including query attribution and the selected anchor. The legacy
`/eventos/:id` alias keeps its existing sanitizer, which strips query and hash
from authentication return targets. These navigation
links neither perform an interaction nor grant notification consent.


## Registration and first publication verification

The disposable real API/browser lane must cover a fresh mobile-browser account:
public invitation, publish intent, visible account terms, signup, exact return to
the composer, actual image upload, server-acknowledged publication, reload by
author and another account, and persistence across a real API restart. The new
identity must not exist before UI signup and receives no fixture role grants.
Signup must request no marketing consent; memory invitation preferences must
remain disabled before and after publication. Authentication for this public
event must grant neither private-event reads nor organizer campaign access.
Synthetic browser cases never establish real recipient eligibility or consent.


CI invokes the same memory/browser lane using the existing RACI runner's local
Unix-socket Docker ownership, canonical Stack executable and Chromium install.
Its explicit `--event-memories` mode reuses the exact disposable container and
selects the existing backend-CI pgvector/PostgreSQL17 image because the complete
schema requires vector. RACI's ordinary mode remains PostgreSQL16. The HTTP lane
owns a separate freshly created test database, binds all226 migrations and drops
only that database. CI selects bundled Chromium; native local checks use Chrome.
Unknown arguments and remote Docker endpoints fail before resource allocation.
Artifacts include only synthetic result JSON and selected screenshots, excluding
credentials/configuration and raw backend logs. None of these tests send email,
push or real event invitations. Local direct-run success does not prove that the
CI adapter has executed; its hosted result must be recorded separately.

An explicitly supplied fixture password is forwarded to the database clients
in native/Docker mode outside GitHub Actions as well. A passwordless native
invocation receives no inherited ambient PostgreSQL password, while an explicit
empty fixture password fails before allocation. Regressions must exercise the
first client invocation and a real password-protected disposable PostgreSQL
instance; a successful trust-authentication run cannot establish this property.
