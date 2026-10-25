# Event memories — implementation and verification record, 25 October 2026

Status: implementation and qualification in progress. This record does not claim
protected integration, production deployment, native-device verification or any
real invitation delivery.

## Observed pilot and existing architecture

Read-only canonical Hetzner/container/database binding and PostgreSQL inventory
role verified; aggregate evidence is private under
`.private/event-memories-20261025` in the original workspace. No private contacts,
tokens or credentials are included here.

- Event 141 is PATCH CULTURE Vol. 1, organized by Diego Saa. Its scheduled window
  was 24 October 14:00–21:00 America/Guayaquil (19:00Z–02:00Z). The end is elapsed;
  actual lifecycle remains **on_sale**, not completed. Scheduled elapsed time
  alone is not confirmation that an event took place and finished.
- Four linked artists: Emanuele Pilo-Pais, Kevin Montenegro, Llama Este Pez and
  Oracle Saint Rigel. Three accepted RSVP, one logistics member. Seven distinct
  related Party accounts across professional/RSVP signals, five currently active
  accounts; professional and RSVP sets overlap by one. No archived aliases found
  in this cohort. Zero tickets, zero audited check-ins, zero event moments.
- The authenticated production event, ticket tiers, moments, self RSVP and summary
  returned HTTP 200 in actual Chrome. The page displayed its composer. Public
  health/DB are healthy; API version is 4dac175d4. The public route returns a shell
  with HTTP 200; this is not by itself proof of authenticated usability.

React/Vite web, Expo mobile and Haskell/Servant/Persistent/PostgreSQL already have
EventMoment, RSVP, ticket admission audit, artist/Party links, logistics membership,
canonical interaction reactions/discussions/mentions/reports, scoped notification
visibility, social follow/block policy, and existing public event Open Graph edge
rendering. Reuse these rather than duplicating content, identity or messaging.

The campaign automation module is CRM/WhatsApp-specific; its consent and
Operations push subscriptions do not authorize event-memory mail/push. No email,
WhatsApp or push provider is invoked by this feature.

## Verified gaps and repairs

1. Mobile emitted explicit nulls rejected by the actual moment codec. It now omits
   absent fields; a wire-payload regression checks the serialized object.
2. The screen converted an authenticated upload failure to a local draft in the
   publish-success flow. It now retains the editor as a failure, never calls local
   storage in that path, and distinguishes explicit local drafts in feedback.
3. Completed events lost public readability. The additive migration retains
   public-listable/read/share capability for explicitly public completed events,
   without enabling RSVP or purchases. Private metadata, source suppression,
   accounts and blocks continue to govern access.
4. The server accepted image/video only, arbitrary moment URLs and lacked author
   edits/deletion. It now supports opt-in extended text/audio reads, HTTPS URLs,
   author/session/parent-scoped changes and optimistic edits. Existing clients
   retain the legacy feed. New public uploads validate MIME, extension, signature
   and size on the existing durable asset path; private parents are denied.
5. The web caption allowed 1,000 characters while the server accepted 280. Limits
   now agree; the composer retains failures and reuses successful uploads. New
   types are capability-gated for older deployed APIs.
6. Memory invitations previously had no event-specific eligibility/consent/replay
   authority. The new disabled-by-default flow reuses Notification with distinct
   consent, live recipient checks, atomic delivery records, deduplication, frequency
   limits and publication-based reminder suppression. Organizer metrics are
   aggregates; callers never receive recipient identities.
7. Public event pages now point directly to the memory composer, preserving intent
   through login/registration. Event pages expose canonical artist Party links
   already supplied by the backend; social Artist IDs are never treated as Party
   IDs. Following remains an explicit decision in existing artist profiles.

See `formal/system/event-memories.md` for exact invariants and limitations. SQL,
OpenAPI, both generated clients and scoped model/fixtures accompany the feature.

## Delivery and pilot campaign hold

No real invitations have been sent by this task. The deployed API does not contain
this migration or worker, the pilot lifecycle is still on_sale, and no new explicit
memory-invitation preferences have been collected. Related accounts are not a
consented recipient list. Delivered/failed/excluded counts cannot be inferred from
uninstalled infrastructure; task-initiated sends are exactly zero.

Protected main requires independent review and required checks. This task must
publish reviewable changes and satisfy those controls before rollout. No admin
bypass, provider activation, bulk message, data export or production fixture is
permitted by passing local tests.

## Remaining scope

Participant-only file delivery/visibility, per-moment private/public switching,
combined-media gallery posts, durable create-request replay after a lost response,
full media-container validation/transcoding/duration enforcement, source-moment
moderation/reporting beyond the existing discussion controls, public moment-specific
previews, native audio/text composition/playback, consented email/push delivery,
D7/D30 retention measurement and a real human pilot remain incomplete. Public file
URLs and external copies are not erased by deleting a source publication. These
are material gaps, not acceptance passes.

## Executions

Fresh exact results, commits, PRs and hosted/deployment state are appended after
qualification. Initial setup failures remain in their original logs: missing
local SSH-key alias (resolved using the existing Mac-specific key), empty stdin
in the borrowed read-only access helper (fixed in the private probe by binding its
fixed query into the remote program), missing shared dependency modules, first
uncached build interruption, ambiguous Haskell key types, and two invalid test
setups/rollback attempts. No product assertion was waived to obtain a pass.
