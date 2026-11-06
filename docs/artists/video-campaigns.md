# Artist video campaigns

`/mi-artista/campanas` is a responsive artist workspace for video links, organic
Instagram/TikTok copy, relative calendars and manual publication records. The
artist profile editor links to it through **Lanzar video**. Administrators can
select an existing artist; another identity requires its existing resource grant.
A current authorized manager can enter from that artist's public profile through
**Crear campaña**, using its canonical ID without impersonating the artist.

Drafts persist in PostgreSQL, rather than browser storage. An explicit UUID and
expected revision make a lost-save retry idempotent and reject stale overwrites.
The authorization guard rechecks and locks the real session, roles and modules
inside the same transaction as the campaign operation. A current authorized
directory manager can also manage the canonical artist, band or project through
active private-view and edit grants. Manager and profile rows stay locked through
the operation; names and pending claims grant nothing. Public featuring additionally
requires publication capability and a published, allowed public/unlisted directory
profile. Self artist and strict administrator retain their existing authority.
Preparing a campaign creates no account or permission grant.

Apply `2026-10-20_video_campaigns` through the registered production migration
lane before deploying the API. The migration creates two tables and an index;
it does not seed or activate campaigns, change permissions, or start workers.
Broad Persistent startup migration remains disabled. Roll back the application
while retaining draft data if the release must be reversed.

## Workflow

1. Open **Lanzar video** from the artist editor.
2. Paste a playable HTTPS video link, or use the existing upload service for a
   video through the existing recoverable storage widget after saving the draft. It keeps the source private, binds recovery to the campaign UUID, and uses current account quota and destination controls; it never raises quotas. A public playable destination still needs separate permission review.
3. Choose Instagram, TikTok or both and generate an editable 14-day relative
   calendar (D−2 through D+11). An optional proposed date is stored only as draft
   metadata. There is no scheduler attached to this field.
4. Add the prepared audiovisual files and copy. Expand advanced options to edit source cuts and manual publication records. Save to continue later.
5. Review the real video preview and download the JSON plan or copy per-piece
   text and attributed links. An imported plan must name the selected artist.
6. After operator review, publish manually on each platform and record the
   resulting URL. Exporting does not mark a piece as published.

**Ready** means files and copy have been supplied for review; it is not public
activation. This workspace has no external publishing worker, provider token
form, ad-spend control or background sender. Video editing/encoding is not a
server job in this release. Attach rendered clips or prepare them outside the
editor; their cut metadata is preserved in the plan. After review, a separate explicit **Publicar video en el perfil de TDF** action
updates only that artist's featured-video field, while locking the reviewed
campaign revision. It does not publish to external networks. This action was
not executed for the Liquid Paper Girl campaign.

The first launch plan is under `docs/campaigns/liquid-paper-girl-2026-10-20/`.
Its original and derived media URLs are deliberately empty until an authorized
operator hosts the files. Existing production artist ID 51 was reused, without
creating a duplicate profile or a fictitious user account.

## API

- `GET /artists/video-campaigns?artistId=51`: private list, maximum 100.
- `PUT /artists/video-campaigns/{uuid}?artistId=51`: save `{expectedRevision,
  document}`; creation uses revision zero. A 409 retains the local draft and
  requires reconciliation with the saved version.
- `GET /artists/video-campaigns/{uuid}/metrics?artistId=51`: unique registrations
  and follow creations attributed to this campaign.
- `POST /artists/video-campaigns/{uuid}/feature?artistId=51`: explicit public TDF
  featured-video update with `{expectedRevision}` and a locked review-ready draft.
- `POST /artists/video-campaigns/{uuid}/conversion?artistId=51`: authenticated
  viewer attribution `{kind,source,piece}`, independent of artist edit access.

The backend verifies a real account/follow creation within the last hour and
after campaign creation, a known campaign piece and review-ready status.
Uniqueness on campaign + viewer + kind prevents retries inflating conversions.
The first accepted piece receives attribution. These are historical creations,
not current follower totals. Request-supplied attribution is not proof of
causality, cross-device attribution or complete measurement of delayed returns.
Anonymous views use existing configured PostHog; provider reach/plays are not
invented and are not included in these backend counters.
Public HTML video files additionally emit observed play-start and ended events
through the existing configured analytics client, with allowlisted campaign
attribution and no media URLs. Editor previews and iframe load events do not
emit playback events. Starts can repeat after pause/resume and are not unique
listeners; these events are separate from server-verified account/follow counts.

Metrics also return an additive `byPiece` array with source, piece, registrations
and follows. The dashboard presents these verified TDF creations separately
from unavailable social reach. Counts are unique per campaign, account and kind;
they are first accepted request attribution, not causal evidence or a count of
unique anonymous visitors. Existing accounts do not become registration conversions.
Shared bio links cannot distinguish multiple posts without a reviewed per-piece
link change. Unknown provider playback remains unavailable, not zero.

Public TDF featuring now additionally requires `rightsConfirmed=true` and
`technicalReviewed=true` on the exact saved revision. Older drafts remain
readable and editable; absent checks fail closed only at publication. Editing
content or duplicating a plan clears these confirmations in the editor. Credits
can be recorded without inventing missing people or roles. Draft save, review
save, export and proposed dates create no public or external action.

Artist profile links preserve allowlisted campaign parameters inside the safe
login return route. Registration does not auto-follow an artist; the existing
explicit follow confirmation is retained.

## Provider prerequisites

Official documentation rechecked on 2026-11-05:

- TikTok Direct Post requires a registered app, approved and user-authorized
  `video.publish`, current creator-info retrieval and the applicable audit.
  Unaudited clients' posts remain private:
  https://developers.tiktok.com/docs/en/content-posting-api-get-started
- Meta's official Instagram collection documents its Facebook Login path:
  professional account linked to a Page, authorized tokens and the publication
  permissions (`instagram_basic`, `instagram_content_publish`,
  `pages_read_engagement`; Page discovery uses `pages_show_list`). Consumer
  accounts are excluded and Stories publication in this path requires a business
  account. Reels use a container, readiness check and a distinct publish request;
  readiness is not publication. The documented formats include MP4/MOV,
  H.264/HEVC, AAC48kHz and recommended9:16. Verify current account limits before
  an actual upload:
  https://www.postman.com/meta/instagram/documentation/6yqw8pt/instagram-api
  The direct Meta content-publishing pages returned429/unavailable here. No
  per-artist eligibility, token grants or Instagram Login path is inferred from
  the Facebook Login documentation.
- TikTok additionally requires current creator information, manual privacy
  selection and explicit review/consent. No default privacy or app approval is
  invented. Its developer guidelines restrict promotional overlays; review
  separate TikTok assets against the applicable manual/API route:
  https://developers.tiktok.com/docs/en/content-sharing-guidelines

No authorized per-artist provider connection was available in this environment.
Manual export works independently of provider approval; no automatic publisher
is presented as connected or functional.

The separate `/campaign-platform` service and PR667 already contain provider
authorization, encrypted tokens, resumable assets and explicit publication
controls for existing campaigns. This PostgreSQL launch workspace does not
silently copy those credentials, enroll an artist or dispatch through that
service. Unifying its provider execution with these drafts remains a dependency;
manual export is the usable route in this candidate. The WhatsApp acquisition
sequences in `docs/campaign-automation.md` and the TDF Sessions x Domo recruitment
panel have different purposes and receive no changes or enrollments here.
The current Domo panel stores its recruitment tracker in browser localStorage
and exports JSON; it is not a durable per-artist launch backend. The WhatsApp
document describes a real administrative sequence/worker boundary whose
activation requires consent and approved templates; it is not an Instagram or
TikTok video publisher. Neither is used as campaign execution authority here.

## Verification

`VideoCampaignsSpec` covers scope admission, draft validation, real SQLite
persistence, retry deduplication and stale revisions. PostgreSQL current-session
authorization uses the existing transaction guard; SQLite helper tests do not
prove its PostgreSQL locking behavior. Web tests cover save/resume, failed-save
recovery, anonymous exclusion, attribution continuity and validation. The real
production rollout and authenticated save require operator deployment access.

## Manager verification boundary

`node scripts/verify-video-campaign-access.mjs` enumerates a finite Boolean manager policy with 16,384 cases and 13 removed-guard controls. It also checks presence of the actual SQL identity, capability and lock clauses. This is not a SQL interpreter, authentication proof or complete interleaving refinement. `VideoCampaignsSpec` can execute the actual compiled helper against temporary tables inside a validated loopback `_test` database through `TDF_VIDEO_CAMPAIGN_MANAGER_TEST_DB`; it checks artist/actor isolation, revocation, private access, profile kind/state/moderation and separate publication capability. Missing external test configuration remains an explicit pending case.
