# Artist video campaigns

`/mi-artista/campanas` is a responsive artist workspace for video links, organic
Instagram/TikTok copy, relative calendars and manual publication records. The
artist profile editor links to it through **Lanzar video**. Administrators can
select an existing artist; ordinary artists cannot select another identity.

Drafts persist in PostgreSQL, rather than browser storage. An explicit UUID and
expected revision make a lost-save retry idempotent and reject stale overwrites.
The authorization guard rechecks and locks the real session, roles and modules
inside the same transaction as the campaign operation. There is no delegated
manager access in this version: only the artist's own account or a strict
administrator with the Admin module can manage a campaign.

Apply `2026-10-20_video_campaigns` through the registered production migration
lane before deploying the API. The migration creates two tables and an index;
it does not seed or activate campaigns, change permissions, or start workers.
Broad Persistent startup migration remains disabled. Roll back the application
while retaining draft data if the release must be reversed.

## Workflow

1. Open **Lanzar video** from the artist editor.
2. Paste a playable HTTPS video link, or use the existing upload service for a
   video within its current 50 MiB limit. The editor never raises that quota.
3. Choose Instagram, TikTok or both and generate an editable relative calendar.
4. Add the prepared audiovisual files, cuts and copy. Save to continue later.
5. Review the real video preview and download the JSON plan or copy per-piece
   text and attributed links. An imported plan must name the selected artist.
6. After operator review, publish manually on each platform and record the
   resulting URL. Exporting does not mark a piece as published.

**Ready** means files and copy have been supplied for review; it is not public
activation. This workspace has no external publishing worker, provider token
form, ad-spend control or background sender. Video editing/encoding is not a
server job in this release. Attach rendered clips or prepare them outside the
editor; their cut metadata is preserved in the plan. Updating the artist's
public featured-video field remains a separate existing profile action.

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

Artist profile links preserve allowlisted campaign parameters inside the safe
login return route. Registration does not auto-follow an artist; the existing
explicit follow confirmation is retained.

## Provider prerequisites

Official documentation checked on 2026-10-20:

- TikTok Direct Post requires a registered app, approved and user-authorized
  `video.publish`, current creator-info retrieval and the applicable audit.
  Unaudited clients' posts remain private:
  https://developers.tiktok.com/docs/en/content-posting-api-get-started
- Meta's platform overview describes account/product prerequisites:
  https://developers.facebook.com/documentation/instagram-platform
  The detailed content-publishing page was inaccessible during this review;
  no account-specific publishing eligibility is asserted.

No authorized per-artist provider connection was available in this environment.
Manual export works independently of provider approval; no automatic publisher
is presented as connected or functional.

## Verification

`VideoCampaignsSpec` covers scope admission, draft validation, real SQLite
persistence, retry deduplication and stale revisions. PostgreSQL current-session
authorization uses the existing transaction guard; SQLite helper tests do not
prove its PostgreSQL locking behavior. Web tests cover save/resume, failed-save
recovery, anonymous exclusion, attribution continuity and validation. The real
production rollout and authenticated save require operator deployment access.
