# Records thumbnail incident — 18 September 2026

## Verified production failure

The public page `https://tdfrecords.net/records#releases` redirects to
`https://www.tdfrecords.net/records#releases`. A real Chromium visit reproduced
both gray images. Its network log records HTTP 404 for both resource images,
with a decodable 120×90 YouTube placeholder body; Federico's image returns 200
and shows the performer in the purple-lit studio. This is not a demonstrated
CORS, CSP or resolution problem.

Backend `/version`: `e1a825bda26dbb16b1c732e551cc4880d5626943`, built
`2026-09-18T15:42:08Z`. The public `/records/feed` returns 33 recordings and five
curated sessions. PostgreSQL read-only inspection confirmed the same resource
IDs, canonical links and thumbnails. Migration `2026-09-06_records_youtube_catalog`
was applied on 7 September, with checksum
`fe7167bdbaac443a470ea4138abd9375b5ebc07d29f5a57ada75f7d7123f6f4f`.

| Recording | Persisted video | Provider result |
| --- | --- | --- |
| Llama Este Pez Pt 1 | `ooPsIHsikYU` | Watch page: removed by uploader; oEmbed 404; image 404 |
| Llama Este Pez Pt 2 | `Cb7VGZJ6apo` | Watch page: removed by uploader; oEmbed 404; image 404 |
| Federico Molinari | `f2BabxM1Pjc` | oEmbed 200, correct title and @tdf.records attribution; image 200, visually inspected |

Both removed images have SHA-256
`20e9aab22032d85684d7d916a1013f7c577a132a5b10ea3fd3578e8d0b28a711`.
All other 31 recording thumbnail requests returned 200. That audit checks
response/body identity, not a claim that every other video was watched.
The five session resources have no stored thumbnail and render provider iframes;
they were not counted as failed image URLs.

The channel page independently confirms stable identity
`UCx9Jpaw_XDrMtIdzWYlU51g` for `@tdf.records`. It now contains different uploads:

| Current public upload | Video ID | Duration | Publication timestamp |
| --- | --- | --- | --- |
| Llama Este Pez … Pt. 1 | `0hYDXQ5hWfo` | 897 seconds | 2026-09-09T15:15:24-07:00 |
| Llama Este Pez … Pt. 2 | `0W4KdgkQD5w` | 903 seconds (Data API) | 2026-09-16T13:23:23-07:00 |

Both current watch pages report the verified channel, playable status and real
thumbnail metadata. Their descriptions independently say the performance was
19 August 2026. Publication is not recording date. The removed entries have
stored durations 756 and 294 seconds: do not silently relabel the new edits as
the old provider identities or borrow their images. Their relationship remains
a review item; they can be imported as distinct public uploads by the approved
channel ingestion service.

## First correction

The additive availability migration retains provider identities and all
recording/contributor/collection links, records `removed_by_uploader` and the
verification time, and clears only the exact broken generated thumbnail URLs.
Original values remain in the existing `catalog_backfill_run` ledger. Replays
are no-ops; rollback restores only the exact version changed by this repair and
retains the additive columns for binary compatibility. Newer editorial edits
are protected by a version comparison.

Records cards, compact previews and FanHub recovery cards share actual `img`
load/error handling. An editorial URL is tried first, provider images must match
the resource ID, and only YouTube resources can use the two bounded YouTube
recovery sizes. A small decoded provider placeholder is rejected. Exhaustion
shows “Miniatura no disponible”; verified deletion shows “Video no disponible
en la fuente”. An image failure alone never declares the video deleted.

## Existing ingestion state — not completion evidence

Read-only production inspection found ten event source registry rows:
Ticketmaster and Buen Plan enabled; Meet2Go, Passline Ecuador, BuenPlan social,
Feel The Tickets, TicketShow, On Time Tickets, Output Concerts and Visit Quito
are disabled manual `web` sources. Preserve their research path. Buen Plan's
undocumented interface is not proof of permission to extend its automation.

The durable research pilot contains 20 candidates (8 draft, 12 review), none
materialized. Its pilot approval dated 17 August is a research-pilot approval;
this correction does not reinterpret it as permission for automatic publication.
Latest actual discovery runs are 240/241 at 12:00 UTC, 18 September: Buen Plan
30 seen/30 updated; Ticketmaster zero. Earlier runs 238/239 are at 06:00 UTC.
This proves the six-hour worker is active, not the requested daily schedule.
Only two subscribed cities were processed. Ecuador-wide discovery, the coherent
06:00 America/Guayaquil schedule, Sunday reconciliation, shared pilot allowance,
video source administration, hourly/weekly video jobs and full backfill remain
separate implementation work. No activation is claimed by this repair.

After explicit authorization, enabled YouTube Data API v3 in the existing Google
Cloud project `tdf-records-477016` and created the server credential named
`TDF Records video ingestion server`, restricted to that API. It is stored as
Fly secret `YOUTUBE_API_KEY` for `tdf-hq`. It was initially staged; the existing
deployment of revision `ab9bbacc9da845b6bfe70ac3fda2ace44f17c918` subsequently
delivered it. Fly reports Deployed and presence-only checks passed on both ORD
and LAX replicas. This does not enable scheduled ingestion.
No key value belongs in this repository or client bundles.

A read-only official API traversal of uploads playlist `UUx9Jpaw_XDrMtIdzWYlU51g`
completed one page: 39 listed identities, 39 metadata responses, 38 eligible public
recordings, one public upcoming broadcast excluded. Two eligible uploads (`0hYDXQ5hWfo` and
`0W4KdgkQD5w`) are absent from the canonical feed. The two removed identities
are absent from the accessible playlist. This is an inventory, not a completed
persisted backfill or evidence of scheduled execution. Data API duration for
Pt. 2 is `PT15M3S`; the watch-page player reported 902 seconds. The official
metadata takes precedence. The excluded upcoming item is not a completed recording and must not be
imported into that collection.

Provider references: [channels.list](https://developers.google.com/youtube/v3/docs/channels/list),
[playlistItems.list](https://developers.google.com/youtube/v3/docs/playlistItems/list),
[videos metadata](https://developers.google.com/youtube/v3/docs/videos),
[developer policies](https://developers.google.com/youtube/terms/developer-policies).

## Local evidence and rollout

Incident artifacts: `/private/tmp/tdf-records-evidence/production-before.png`,
`browser-before.json`, `all-images.json`, `db-inventory.txt`, `event-state.txt`,
provider watch/oEmbed responses and extracted metadata. Do not publish raw
provider HTML, which includes request tracking values.

Register the new migration with its introducing commit in the existing manifest;
use normal independent review, CI and the guarded backend release process.
Frontend deployment follows the existing main/Cloudflare workflow. Verify the
resulting API statuses and browser rendering after rollout. The production
catalog has not been modified by this investigation or local tests; only the
separately authorized provider credential has been configured. Implementation, successful
local tests, CI, merge, deployment and scheduled execution are separate gates.
