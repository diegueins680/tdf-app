# Artist profiles, public videos and campaign destinations

Authority: Diego authorized implementation on 2 November 2026, including TDF custody until verified claim, real session images, public viewing, authenticated explicit follows, hosted and official embedded sources, web and Android/iOS, and edits to authorized campaign links without changing approved publication times.

## Existing identity and catalog

Use the canonical artist Party and existing `/artists/{slugOrId}/public`, `/fans/me/follows/{artistId}` and verified directory claim/manager flows. A social-event artist ID is a separate namespace. Neither a draft claim nor account creation grants artist management. TDF administrative status does not grant access to somebody else's campaign. No follower is created as a side effect of viewing, signup, claim, video publication or navigation.

The public artist page renders `apFeaturedVideoUrl` above its biography and a library of published videos. Records recordings use the same player. Preserve the existing explicit follow/login return path, reconciliation after a lost response and current-session ownership checks. Anonymous viewing needs no login. Playback starts only after a click and the provider's playback controls; changing source or closing the player removes the previous player. Keep an original-source fallback. Unavailable, private, age-restricted, region-restricted, removed or non-embeddable content cannot be made playable by TDF.

## Library service contract

The existing campaign service owns metadata in an additive `artist_videos` table in its existing SQLite database. Its normal consistent snapshot covers this table. There is no new queue, separate storage subscription, billing activation or backup timer.

Management routes under `/campaign-platform`:

| Route | Contract |
| --- | --- |
| `GET /videos?artist_id=N` | Current artist authority plus existing campaign membership; returns only accessible campaigns. |
| `POST /videos` | `artist_id`, `campaign`, `title`, `description`, `poster`, `scope` (`own` or `tdf`), and exactly one `source_url` or `asset`. Creates a private draft; repeated identical source and metadata reuse the same identity. A conflicting duplicate returns 409. |
| `PUT /videos/{id}` | Same fields plus exact `version`; update increments the version, withdraws the old public version and clears approval. Stale writes return 409. |
| `POST /videos/{id}/publish` | Exact `version`, `rights_confirmed: true`, `technical_reviewed: true`. Current artist authority and campaign approval permission required. TDF scope additionally requires a current strict TDF administrator. Does not publish on any external social account. |
| `POST /videos/{id}/unpublish` | Exact `version`; same current permissions. Withdraws public metadata/media, preserves original/history. |
| `GET /public/videos?artist_id=N` | Anonymous, only exact published and rights-confirmed versions; excludes owner, campaign, original path and credentials. |
| `GET/HEAD /public/videos/{id}/media` | Anonymous byte-range access only to the finished asset bound to a currently published version. Unpublishing/editing denies new requests. Already downloaded bytes cannot be recalled. |

Authority comes from the current Core session. Own Party and strict TDF admin are recognized only after resolving the public canonical artist. A manager must match the canonical public directory profile ID and its current private `capabilities.edit` grant. Failure to resolve or read authority fails closed. A draft/private directory profile without a resolvable canonical public association needs its existing claim/visibility workflow completed; it does not gain permission by name matching.

Uploads reuse campaign reservations, 4 MiB resumable offsets and the existing physical disk/quota guard. Referenced originals cannot be deleted. Publish checks the existing finished file using bounded `ffprobe`, disallowing network protocols and unknown containers; MP4 H.264/AAC and WebM VP8/VP9/AV1 with Opus/Vorbis are accepted. Other codecs need a reviewed derivative through the existing render workflow; publication does not silently transcode or overwrite the original. A successful probe does not replace manual playback, synchrony and image review.

For external sources accept complete HTTPS URLs from YouTube, Vimeo, Instagram and TikTok; normalize their provider identity for deduplication. Never accept supplied iframe HTML, arbitrary frame origins, external server-side downloads or a shortened URL needing a redirect fetch. YouTube uses privacy-enhanced insertion with a valid referrer. Vimeo preserves an unlisted access hash without recording it in analytics. TikTok uses its current `player/v1` endpoint. Instagram embeds remain conditional on the provider's current public-content/insertion settings; no API or private-content bypass is supplied.

## Mobile

`/artistPublic?slugOrId=...` is a public route for canonical fan artist identity. Android/iOS show the same TDF profile/player inside `react-native-webview` 13.15.0 (the installed Expo 54 compatible version). `artistDetail` retains its social-event identity. Approved HTTPS `/a/` and `/artista/` deep links preserve bounded attribution query fields and cannot substitute the reserved identity field. Existing Social discovery links now open this canonical profile.

The WebView does not inject a native bearer token or copy cookies out of another browser. Following uses the page's normal TDF login and explicit confirmation. A browser fallback is always offered for identity providers that disallow embedded authentication or for failed playback. Native-session SSO is not claimed. Native WebView dependency changes need a new normal native build; an OTA JavaScript update alone is insufficient. Metro/type/unit results do not qualify physical-device playback, OAuth or release signing.

## Measurement

Existing consent/configuration-controlled analytics receive `tdf_video_player_opened` separately from actual file `tdf_video_play_started`, `tdf_video_watch_sample` and `tdf_video_ended`. Sampled watch time is an estimate of visible elapsed playback, bounded by both media and monotonic time; seeks/background suspension are excluded, and short intervals lost at abrupt navigation may be undercounted. Do not send media URLs, unlisted hashes, names or credentials. With no analytics key or applicable consent, the existing client is a no-op.

An iframe open/load is **not a playback**. External-provider play/progress SDK measurement remains unqualified and must not populate TDF watch-time or retention metrics. Existing authorized YouTube Analytics continues to report provider views/watch time; never sum it with TDF visits, player opens or file samples, and never attribute a follow to a promotional post solely because it happened later. Use exact successful follow evidence and available acquisition context, acknowledging reporting gaps.

## Roj(o) rollout and protected gates

The deduplicated canonical profile is Party 520, slug `rojo`, administered by TDF until verified claim. It uses the already approved full-session thumbnail (real performance image with the official TDF logo), confirmed members, recording date and explicit release context. It references the existing YouTube master; it does not upload another video. The Records recording already exists and is reused.

Deployment must pass the existing source/CI/review/admission and release controls. Keep existing campaign/source PRs; do not bypass protected review with a CMS/Caddy/manual deployment. The currently deployed player must be checked anonymously on mobile and desktop before migration: actual playback and audio, source fallback, follow intent through auth, canonical identity and visible TDF/YouTube subscription route.

Only then update authorized social captions/links, native scheduled entries and the existing Linktree session entry. Preserve every original timestamp, platform ID, approval and publication receipt; compare actual native schedules with server records. Edits create reviewed current versions, not new publications. Native Meta/YouTube schedules need their own native edit; the campaign panel does not synchronize them automatically. Immutable or expired pieces are recorded as uneditable; never delete/repost, re-upload, resend collaboration invitations or reset an uncertain provider outcome to obtain a new link. Bio editing still requires its existing specific authorization.

Preferred copy: **Ver la sesión y seguir a Roj(o) en TDF.** Destination after qualification: `https://www.tdfrecords.net/a/rojo`, with per-piece acquisition parameters. Keep the verified original YouTube URL as a secondary source and channel-subscription route. Until qualification, preserve the working campaign destinations and schedules. The private migration ledger contains the exact before/after proposals and current state; proposed edits are not reported as executed.

Sources: [YouTube iframe API](https://developers.google.com/youtube/iframe_api_reference), [YouTube insertion conditions](https://support.google.com/youtube/answer/171780), [Vimeo Player SDK](https://developer.vimeo.com/player/sdk), [TikTok Embed Player](https://developers.tiktok.com/docs/en/embed-player), [Expo WebView](https://docs.expo.dev/versions/latest/sdk/webview/). The installed Expo package's `bundledNativeModules.json` is the version-specific dependency authority; the latest documentation can point to a newer SDK.
