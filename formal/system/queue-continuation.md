# Queue continuation from similar artists (MEDIA-MUSIC-CONTINUATION-001)

Status: second of the three changes that reuse the artist similarity
(COMMUNITY-ARTIST-SIMILARITY-001, [artist-similarity.md](artist-similarity.md)). On 2026-11-24 the feature owner approved the web player target, discovery-only artist recommendations, the 12-artist request limit, the five-track continuation limit and the default-off rollout tested at `2931f1306`. The build flag is `VITE_PLAYER_SIMILAR_CONTINUATION_ENABLED`.

## Why this target

The task names `TDF.ServerRadio` for "radio sequencing". That module is a catalogue of
internet radio stations: a `radio_stream` has a URL, a country and a genre, and no
artist. Artist similarity cannot order it. The place where the platform decides which
recording plays next is the web player queue (`tdf-hq-ui/src/player/queue.ts`), so the
sequencing is specified there. No backend route, response shape, OpenAPI contract or
Mobile pin changes.

## Inputs

- The queue of the single web player (MEDIA-MUSIC-PLAYER-001): its tracks, the current
  index, `shuffle` and `repeat`.
- `GET /fans/me/recommendations?limit=12` for the signed-in listener: the artists in
  the order the server ranks them (multi-signal similarity when its flag is on, the
  follows-only ranking otherwise, completed by the popularity fallback).
- For each recommended artist, in that order: the newest public release
  (`GET /music/releases?artistPartyId=<id>&limit=1`) and its tracks
  (`GET /music/releases/{slug}`).

## Rule

When playback reaches the end of the queue (`nextQueueIndex` is negative, which implies
`repeat = off`), the listener is signed in and the flag is on, the player loads the
candidates above and appends `planQueueContinuation(queue tracks, candidates, 5)`:

- walk the candidates in server order; a repeated artist id is ignored;
- from each artist take the first track, in release order, that has at least one
  source and whose recording is not already in the queue or already taken;
- at most one track per artist and at most 5 tracks;
- then playback moves to the first appended track.

If the plan is empty, or anything fails, the player pauses at the end of the queue
exactly as before this change.

## Properties

- **Subset.** Every appended track belongs to an artist the server returned in that
  response. The client adds no artist of its own, so the exclusions proved for the
  server response (listed, not followed, not the caller, followable) carry over to the
  appended tracks as of that response.
- **Order.** The artists of the appended tracks are a subsequence of the server order.
  With the track choice above the result is a function of (queue, candidates): total
  and deterministic.
- **No repeat.** No appended recording is already in the queue, and no recording is
  appended twice.
- **Bound.** At most 5 tracks, at most one per artist.
- **Fallback and opt-out.** The client does not read behaviour. A listener who opted
  out receives the popularity fallback from the server, and the continuation is then
  built from exactly that list. An anonymous listener gets no continuation.
- **Latest intent.** If the queue changed while the candidates were loading (the
  listener chose something, cleared the queue, or the owner changed), the loaded
  continuation is discarded. An explicit playback intent given while it loads (pause,
  play, selecting a track, previous, a newly loaded track, or starting the radio, which
  only pauses the player and leaves the queue as it was) aborts it as well, so it never
  selects or starts a track, and never stops a radio the listener just started.

## Privacy

The requests are the listener's own recommendation read and public catalogue reads.
Nothing new is stored on the server. The appended tracks enter the locally persisted
queue under the existing owner rule (MEDIA-MUSIC-PRIVACY-001).

## Failure and staleness

- Recommendation read fails or returns nothing: no continuation.
- A release read fails for one artist: that artist is skipped.
- The candidates are loaded once per queue end. An artist hidden between the
  recommendation response and the release read is not re-checked by the client; the
  public release routes apply their own visibility at that later time.
- Sources are resolved by the player engine as for any other track (territory, preview
  and access rules of MEDIA-MUSIC-PLAYER-001 are unchanged).

## Evidence

| Property | Evidence | Level |
|---|---|---|
| Subset, order, no repeat, bound (the first two are the critical properties carried to this surface) | `formal/player/QueueContinuation.tla`: Apalache inductive invariant over any served order of three artists, any queue and any limit in range | proved for the model |
| | TLC on the same model; negative control without the queue filter appends a recording that is already queued | model-checked |
| | Jest, seeded random cases over `planQueueContinuation` (1,000 cases per property) and fixed cases | tested |
| Loader keeps server order, skips failing artists, stops early | Jest with stubbed API | tested |
| Disabled flag, anonymous listener, unresolved identity, or a new playback intent | Jest on the decision function and player | tested |
| Signed-in playback moves from the last queued recording to the recommended track | Chromium browser with real loopback audio and synthetic session and catalog responses | tested |

Release visibility changes between the two server reads remain unverified. The seeded tests exercise the same four invariants as the TLA+ model on the TypeScript implementation.
The web workspace has no property-testing library, so the random cases use a small
seeded generator instead of QuickCheck; they are tests, not proofs.
