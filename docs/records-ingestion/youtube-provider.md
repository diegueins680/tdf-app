# Official YouTube provider boundary

`TDF.Services.YouTube` reads `channels`, paginated `playlistItems`, and batches
of up to 50 `videos` through the official Data API. It accepts a stable channel
identity, verifies the returned channel identity, and reads the returned uploads
playlist. A syntactically valid video ID alone does not verify an association.

Requests use the shared-manager interface, HTTPS on the fixed Google API host,
an API key header, no redirects, a 10-second response timeout plus a 15-second
total deadline per attempt, a 2 MiB decoded
body limit and at most three attempts for transport errors, HTTP 429 or 5xx.
Backoff has bounded jitter. Other HTTP errors, including quota/permission 403,
are explicit failures. Exceptions never expose the request, credential or
provider body. Empty/malformed responses are not successful empty inventories.

The adapter preserves the provider pagination token. Its caller must commit
each accepted page with its durable checkpoint and never reconcile deletions
from an incomplete traversal. Missing video metadata is returned separately
from verified public metadata; it is not a deletion instruction.

Only processed public videos with no active/upcoming broadcast qualify.
Livestream recordings require both actual start and actual end metadata.
Nonpublic titles/descriptions are discarded. Non-embeddable public videos retain
their accurate provider-link status. Thumbnail metadata must match the video
identity. Missing thumbnail variants remain missing; no resolution is invented.
The API has no reliable Shorts discriminator in this response, so duration is
not used to invent one. Dates retain their provider publication meaning.

## Credential and actual provider verification

On 18 September 2026, with explicit project-owner authorization, enabled YouTube
Data API v3 in existing Google Cloud project `tdf-records-477016`. The credential
named `TDF Records video ingestion server` is restricted to that API. Its value
is held in Fly's `YOUTUBE_API_KEY` secret for `tdf-hq`; never put it in a client
bundle, command transcript, checked-in fixture or API response.

The key was initially staged. The existing production deployment subsequently
made it available: Fly reports **Deployed**, and presence-only checks succeeded
on machines `0807ee9cd34668` and `3d8d2500c67168`, running revision
`ab9bbacc9da845b6bfe70ac3fda2ace44f17c918`. This verifies secret delivery, not
scheduled ingestion.

The actual Haskell adapter successfully queried:

- channel `UCx9Jpaw_XDrMtIdzWYlU51g` (`TDF Records`, `@tdf.records`);
- uploads playlist `UUx9Jpaw_XDrMtIdzWYlU51g`;
- one complete page of 39 identities;
- 38 eligible public recordings and one public upcoming/unprocessed broadcast
  held for review; no metadata identities were missing.

This corrects an early investigation note that called the excluded item
nonpublic. Its actual metadata is public, uploaded, upcoming, without an actual
start/end. The two new Llama Este Pez recordings remain absent from the canonical
catalog until the persistence/backfill step is completed.

## Validation and remaining integration

The module and spec compile with the repository's Stack GHC and `-Wall`.
Ten Hspec examples pass, including 100 QuickCheck duration round trips. Tests
cover channel mismatch, wrong thumbnail associations, malformed/duplicate pages,
opaque checkpoint tokens, private/unlisted content, completed/active/upcoming
streams, non-embeddable content, absent metadata and redacted credentials.
Live provider access was tested separately from fixtures.

The adapter itself performs no database writes, schedules or notifications.
Next integration must extend the existing `SocialSyncAccount` identity registry
(platform/external user ID, existing Party/artist links) with separately audited
ingestion approval; a connected account or profile link is not channel-import
approval. Reuse canonical recordings/resources/collections and the existing
catalog run ledger. No second channel identity registry or standalone scraper
is needed. Durable checkpoints, administrative controls and hourly/weekly
execution are not implemented by this provider-only change.

Official references: [channels.list](https://developers.google.com/youtube/v3/docs/channels/list),
[playlistItems.list](https://developers.google.com/youtube/v3/docs/playlistItems/list),
[video metadata](https://developers.google.com/youtube/v3/docs/videos),
[refresh and retention policies](https://developers.google.com/youtube/terms/developer-policies).
