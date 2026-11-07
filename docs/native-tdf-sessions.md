# Native TDF session delivery

Native video/audio extends the existing published Records catalog. It does not
create another artist or catalog. A `tdf` resource on a published recording or
session stores versioned `providerMetadata.tdfNative` delivery metadata. The
existing `/records/feed` response exposes that metadata without an API or database
schema migration. Artist releases match explicit canonical `artistPartyIds`;
names, slugs and guessed contributor matches are not identity evidence.

A session has `awaiting_master`, `processing`, `ready` or `failed` state. Ready
metadata requires separate HTTPS video and audio sources. Chapters have stable
UUIDs and ordered, non-overlapping source offsets. The public session card shows
full-session video/audio actions, its songs and expandable credits. Chapter
recordings retain their own Records identities and source relationship. Original
masters are preserved; delivery derivatives and posters are independent of
YouTube. Existing YouTube resources remain historical source references.

## Incorporating a master

1. Find the existing published Records entity by its canonical UUID and source
   code. Confirm its canonical artist account IDs. Reuse existing resources and
   contributor relationships. Do not import an unrelated source with a similar
   name.
2. Inspect the original with `ffprobe`, verify a SHA-256 checksum and check its
   playback. Preserve the original and its checksum. Read from shared storage
   directly where possible; check free space before preparing derivatives.
3. Prepare H.264/AAC MP4 delivery renditions (for example 720p, 480p and 360p),
   with `-movflags +faststart`, plus an independent AAC/M4A audio stream and an
   owned poster. Validate dimensions, durations and synchronization. Audio mode
   must not request a video rendition.
4. Check chapter titles and offsets against the original. Exclude introduction,
   adjacent songs and closing credits from song playback. Store recording and
   publication dates separately and preserve verified credits.
5. Write a private manifest with `schemaVersion: 1`, `entity` (`kind`, `id`,
   `expectedCode`), `title`, `posterUrl`, `localFiles` and `native`. The native
   object contains `schemaVersion: 1`, `kind: "session"`, `artistPartyIds`,
   `artist`, `state`, `durationMs`, dates, `sources`, `chapters` and `credits`.
   Source fields are `url`, `quality` (`low`, `medium`, `high`, `lossless`) and
   `mediaType` (`video/mp4`, `audio/mp4`, `audio/mpeg`). Each chapter contains
   `id`, `title`, `startMs` and `endMs`; each credit contains `name` and `role`.
6. Run `python3 scripts/prepare-native-session.py manifest.json --inspect-media --sql /private/tmp/native-session-import.sql`. The tool validates the manifest
   and prepares SQL; it never connects to production or uploads a file.
7. Upload only inspected derivatives to TDF-controlled storage, retaining space
   headroom. Use immutable content-addressed filenames, verify server checksums,
   MIME types and public `206 Partial Content` responses, including end ranges.
8. Rehearse the exact SQL on an isolated database restore: stale identity refusal,
   rollback restoring original rows, successful import, idempotent replay and
   unchanged accounts, permissions and migrations. Apply the reviewed transaction
   once through the authorized operator workflow, preserving its hash and receipt.
9. Read back `/records/feed` and test full session/song playback in the actual web
   build with YouTube blocked. Verify desktop/mobile layouts, mode/quality
   switching, navigation continuity, source failures and radio takeover. Public
   data activation and deployed web verification are separate receipts.

## Playback and failure behavior

`PlayerProvider` retains media elements above navigation and gives only the
selected engine a source. Native sessions use a single persistent video element
for both MP4 video and independent audio-only playback. Ordinary release audio
keeps its existing audio element. Switching engines releases the previous source.
Format/quality switches restore the source position after metadata is ready;
chapter controls expose relative times and enforce source offsets on seeking and
progress events. Pausing or radio takeover cancels obsolete playback intent.

The global controls provide transport, volume/mute, quality, 0.5–2x speed,
chapters, queue, fullscreen and picture-in-picture. Optional browser capabilities
are detected. Media Session reports metadata, state and position where supported.
Desktop browser/emulated mobile checks do not qualify physical iOS/Android
background playback, screen-lock policies or every fullscreen/PiP implementation.

The canonical Music collection intentionally answers 404 when its public feature
is disabled. That condition is distinguished from a failing enabled catalog;
other failures retain available legacy/Records entries and a retryable notice.
An unavailable source is never treated as an empty successful catalog. A
malformed Records response cannot discard successful releases from other sources.

For Roj(o), use artist account 535 and existing recording
`8e549880-4881-4423-9819-122f749287b2`; the old artist 520 was merged separately.
The original session description establishes Caída 00:12, Huellas 05:37, Canto
Menguante 11:15 and Ritual 15:15. The checked song endpoint for Ritual is 21:07,
before the closing credits; full-session playback includes the credits.

This implementation does not declare every stored session ready automatically.
Missing originals, unverified chapters and inaccessible NAS files remain explicit
operational dependencies. Full application conformance and production release
approval are not established by these scoped playback tests.

The current production asset route was also probed with an explicit `Origin`
header: that variant streams HTTP 200 instead of honoring Range. Native media
elements use the tested requests without `crossorigin`; beginning/end range
samples returned 206 and matched the uploaded files. CORS-enabled media fetch
adapters need a server-side fix and separate qualification before adoption.


Chapter transition and imported recording boundaries

Imported individual recordings (`tdfNative.isTrack: true`) retain their canonical
recording publication identity and validated source `startMs`/`endMs`. Their card
and recording deep link use the same bounded global-player path as chapter
selection; the full-session URL does not mean playback starts at zero. Invalid
bounds retain the existing malformed-metadata fallback rather than inventing a
valid native range.

An internally requested pause is consumed before loading the next source. This
keeps its delayed native event from cancelling the next chapter's autoplay and
prevents an unconsumed suppression counter from hiding a later user pause.
Already-paused elements do not wait for an event that `pause()` will not queue.
Source generations and abort guards select only the latest continuation; unmount
removes its pending listener. Explicit user pause cancels intent synchronously.
These are covered by controlled asynchronous media-event tests and an owned
synthetic native MP4 browser fixture, not an all-schedules proof or authorization
to fetch, publish or operate external media.
