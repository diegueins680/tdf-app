# Unified playback — implementation and remaining obligations

## Implemented in this candidate

Radio catalog selections and previews now call PlayerProvider. RadioWidget no
longer creates or controls a playback audio element. Volume, mute, media-session
controls and playback state belong to the shared engine. Torrent resolution is
also owned and disposed by the engine, using its single audio element. Local
microphone capture/broadcast tools remain separate from listening transport.

Artist profile entry clears the previous context immediately. Public canonical
release details build a recording-deduplicated, disc/track-ordered queue. Asset
access is still resolved by the existing server authorization endpoint. Explicit
pause survives automatic artist changes; stale profile fetches are aborted.
There is no automatic artist-to-radio fallback.

Terminal source failure advances forward through the current queue, ignoring
repeat/shuffle while recovering. A failed-ID set prevents recovery cycles. Two
existing network retries precede terminal recovery. Source loading and stalls
have a 15-second watchdog. Offline failures stop without classifying media as
invalid. Autoplay denial retains the selection and never skips it. Native
`playing` events, rather than play-promise resolution, own the visible playing
state. Failed radio entries are omitted from the current session's public list;
this is **not authoritative global quarantine**.

Next-artist-track asset authorization can be warmed with a five-second bound
unless data saving is enabled. It does not download media or establish decoding
health; selection still obtains a fresh grant.

## Verification scope

The focused Jest suite covers engine recovery, explicit pause, stale artist
results, membership filtering, ordering, existing profile behavior, radio layout
and blocked storage. A shared-provider integration case asserts exactly one
audio element and verifies that radio cannot reclaim a later artist context.
The UI production build and changed-source ESLint pass. These are executable
checks, not mathematical proofs or evidence of deployed behavior.

Native fixtures were generated successfully through the real AAC/FLAC pipeline.
Native browser tests could not launch because Chromium is absent; the standard
Playwright installation failed with truncated/non-ZIP downloads. No browser
playback pass is claimed.

## Required before this request is complete

- Persisted, authoritative media health with source-version guards, sanitized
  owner/admin diagnostics, repair/revalidation actions and public selection
  enforcement across all listening surfaces.
- Bounded, SSRF-safe validation on ingestion and periodic revalidation. Client
  failures must not write global invalidity; live/offline streams need a policy
  distinct from missing stored files.
- Artist-owned persisted featured-track selection and validation.
- Reconciliation of legacy ArtistRelease uploads/links with the canonical
  MusicRelease catalog. This candidate selects canonical public releases only.
- Full artist catalogs beyond the current list endpoint's default page.
- Broader component/state-model coverage and native browser verification,
  including torrents, pause/autoplay and artist/radio transitions.
- Consolidated docked presentation: both control surfaces currently use the same
  engine, but the legacy radio dock remains as a second view of that state.
- Hosted required checks/review, then the established release procedure and
  post-deployment smoke verification.

No SQL migration, backend health worker, production change or deployment is
included. Keep the PR in draft until the remaining implementation and validation
obligations are completed.
