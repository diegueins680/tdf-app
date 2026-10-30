# Unified playback — implementation and remaining obligations

## Implemented in this candidate

Radio catalog selections and previews now call PlayerProvider. RadioWidget no
longer creates or controls a playback audio element. Volume, mute, media-session
controls and playback state belong to the shared engine. Torrent resolution is
also owned and disposed by the engine, using its single audio element. Local
microphone capture/broadcast tools remain separate from listening transport.

Artist profile entry clears the previous context immediately. The shared artist
catalogue reads canonical pages with a hard 10,000-row bound and combines public
legacy releases. Canonical details build a recording-deduplicated, disc/track-ordered
queue, followed by legacy direct HTTP(S) media from the same Party. Known provider
pages retain their external-platform action. A canonical catalogue outage can
still prepare available public legacy media. Asset
access is still resolved by the existing server authorization endpoint. Explicit
pause survives automatic artist changes; stale profile fetches are aborted.
There is no automatic artist-to-radio fallback.

Terminal source failure tries the remaining authorized renditions of the current
recording, then advances forward through the current queue, ignoring repeat/shuffle
while recovering. Failed-source and failed-track sets prevent recovery cycles. Two
network retries per rendition precede terminal recovery. Source loading and stalls
have a 15-second watchdog. Offline failures stop without classifying media as
invalid. Autoplay denial retains the selection and never skips it. Native
`playing` events, rather than play-promise resolution, own the visible playing
state. Loading does not advertise an active radio presence. Failed radio entries are omitted from the current session's public list;
this is **not authoritative global quarantine**.

Next-artist-track asset authorization can be warmed with a five-second bound
unless data saving is enabled. It does not download media or establish decoding
health; selection still obtains a fresh grant.

The global player is the only dock after any track is selected. The radio panel
remains a catalogue, station-management and broadcasting view of the same engine;
a compact catalogue launcher replaces its redundant transport dock. A silent
radio selection still loads the chosen source without calling play.

## Verification scope

The focused Jest suite covers engine recovery, explicit pause, stale artist
results, membership filtering, ordering, existing profile behavior, radio layout
and blocked storage. A shared-provider integration case asserts exactly one
audio element and verifies that radio cannot reclaim a later artist context.
The UI production build and changed-source ESLint pass. These are executable
checks, not mathematical proofs or evidence of deployed behavior.

Local integration verification passed 331 suites / 3,817 tests after merging the
concurrent profile-link fixes. A later explicit-retry regression checks that an
exhausted selection can start another bounded attempt. The final count and source
identity are recorded in the PR. TypeScript, production build, changed-source
ESLint, catalogue authority and immutable specification admission passed; the
repository formal audit found no error-level findings. These checks do not prove
server-side media health or deployed behavior.

Hosted native CI on the earlier 8de15e02ee179de5990019764829eb939deba976
candidate passed 50 real AAC/FLAC browser journeys. Its five remaining failures
were in the deterministic media fixture, which emitted playing without reflecting
currentSrc. That fixture now models currentSrc as well. The native terminal case
now exhausts every rendition before expecting a track change. Current-head native
verification remains required. Local browsers are absent and the attempted
Playwright installation returned truncated ZIPs; no new local native pass is claimed.

## Required before this request is complete

- Persisted, authoritative media health with source-version guards, sanitized
  owner/admin diagnostics, repair/revalidation actions and public selection
  enforcement across all listening surfaces.
- Bounded, SSRF-safe validation on ingestion and periodic revalidation. Client
  failures must not write global invalidity; live/offline streams need a policy
  distinct from missing stored files.
- Qualification of the implemented artist-owned featured-track preference and
  selector (artist-featured-track.md), including hosted backend/PostgreSQL checks
  and publication of the matching Mobile contract.
- Physical reconciliation/provenance mapping of legacy ArtistRelease uploads
  with canonical MusicRelease records. Read-time composition preserves source
  identity and does not infer equivalent recordings or rights.
- Explicit handling of artist catalogues beyond the shared adapter's 10,000-row
  limit; a bounded read must not be reported as a complete physical migration.
- Broader component/state-model coverage and native browser verification,
  including torrents, pause/autoplay and artist/radio transitions.
- Hosted required checks/review, then the established release procedure and
  post-deployment smoke verification.

The featured-track preference includes additive SQL as described in
`artist-featured-track.md`; it is not a media-health worker. No production
migration, production change or deployment has run. Keep the PR in draft until
the remaining implementation and validation obligations are completed.

## Discover → listen → follow delivery checkpoint, 30 October

`play_start` now requires the current, unpaused native source's `playing` event.
A fulfilled `play()` promise alone does not count a listen. Repeated `playing`
events after buffering do not duplicate a start within one play attempt; a new
explicit resume can record its own confirmed start. Existing account/source,
authorization, pending-position and explicit-pause fences remain in place.
Three new actual-provider component tests cover this boundary. The original
implementation fails the promise-only and native-before-promise controls.

The missing featured-track compiled snapshot is regenerated from the actual
TypeRep artifact12960059760 of CI41014941561 (merge checkout
`8e370e3b6fdc15cfa85af2582770c64c6ae92406`, tree
`d514a90e6c70ccb497f72b474b70d71063e71736`). Its ZIP SHA256 matches the GitHub
artifact digest `d9d9f49782cd945224a15cdff68ba89a8999ec29933707f721500aada08250c1`.
Review of the decoded artifact finds exactly three added featured-track routes,
no removed routes, and no change to any prior operation except its ordinal.
The public read is registered as public-read; both owner operations retain
their compiled authentication. Availability, status and public-route admission
checks pass. Replaying the retained TypeRep reproduces the snapshot exactly;
the previous snapshot fails the existing declaration and availability checks.
This is reuse of identified compiler evidence, not a new local backend build.

The Mobile companion patch now includes normal context. Ordinary `git apply`
on the former zero-context patch could place additions differently from the
canonical generator; the replacement applies to the exact published pin and
reproduces the generated web/Mobile bytes. Its generated types also type-check
in isolation. Publishing Mobile remains blocked by an actual integration403,
including when using the repository's canonical `TDF-mobile` capitalization.
The root pin and generated-client drift gate remain unchanged.

Local validation: 332 UI suites / 3,831 tests pass, as do the focused68,
TypeScript, changed-source lint, production build (339397gzip initial bytes),
twelve compiled-surface controls, public-route admission and catalogue audit.
An initial full suite was launched from the root instead of the UI workspace;
two relative-path fixtures failed. The complete workspace invocation above
supersedes that harness mistake without source/test weakening. Local Chromium
installation failed, so no new local native-browser pass is claimed.

Read-only production observation: anonymous `/inicio` discovery opens
`/artista/115`; its saved `/a/verde70` URL preserves the Verde70 profile. The
follow link retains the same artist and an explicit post-login follow intent.
That profile has no published TDF releases; its external Spotify embed is not
proof of global-player playback. Backend `/health` returned200; `/version`
reported `4dac175d42616b5b2f4a3e8faebb83692cd2e6b0` and the sampled
`/music/releases?limit=1` returned404. The404 alone does not identify a flag or
its cause. No authenticated follow, account creation or production mutation ran.

The Docker cleanup defect is corrected separately in existing PR679 using its
captured actual response, with twelve runner simulations and two shell/Compose
checks. Protected integration currently waits for PR642; PR676's current
workflows are successful but admission is still pending. Required media-health
implementation, Mobile publication, current-head hosted checks/review and the
guarded deployment/recovery prerequisites above remain incomplete. This
checkpoint does not accept the full discovery/listening/follow delivery request.
