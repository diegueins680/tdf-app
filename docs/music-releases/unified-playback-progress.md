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
- Artist-owned persisted featured-track selection and validation.
- Physical reconciliation/provenance mapping of legacy ArtistRelease uploads
  with canonical MusicRelease records. Read-time composition preserves source
  identity and does not infer equivalent recordings or rights.
- Explicit handling of artist catalogues beyond the shared adapter's 10,000-row
  limit; a bounded read must not be reported as a complete physical migration.
- Broader component/state-model coverage and native browser verification,
  including torrents, pause/autoplay and artist/radio transitions.
- Hosted required checks/review, then the established release procedure and
  post-deployment smoke verification.

No SQL migration, backend health worker, production change or deployment is
included. Keep the PR in draft until the remaining implementation and validation
obligations are completed.
