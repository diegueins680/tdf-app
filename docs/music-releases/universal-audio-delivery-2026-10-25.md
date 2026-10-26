# Universal audio: first P0 delivery

Baseline: `b732efd65de23e5b97f3f619a2242397c64965c3` (`origin/main`,
October 25, 2026). Branch: `fix/universal-playback-reliability-20261025`.
This is a focused playback-correctness increment. The mission is incomplete.
Merge, release, production playback, physical-device acceptance and universal
catalog coverage require separate evidence.

## Changes and reproduced failures

The release and library pages previously authorized every rendition of every
queued recording before dispatching their selection. The release page could
finish after navigation and overwrite a newer selection. The library could
substitute the first queued track if the explicitly selected recording failed.
Three new page regressions failed on the baseline; the stale-page example
produced `external -> first` after unmount. Pages now synchronously dispatch
asset descriptors; only the global engine resolves the selected recording.
Queued recordings obtain no speculative delivery credentials.

Six additional engine regressions failed on the baseline: browser policy
rejection removed direct audio's URL; stale native playing/waiting events could
contradict a pending selection or pause; unsupported Media Session actions
threw out of an effect; a direct-source reload reset a long mix to zero; decode
failures retried without benefit. Existing 32 focused tests passed before the
new failures were introduced.

The engine now retains valid sources after browser gesture rejection, fences
native events by loaded identity/source and play intent, guards optional Media
Session actions, cancels superseded authorization with AbortSignal and restores
position through the common reload path. Automatic recovery has one timer and
two retries at 1s/2s; pause, selection and unmount cancel obsolete recovery.
A failed native play promise loses ownership once recovery starts. Decode
failure is terminal; protected sources can refresh a media error caused by an
expired grant. Transient authorization failures retry under the same bound;
permission denial remains terminal for that candidate.

Only freshly authorized renditions of the same recording may serve as fallback
for an asset-bound track. Full-audio denial can fall back to its authorized
preview, whose cap applies only when the loaded source is actually a preview.
A protected track cannot downgrade to an unprotected cached URL. No other
queued recording is substituted on denial. Re-selecting the currently paused
queue item resumes its existing decoder. Buffering retains a usable Pause
control. No backend authorization policy or signed-URL lifetime changes.

## Verification boundaries

- Unit tests use controlled HTMLMediaElement doubles and API failures. They
  verify ordering, identity, cancellation, permission refusal, pause, retries,
  preview limits, direct-source recovery and existing queue/analytics behavior.
- Native Playwright tests generate a synthetic 24s lossless master, execute the
  real audio-v2 FFmpeg pipeline and verify its master/derivative hashes. AAC/FLAC
  bytes come from a real loopback range server; metadata/authorization are
  declared fixtures. Browser media clocks, promises and events are not replaced.
- Existing native journeys cover SPA element identity, seek, repeat, rapid
  switching, quality changes, mobile controls and axe checks. New native cases
  cover full-access403 -> preview, expired delivery -> reauthorization at the
  retained position, and terminal failure after two retries. The short preview
  cap case injects the documented engine event with a 1.75s cap because the
  real fixture is shorter than the page's existing 30s cap; this is explicitly
  an engine-contract check, not proof of configurable backend preview durations.
- The finite design model checks 555 selection states/2,144 transitions and
  22 recovery states/65 transitions, plus seven guard-removal controls. Its
  five tests do not prove refinement of React or browser behavior. See
  [player-session.md](../../formal/system/player-session.md) for every invariant,
  assumptions and unresolved obligations. No implementation is labeled PROVEN.
- Native metadata/HTTP403 fixtures do not establish real backend upload,
  entitlement revocation, storage, CDN, Wi-Fi/mobile switching, OS suspension,
  Bluetooth behavior or physical iOS/Android behavior.

Initial browser setup attempts failed before media tests: sandbox loopback
permission, then missing jq on PATH. The existing jq1.8.1 fixture tool was used
for the authorized rerun; no audio processing behavior was replaced to make
the fixture pass. A focused invocation named a nonexistent test path and failed
setup; its 47 actual passing assertions are not counted as a passing command.
The first desktop native run passed7/8 and exposed terminal error text being overwritten. The first four-project matrix passed30/32, including all24 Chromium cases; Firefox expired-delivery cases exposed its reversed promise/error event order. Both defects received implementation regressions. The Firefox rerun passed8/8 (seven native plus one mocked-engine journey). Earlier failures remain recorded; no retries/skips or relaxed assertions were introduced. Final executed results and source fingerprints are in the accompanying
`reports/universal-audio-2026-10-25.json`; pending entries remain pending.

## Performance and observability

There is a concrete request-count improvement: selecting a release formerly
waited for all queued rendition authorizations and then reauthorized the selected
source. Now pages make zero access calls and the engine initially authorizes
only the preferred selected rendition; denial can require bounded same-track
candidate checks. This removes queue size from the normal startup dependency.
The controlled unit fixture has two tracks and four renditions: the old page
waits on four access calls before dispatch; the new page dispatches immediately
with zero calls. This is a deterministic dependency/count result, not an
observed production latency improvement or p95 measurement.

Native runs attach selection-to-trusted-playing samples using browser monotonic
time. Samples run on this shared macOS host against loopback storage with
synthetic API metadata. They do not establish broadband/physical-device p95,
99.5% success or aggregate rebuffering SLOs. Cold/warm cache states, service
conditions and enough independent samples must be defined before release SLO
acceptance. Search performance is unmeasured in this increment.

CI now runs all40 configured native-plus-regression cases in the pinned Playwright image, including WebKit, and its result feeds the existing required quality aggregate. Local WebKit and physical devices remain unverified.

Recovery uses the existing accepted `buffering` analytics event with
`recoveryPhase=attempt` and attempt number; source URLs/credentials are absent.
This preserves the deployed event-type allowlist without a migration. Existing
quality/start/error/seek/pause/progress events remain. Delivery is best effort;
no new dashboard, alert, guaranteed ingestion, artist analytics or consent
policy is claimed.

## Acceptance and release gates

The full 23-criterion initial matrix and prioritized P1-P5 gaps are in
[the architecture audit](universal-audio-audit-2026-10-25.md).
This increment targets AC-004/006/007/008/009/014 and parts of AC-015/017/023
for the existing canonical web player. Each retains its scoped test boundary;
none establishes acceptance for every authorized platform asset.
AC-001/002/003/010/011/012/016/020/021 require additional backend, historical
catalog, rights, storage or production evidence. AC-005 performance,
AC-013 physical support matrix and AC-015 complete WCAG2.2AA are unmet as whole
criteria. AC-018 exact release-candidate CI, AC-019 complete critical-risk
resolution and AC-022 tested target rollback remain release gates.

An anonymous read-only production probe during this increment observed
`/health` returning `{"db":"ok","status":"ok"}` and
`/music/releases` returning HTTP404. This does not establish why the catalog is
unavailable, its configuration or a safe activation path. No feature flags,
production records, uploads, messages, payments or deployments were changed.

Main requires 20 status contexts, strict up-to-date checks, independent approval,
stale-review dismissal, conversation resolution and FIFO integration admission.
These policies remain intact. Draft event audio #608/#615 and other operators'
changes remain untouched. The latest base and current PR heads must be checked
again before merge; historical reviews and test runs cannot be rebound to a
new revision as if newly executed.

Rollback for this frontend-only increment is a reviewed frontend redeploy of
the last healthy immutable artifact through the existing hosting procedure.
It requires no data migration or original-master mutation. Existing active tabs
retain their loaded bundle until reload, so mixed-version operation must stay
compatible with the unchanged API. A rollback drill against the target hosting
service and real active sessions has not been executed here. A passing build
is not a deployment or production playback receipt.

The next highest-value work is to qualify and integrate this P0 candidate, then
extend authorized catalog retrieval to recording titles with pagination and a
universal-search playback projection. Legacy provider links and the independent
radio/preview engines need their own focused corrections. Do not activate the
complete catalog/worker stack merely to make a frontend smoke test succeed.

Browser behavior references checked during implementation: [HTML media play and errors](https://html.spec.whatwg.org/multipage/media.html#dom-media-play-dev), [Media Session action registration](https://w3c.github.io/mediasession/#dom-mediasession-setactionhandler). These define browser boundaries; they are not execution evidence.

## Current-main reconciliation

Protected main advanced to `5114fe95ac8e328b58162a8dcae5c9015a47ce09` through financial integration #551 during this delivery. The normal merge preserves every main requirement exactly except the intentional player requirement update retained from the original playback commit. Main backend, SQL, 220-entry migration manifest, dependencies and Mobile pin are unchanged by the playback delta. Only generated inventory/traceability conflicted and were regenerated. Earlier local tests remain tied to their original inputs; combined-candidate hosted checks are required.
