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

## First hosted qualification follow-up

Head07f66 passed hosted UI/repository/API/migration/formal checks, but the catalog audit correctly rejected nine new verification tuples/fixture-field sets that were untracked during the original local scan. Each now has an individual technical-constant decision; all existing decisions are preserved. Native CI stopped before tests because the root installation container inherited a home owned by UID1001. Native preflight and tests now use its owned `/root` home; the identity/owner guard remains enforced. These are failed original gates requiring fresh-head execution, not waived checks.

## Linux native audio output

Hosted head6e618 passed33/40 native-plus-regression cases: Chromium24/24 and WebKit8/8, plus the explicitly mocked Firefox case. All seven real Firefox cases reached trusted playing then failed with native code3 in3–6ms. Every trace records `NS_ERROR_DOM_MEDIA_MEDIASINK_ERR / OnMediaSinkAudioError`. This identifies a renderer/output-sink failure; it is not a network retry or assertion timeout to waive. The new Linux-only bootstrap starts an isolated PulseAudio daemon on a private authenticated UNIX socket with a clocked null sink, bounded readiness, captured diagnostics and cleanup of only its own PID. No media methods, clocks, error policy or playback assertions change. The null sink [consumes samples paced by system time](https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/User/Modules/#module-null-sink); actual browser decoding and advancement remain required. Local Linux execution is unavailable because the local Docker daemon is stopped; exact hosted execution remains mandatory. A separate unit fixture now waits for the library button to be enabled before clicking instead of assuming one microtask flush means its query completed.

## Directory-main refresh

Playback464d49 passed the complete hosted CI, including40/40 native-plus-regression cases (35realmedia,5mockedengine; zero skips/retries) and fresh independent review5647112905 after its commit. Main then advanced to e938c72ba4b87010214f6838f06bfac45a0f45c0 through directory parity614. The normal refresh preserves all141main requirements except the deliberate existing player-contract delta, all main catalog decisions and its221-entry manifest/backend exactly. Only generated indices conflicted. The prior green evidence is retained for464d49; the refreshed head needs new exact-head checks and review. Integration admission was still waiting for earlierPR616, with no bypass or deployment.

The final consolidated refresh also retains merged privacy/logout616 at main1c9c833cc1ba62b47822fca3c20e14e86cf28daa. All142main requirements except the existing player delta and all1330main catalog decisions are exact; nine audio model/fixture decisions remain separate. Main auth/client/session files, backend,221migrations,dependencies and Mobile are retained. The focused player and privacy suites are rerun together; exact-head hosted qualification and fresh review remain required.
