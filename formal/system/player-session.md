# Universal player selection and bounded recovery

Requirement boundary: the persistent web `PlayerProvider`, its synchronous
selection events, and its retry/play callbacks. This is a finite executable
design model with implementation regression tests. It is not a mathematical
proof of React, browser decoding, media delivery authorization, or the platform.

## State and transitions

The intended state machine is:

`idle -> loading/authorizing -> preparing -> playing <-> paused`

`playing -> buffering/recovering -> loading -> playing | error`

Authorization denial terminates in `unavailable`. A browser gesture restriction
terminates the attempt in a user-action-required error/blocked outcome; another
explicit play gesture may resume the same valid source. The implementation's
public status type may use `loading` for preparing/recovering and `error` with an
actionable message for blocked. These are abstraction mappings, not extra UI
states promised by the model.

Selection synchronously installs deferred asset descriptors in the one global
engine. It does not wait for authorization of the complete queue. Each selected
source receives a monotonically increasing generation. `authorizePlayerAsset`
may resolve or reject in any order; only the current source generation may
install its result. Full and preview fallback candidates retain the selected
track identity. Another selection replaces the engine's source and invalidates
obsolete callbacks. `play()` has an additional attempt epoch: pause and a newer
play attempt invalidate an earlier `play()` success or rejection. A pause may
leave metadata preparation pending, but that preparation cannot autoplay.

One retry timer may be pending per engine. Transient recovery consumes at most
two automatic retries for the track/quality selection. Repeated errors while a
timer exists do not schedule parallel retries. Pause, selection/source change,
and unmount cancel the timer. A timer also checks its captured generation before
mutating the engine. User retry is distinct from unlimited automatic retries.

## Executable model

Run from repository root:

```sh
node --test scripts/test-player-session-model.mjs
```

The runner exhausts reachable states by breadth-first search and reports the
shortest counterexample when an invariant fails. Its seven negative controls
remove source-generation rejection, play-epoch rejection, pause invalidation,
pause timer cancellation, selection timer cancellation, the retry bound, or the
single-timer guard. Each must produce a counterexample for the check to pass.

The selection model has two asset identities in three ordered user selections,
`A -> B -> A`, with distinct generations even when identity is reused. Each
selection has one pending authorization response. It explores success/denial,
pending play success/rejection/gesture blocking, up to two additional user
pause/play commands, and arbitrary placement of navigation stutters. The
positive model visits **555 distinct states / 2,144 transitions**. The separate
recovery model permits two selections and two automatic retries and visits
**22 distinct states / 65 transitions**. A finite outcome check explores success
on retry one, success on retry two, and terminal failure after retry two.

## Properties and evidence classification

| Property | Status at this boundary | Evidence and limit |
| --- | --- | --- |
| PLAYER-001 single owner | Assumed by model; implementation must be TESTED | `SessionProviders` owns one identity-aware provider/audio element above keyed query scopes and App routes; model's `engine = 1` does not prove application topology or other legacy players. |
| PLAYER-002 selection consistency | MODEL_CHECKED_WITH_BOUNDS | Loaded and playing generations equal the selected generation; A/B/A prevents identity equality masking an obsolete request. Authorization of the bytes is an environmental assumption. |
| PLAYER-003 state consistency | MODEL_CHECKED_WITH_BOUNDS | `playing` requires current source, current play attempt and play intent. Native browser events/actual sound are outside this model. |
| PLAYER-004 queue integrity | UNVERIFIED by this model | Queue reducer tests are a separate obligation; no queue algorithm is modeled here. |
| PLAYER-005 stale response rejection | MODEL_CHECKED_WITH_BOUNDS | Source and play-generation controls produce concrete counterexamples; recovery timer cancellation is checked separately. |
| PLAYER-006 navigation continuity | Assumed transition; implementation must be TESTED | Navigation is modeled as session-preserving stutter. Real route changes must retain the actual element; the model does not inspect the router. |
| LIVE-003 playback resolution | PARTIAL | Model allows denial, gesture blocking, success and error. A response that never arrives is permitted; fairness/timeouts are not proven. |
| LIVE-004 bounded recovery | MODEL_CHECKED_WITH_BOUNDS, conditional | At most two retries and one timer; finite outcome enumeration terminates when each attempt completes. Browser/network scheduling fairness is assumed. |
| MEDIA-001 through MEDIA-006; LIVE-001/002 | UNVERIFIED by this model | Existing media/publication/authorization models remain separate; no rights or catalog completeness claim follows. |

No property is classified `PROVEN` for the implementation. The runner checks a
design abstraction; the source-to-model relationship is manually mapped and
exercised by focused unit tests and native-browser regression tests. It is not
an extracted implementation, refinement proof, or temporal model checker.

## Implementation correspondence and trusted boundaries

| Model operation | Implementation boundary |
| --- | --- |
| synchronous selection | `MusicReleasePublicPage`, `MusicLibraryPage`, `PLAYER_LOAD_TRACK_EVENT`, and `PlayerProvider.playTrack`/queue selection |
| authorize generation | `PlayerProvider` source effect, `sourceGenerationRef`, `authorizePlayerAsset` |
| current play response | `PlayerProvider.play`, `sourceGenerationRef`, `playAttemptRef` |
| pause | `PlayerProvider.pause`, autoplay intent, attempt invalidation and recovery timer cancellation |
| media event | `PlayerProvider` native listeners, loaded track/current source checks and playback intent |
| retry | `PlayerProvider` error handler, retry count/timer, source-refresh effect |
| navigation | `SessionProviders.tsx`: `SessionProvider -> PlayerProvider -> keyed SessionQueryProvider -> App`; App renders routes/global player without creating another provider |

Model nondeterminism includes arbitrary response order, response rejection,
authorization denial, browser play rejection, user pause/resume, selection
replacement and recoverable media failures. Authorization results are trusted
booleans for the chosen asset. Actual HTTP permissions, preview duration,
private metadata, token expiry, browser event queues, currentSrc semantics,
operating system interruptions, storage, media decode, cross-tab ownership,
and production deployments require their own evidence.

The model's single-source authorization is an abstraction of ordered candidate
authorization, not a check of fallback policy. The retry model abstracts a
refresh as completion of one recovery attempt; it does not establish real
network retry latency. Epoch integers are monotone within a bounded run;
JavaScript number exhaustion and process crashes are excluded.

## Counterexamples retained by the runner

- Stale authorization: select A; select B; authorize A -> source no longer
  corresponds to the selection.
- Stale play success: select A; authorize A; select B; old A play succeeds ->
  visible playback refers to the superseded source.
- Missing pause invalidation: select A; authorize A; pause; pending A play
  succeeds -> UI says playing despite explicit pause intent.
- Missing pause/selection cancellation: error schedules retry; pause or select
  another asset -> obsolete timer remains pending.
- Missing retry bound: repeated errors and recoveries permit a third automatic
  attempt for the same selection.
- Missing timer guard: two errors schedule two parallel retries.

The exact traces and explored counts are printed in test logs. This contract
does not assert that another browser tab cannot play audio, that every platform
surface uses this provider, or that every historical asset is discoverable.

The ownership correspondence was updated by the listener-scoped privacy increment.
Same-owner readiness and navigation retain this provider; actual owner changes
intentionally end the prior listening session. See
[player-listening-privacy.md](player-listening-privacy.md) for finite identity and
real-decoder evidence. Owner tags, hydration and preference retention are outside
this bounded model; its counts, assumptions and verification statuses are unchanged.
