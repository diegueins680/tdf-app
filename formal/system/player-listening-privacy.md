# Client listener ownership of playback state

The final validator correction is committed at
`b6259692c11c7b502b974cf6bee7d4ca189925f7`. A matching-owner saved track
with sources `[validSource, null]` reproduced an App render crash under the
previous validator. Nonempty source arrays must now pass the existing predicate
for every member; the whole corrupt track is discarded and valid explicit B can
still play. This closes the named crash path, not every possible malformed-field
case. Prior `a0120daa` 85-test/27-browser results retain their historical identity;
the corrected source has its own 86-test/27-browser qualification.

Requirement: `MEDIA-MUSIC-PRIVACY-001`. Status: implemented and locally `TESTED`
at source commit `b6259692c11c7b502b974cf6bee7d4ca189925f7`. Release
acceptance remains `PARTIAL` pending exact integrated-candidate CI and review.
This contract covers accidental restoration/exposure of local listening state
across client principals. It does not prove server authentication, media access,
cross-tab exclusion, secure erasure or privacy against editable browser storage.

## Evidence and policy

A source-bound audit on main `3af768dacd95c1223975c62da3ce8088bed89b47`
observed selected public titles, queue IDs and resume positions carrying over
through logout, another-account login/reload and a direct account switch. It
used the actual App/session composition with declared route/API/media doubles.
The old native element was paused/replaced; the audit did not establish
cross-account continuous sound or disclosure of server-private media bytes.

The desired logout case also fails on the newer playback baseline
`7de7c292753565ae6ea64c57dd9aa4b97e330b6d`, where the old selected track
remains visible after logout. Audit/design receipts and the exact counterexample
log retain their own source identity. No final corrected-source qualification is
inferred from those reproductions.

The selected client policy separates:

- **Device preferences:** volume, muted and requested quality. These remain
  device-scoped, including their current in-memory values during owner changes.
- **Listening state:** queue, selected track/title, positions, shuffle and repeat.
  These belong to one explicit current client owner, not a device-wide history
  of successive authenticated listeners.

The owner is a valid positive safe-integer party ID, explicit anonymous `null`,
or unresolved/invalid `undefined`. A signed-in descriptor missing a valid ID
must not fall back to anonymous ownership. Anonymous `null` represents one
device-anonymous context; it cannot distinguish different anonymous humans.

## Storage envelope and restoration

One existing `tdf-global-player/v1` envelope adds an explicit own-property
`ownerPartyId`, serialized atomically with queue and positions. Restoration of
listening data requires resolved client identity, a valid explicit owner, an
own-property owner tag and strict equality with the current owner. Legacy records
without a tag, mismatched tags and invalid owner types do not restore listening
data. Existing device preferences can be retained independently.

Matching-owner queue restoration is paused and uses existing track validation and
queue limits. Positions must be finite nonnegative numbers. Serialization keeps
the existing removal of artwork and short-lived canonical source URLs. This is
not a new rights grant, URL-refresh proof, expiry policy or claim that every
legacy direct reference is safe to retain.

Initial unresolved identity must not restore a previous listener's title/queue
or write an empty unresolved envelope over saved state. The first ready
transition may hydrate a matching-owner saved queue **only if no user queue edit
has occurred in the current session**. A new selection or enqueue, selection
change, remove/move, explicit clear, or other queue edit takes precedence over
old saved listening state. An empty current queue after a user edit remains
intentional; checking only whether a current track exists is insufficient.

Initial matching-owner restoration is different from an in-application owner
transition. A rapid A -> B -> A (or anonymous -> account -> anonymous) must not
revive the old envelope merely because the intermediate owner's 250 ms save has
not run and the returning owner's tag happens to match storage. Actual owner
transitions start a new empty listening session, even in that debounce window.

If a listener selects B while same-owner bootstrap is pending, readiness must
not substitute a previously stored A or replace B's decoder/position. If the
explicit owner actually changes during bootstrap, the owner-change reset policy
applies instead. This refines latest-selection safety, not an autoplay exception.

Ready here means the existing **client session** has finished resolving.
Bootstrap failure may retain a cached session descriptor under current session
policy. Matching that label does not establish a successfully verified server
session. Missing storage, invalid JSON and unavailable storage are exercised empty-state
paths. This is not a guarantee for every structurally malformed saved track;
existing per-source validation remains a separate robustness obligation.

## Ownership and transitions

The required production composition is:

`SessionProvider -> identity-aware PlayerProvider -> keyed SessionQueryProvider
-> theme/toast -> App routes and GlobalPlayer`

The engine must live inside the session provider and above keyed query scopes.
Otherwise a same-owner bootstrap/auth-scope occurrence can remount it and revive
old saved state. `App` consumes the shared player; it must not create a second
provider. Query-cache and role/session scope isolation remain independent.

| Event | Required effect |
| --- | --- |
| Initial client identity pending | Suppress listening restoration and persistence; permit current explicit public listening actions under the existing engine policy. |
| First ready, same owner, no queue edit | Hydrate matching saved listening state paused, without autoplay. |
| First ready, same owner, a user queue edit occurred | Preserve the current queue/selection/position and decoder; never revive the old saved queue. |
| Same-owner background loading, auth/query scope change or supported route change | Preserve the engine and listening intent; no unnecessary player remount. |
| A to B, account to anonymous, or anonymous to account | Invalidate/stop prior session work and clear listening state before previous titles can commit under the new owner. Retain device preferences. |
| Owner invalid/unresolved | Do not restore or persist listening state under an anonymous or guessed owner. |
| Prior delayed save on actual owner change/unmount | Cancel/invalidate it so it cannot relabel or repopulate another listener's state. |

An owner change intentionally ends that application's old listening session;
route continuity is conditional on a stable owner. There is no separate retained
envelope for every historical account. Existing source-generation/play-attempt
cleanup and real backend media authorization remain mandatory.

## Verification boundary

The final identity suite must exercise actual App/session/player composition with
declared API, route/layout and JSDOM media doubles. Required cases include logout,
direct A/B/A changes, reload under B, same-owner delayed successful and failed
bootstrap, invalid/legacy tags, explicit anonymous ownership, initial write
suppression, obsolete save cleanup, latest-selection/clear/reorder precedence,
same-owner background loading/navigation continuity and immediate preference
changes followed by an owner switch.

The final source-bound receipt records **86/86 tests in eight suites**: 23
identity cases, 39 engine cases, three queue cases and 21 session cases. Scoped
lint and the Node 22.14.0 TypeScript/Vite/budget build pass; the initial JavaScript
gzip budget is 336,907 bytes. Earlier 74-test observations and the rejected
readiness-remount variant are historical, not final qualification.

The installed browser matrix passes **27/27 cases** with zero skips, retries or
flaky outcomes: **24 real-media native cases and three explicitly mocked smoke
cases**, across Chromium desktop, Chromium phone emulation and Firefox. Native
case `PW-MUSIC-NATIVE-08` passes in all three: hold a synthetic same-owner session
response while fresh B actually decodes and plays, then resolve bootstrap/query
scope and verify that the same native element/source B continues rather than
saved A. Real generated AAC/FLAC, loopback delivery and untouched native
methods/events/clocks provide decoder evidence. Metadata/session/authorization
replies are synthetic; this is not backend identity, cookie, private-delivery or
production authentication acceptance. WebKit, physical devices and hosted
candidate results remain unverified for this increment.

The version-controlled report retains six committed source fingerprints,
qualification/commit receipt hashes, logs and bundle hashes, historical baseline
counterexamples and the exact declared test boundary.

No theorem or extracted/model-checked identity state machine is claimed. The
existing bounded player model does not model owner tags, preferences or account
hydration; its checked bounds/statuses must not be promoted by this increment.
Finite tests can establish `TESTED` behavior for named cases only. Authentication,
private byte access, session-cookie revocation, concurrent tabs, storage tampering,
browser history/backups and physical-device behavior require separate evidence.

## Migration, release and rollback

Local untagged listening state is deliberately discarded while usable device
preferences remain. This does not delete original audio, server libraries or
server listening history. Owner-tagging and overwrite are not secure erasure;
an unresolved session can leave the previous envelope on disk until a valid ready
owner writes the replacement.

Application rollback to an older unscoped loader can restore the known privacy
gap because that loader ignores ownership tags. Older tabs and application
versions are outside the new loader's guarantee. Any rollback/mitigation must
account for that behavior; no target rollback drill is established here.

The privacy increment is stacked on the locally tested playback foundation, then must
normally merge the latest root playback integration before publication. It adds
no server schema, payment behavior, media dependency or production write. Its own
requirement owns identity behavior. Any deliberate addition of `SessionProviders`
to the existing player requirement or ownership mapping is a disclosed mapping
correction only; all other inherited requirement fields remain exact.

`PLAYER-002/003/005/006 privacy boundary -> MEDIA-MUSIC-PRIVACY-001 -> session/player
composition + guarded local hydration -> actual App/identity tests + engine/session
regressions -> exact-candidate CI -> independent review -> target acceptance`.
