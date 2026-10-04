# Artist follow continuity compatibility contract

Base: `827d745629f8cdbaee4429ded3689d3bf520a09d` (draft PR 359).
This repairs the canonical public-artist journey needed by the shared discovery/profile
domain; it is not event hiring, a new marketplace, or completion of event operations.

## Evidence and scope, before implementation

`ArtistPublicPage.intent.test.ts` imports two missing exports. The component tests and
Spanish/English `artistFollow` translations still specify an artist-bound signup return
and explicit confirmation. Git commit `85abe7249` contains that implementation;
`01c1e207f` resolves merge conflicts and removes it. Current code also infers “not followed”
from an unfinished/failed query, and mutation callbacks read mutable current render state.
The baseline focused run fails both artist suites at module loading. The unchanged FanHub
onboarding suite also fails five tests (eligibility, error/retry and accessible loading);
its broader restoration is a separate bounded task, not grounds to delete those tests.

Server inspection (`TDF/Server.hs`, `fanListFollows`, `fanFollowArtist`,
`fanUnfollowArtist`) confirms `requireFanAccess` and actor identity from `auPartyId`,
not a browser-supplied viewer ID. `insertUnique FanFollow` gates follow engagement and
follower notification creation. This is source evidence, not a fresh server concurrency
test. The existing club branch also auto-follows other club members in both directions;
that broader consent/privacy behavior is unchanged and requires separate review before
claiming complete discovery consent compliance. The browser fixture does not exercise it.

| ID | Operation and required contract | Formal / executable refinement |
|---|---|---|
| AF-01 | Guest CTA returns to a validated local `/a/<segment>` with signup/intent and an exact positive safe artist ID. Invalid/external/control/encoded-delimiter paths fall back to `/fans`. | Pure URL/ID adversarial tests; existing login redirect sanitizer remains authoritative |
| AF-02 | URL is intent, never mutation consent. Only a click after authenticated session readiness and successful follow-state lookup may follow/unfollow the rendered artist. Coalesce clicks while pending. Unknown/error state disables the toggle and offers retry. | `ArtistFollowConsent.NoUnconfirmedMutation`; component loading/error/click tests |
| AF-03 | Freeze target, follow/unfollow action, session identity and route key at dispatch. A stale follow callback cannot navigate or clear another intent. Check session identity, profile pathname/artist and mount status before and after the onboarding handshake; removing only the consumed resume keys is not a profile change. | `ArtistFollowConsent.CurrentTargetReceipt`, existing `WebOnboardingRecovery`; delayed response/session/route and helper receipt tests |
| AF-04 | Resume only for one canonical `resume=follow` and one exact artist ID. Success or already-followed state removes only these two keys, preserving unrelated query/hash. Failure retains intent and displays safe Spanish/English recovery copy. No automatic follow, even on retry/reload. | Pure duplicate/malformed parameter tests; rendered consent, failure and cleanup tests |
| AF-05 | Reuse existing `Fans.follow/unfollow/listFollows` and server-evidence onboarding. Analytics never turns failed follow into success. Handle asynchronous completion without blocking the follow result. | Existing canonical analytics contract plus new optional current-context predicate tests; component callback tests |

## Finite model and assumptions

`ArtistFollowConsent` has two Parties plus logged-out state, two artist targets, three
effective context generations, known/unknown follow state and one pending command.
Dispatch and context changes interleave; response success/failure is nondeterministic.
Three negative controls remove click, known-state or current-context guards. All must
produce the specified invariant violation, and the positive model must pass before
implementation. URL parsing and React rendering are executable refinements, not TLA+
string/browser proofs. No new Alloy relation is introduced.

The client checks suppress stale UI/analytics, not server authorization or a request
already dispatched. Cookie changes across tabs and in-flight remote revocation remain
server/transport concerns; this branch adds no permissions or credentials. New context
identity is conservatively invalidated on session object changes as well as route changes.
The profile generation advances when session identity, artist ID or pathname changes;
leaving and returning to the same profile cannot revive the earlier receipt. Query-only
cleanup preserves that generation so the current onboarding receipt can still be consumed.
No liveness assertion assumes a hung network request resolves. Cross-tab deduplication,
verified engagement/reputation, bookings and financial state remain outside this repair.

## Compatibility and rollback

Preserve canonical endpoints, generated clients, artist/release/merch rendering, existing
IDs, browser history outside the two resume keys and localized copy. The helper's optional
predicate keeps existing callers source-compatible; callers without it do not gain a new
session-isolation guarantee. FanHub's floating Promise receives explicit best-effort
handling only; its known eligibility/exit defects remain documented, not declared fixed.
No schema, migration, feature flag, mobile pointer, provider or live-money changes.
Rollback reverts this bounded commit; no stored data is migrated or removed.
