# Artist subscriptions and implicit fan-club effects — 2026-09-16

Dependency: [draft #409](https://github.com/diegueins680/tdf-app/pull/409), exact
parent `7e601607f60b2987e502c4a14db821bbf37df296`, on #402 → #397 → #390.
Parent [full CI35129595035](https://github.com/diegueins680/tdf-app/actions/runs/35129595035)
has completed successfully, including backend, HTTP, migration and browser checks.
Skipped UI/mobile jobs are not counted as passed. This packet does not authorize
merging, deployment, or activation of production flags.

## Audit and decision

`Server.fanFollowArtist` previously created FanFollow, an engagement event and a
named `artist_liked` notification, then copied a fan profile into the artist's club
and created PartyFollow edges in **both directions with every existing member**.
One person's artist subscription therefore manufactured other people's apparent
relationships. Work grew with the entire club roster. `fanUnfollowArtist` did not
share account/session locks, allowing removal to race follow's response construction.

| Capability | Decision | Acceptance / boundary |
|---|---|---|
| POST `/fans/me/follows/:artistId` | **Refactor**, reuse FanFollow and exact FanFollowDTO | Preserve directed artist subscription, timestamp and engagement record; apply current session and existing profile eligibility |
| Automatic FanClubMemberProfile copy and reciprocal PartyFollow fanout | **Retire after enforcement** | No roster query, profile creation or graph writes after durable activation, any canonical pair, or any canonical closure; preserve existing rows |
| New named artist-follower notification | **Suppress after enforcement** | No new unqualified name-bearing notification; no inferred consent from membership |
| DELETE `/fans/me/follows/:artistId` | **Repair concurrency/session boundary** | Serialize with follow; removal of one's own subscription remains possible when artist visibility is denied; idempotent 200 NoContent |
| Existing Fan/Customer role requirement, artist profile identity, commercial workflows | **Reuse** | Same role/identity rules and URLs; no booking, purchase, EPK or release ownership changes |
| Existing notifications and counts, `fanListFollows`, explicit member-profile APIs | **Separate repair required** | Stored notification bodies lack follower provenance; current list/count serving still needs canonical eligibility. Member-profile visibility/cleanup is not solved here |
| Notification consent, role-grant revocation races, organization delegation, spam controls | **Deferred / activation blockers** | No promise of all-domain authorization or abuse protection from this adapter |

FanFollow is authoritative for the user's artist subscription and existing club
access rules. FanClubMemberProfile is club-specific profile data, not evidence of
bilateral connection consent. PartyFollow remains historical directed graph data.
SocialV2Pair exclusively owns canonical connection/follow/block state. Neither club
membership nor artist subscription grants another member's consent or reputation.
Unfollow does not erase historical club profiles/edges/notifications; a reviewed
data-lifecycle repair must resolve those without deleting user-authored content.

## Policy and compatibility

`TDF.Social.FanEffects` supplies both production handlers. Route aliases in
`TDF.API.FanFollowing` are shared with the real bearer HTTP harness. The existing
Fan/Customer and coherent-role check is reused by other fan routes through the
existing `requireFanAccess` name. Admin alone is not a Fan grant.

- No foundation: preserve the legacy operation. Foundation with missing adapter:
  503, even before activation. Do not fall back based on a temporary empty graph.
- Fully staged adapter: lock runtime `FOR SHARE`, ordered actor/artist Party and
  credential rows, then the actual token; verify current owner/active/purpose.
- Follow calls `social_v2_profile_eligible` after the locks. Denial returns 404
  before artist DTO fields, subscription or side effects. The current account-only
  pilot excludes governed organization/inactive/closed/blocked identities; it does
  not invent organization delegation or authorize an artist merely by graph proximity.
- Legacy automatic effects are allowed only if activation has never occurred,
  the canonical pair table is empty, and no canonical closed preference exists.
  This reuses the global legacy-inference retirement predicate from #402. An
  unrelated canonical state conservatively retires fanout, without independently
  denying the requested artist subscription.
- Once effects retire, skip the entire roster/profile/notification path, rather
  than loading members and filtering them client-side. Existing artist subscription
  DTOs remain compatible with both web and native clients. No new selection IDs/UI.
- InsertUnique FanFollow and account locks deduplicate same-subscription retries;
  only the inserting operation creates its engagement/legacy notification record.
  Existing creation times, member handles/bios and historical NFC edges are retained.
- Unfollow rechecks the session and Fan/Customer scope and uses the same locks. It
  removes only the caller's FanFollow and records one unfollow event only when that
  row existed. Target visibility is unnecessary for this cleanup; no target fields
  are returned. Blocked/closed target cleanup remains available.

Following and unfollowing have no revision/request key in the legacy contract.
If removal wins the locks, a later follow may legitimately recreate a subscription.
If follow wins, it returns a valid response and the subsequent removal may delete
the row. An explicit unsubscribe/resubscribe is a new subscription lifetime and may
create a new legacy notification before retirement; it is not the same retry.
No exactly-once transport or stale-client intent guarantee is claimed. Canonical
connection commands retain their separate revision/idempotency protocol.

The guard requires READ COMMITTED. Runtime activation waits for admitted legacy
work to commit. Canonical public mutations require `enabled=true`, which durably
sets `activated_once`; therefore a public canonical mutation cannot introduce the
first governance state during an admitted never-activated transaction. Existing
governance state is tested before admission. Direct owner SQL bypassing these
protocols is outside the model. Token/account locks and fresh profile policy protect
the follow action itself against committed token, block and closure changes.

Deadlock/serialization failures return retryable 503, unsupported isolation 503,
other SQL failures 500 without internal details. No caches or projections grant
rights, and no new queue/outbox/job/cache/infrastructure is introduced.

## Dated research to decision

Primary sources accessed **2026-09-16**; project-specific choices are distinguished
from the sources below. Earlier graph/storage/discovery decisions remain applicable.

| Problem | Primary evidence and publication date | Selected inference / rejected alternatives | Validation |
|---|---|---|---|
| A subscription is not every member's relationship intent | [ActivityPub Follow/Accept](https://www.w3.org/TR/2018/REC-activitypub-20180123/#follow-activity-inbox), W3C Recommendation 2018-01-23, distinguishes actors' Follow and Accept activities | Keep FanFollow independent of PartyFollow/canonical consent. Retire implicit member fanout, rather than importing all reciprocal history as acceptance. This does not add federation or require ActivityPub's whole protocol | NoImplicitEffects, generated stored-edge/profile checks; existing consent models remain separate |
| Activation or removal races independent database statements | [PostgreSQL 17 row locking](https://www.postgresql.org/docs/17/explicit-locking.html#LOCKING-ROWS), versioned official docs, page update unavailable | Hold shared runtime and ordered account/token locks through the operation; serialize both follow and unfollow. Reject standalone preflight checks or unrelated write transactions | Actual HTTP races and TLC UnsafeLocks |
| Large clubs cause unnecessary work | Source audit identifies two per-member insertUnique loops; this is repository evidence, not a borrowed scale architecture | Do not enumerate members after retirement. Reject a graph database, fanout queue or cached permission list for a side effect the product does not need | Missing-roster-relation regression; 100/10,000-member handler benchmark |

## Executable model and traceability

`formal/social/FanEffects.tla` was checked before implementing its changes, then
extended and checked again **before** implementing serialized unfollow. Pinned TLC
distribution 1.7.2 / Java 17.0.12; jar SHA-256
`fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22`.

Bounds: one subscriber/artist pair, two existing club members/four possible new
directions, four representative starting edge sets, club/profile presence,
subscription state, two follow attempts with retry, concurrent unsubscribe,
activation/pause, existing/new canonical governance, eligibility and token denial,
and transaction phases. Generated observations check actual committed outcomes,
not a second implementation of the permission predicate.

| ID | Property/actions | Mechanism | Implementation test |
|---|---|---|---|
| FE-01 | NoImplicitEffects; Activate/Pause/Govern, Lock/Commit | Monotonic activation + global retirement predicate; shared runtime lock through transaction | Generated edge/profile/notification counts; activation wins/follow wins races; unrelated closure |
| FE-02 | CurrentSession; Revoke/DenyAccess/Check | Ordered account/credential locks, withCurrentSession, shared profile eligibility before DTO construction | Model-generated 401/404 outcomes; token/block/closure HTTP waiters; Fan/Customer scope checks |
| FE-03 | SubscriptionPreserved; Commit | Reuse FanFollow and exact DTO independent of automatic effects | Generated subscription state, unchanged DTO/timestamps, no-roster relation regression |
| FE-04 | NoDuplicateAlerts; Retry/Unfollow | UniqueFanFollow and shared locks on both operations; notify only on new row | Generated alert counts, competing retry HTTP test, actual POST/DELETE ordering |
| FE-05 | Progress; weakly fair Lock/Check/Commit/Retry | Finite transaction/error outcome under available database and scheduling | TLC liveness; bounded real race barriers; no outage/starvation proof |

Final positive result: **30,773 generated / 12,738 distinct states, depth 15**;
safety and weak-fair progress passed. Four deliberate controls detect early lock
release, forgotten activation after pause, ignored existing governance, and
notification creation on retry; the checker requires the named invariant failure.
**708 observed Commit cases** are regenerated from the checked DOT graph and run
through the actual shared HTTP handlers against private PostgreSQL.

The model abstracts canonical governance and actor/artist eligibility; the prior
profile/session/relationship models qualify those policies. Role-grant revocation,
delegation, arbitrary trusted SQL, old binaries, deletion/export retention,
notification delivery, unbounded callers and transport are not proved here.
Unsubscribe is modeled as an independently serialized source mutation; extra HTTP
tests cover its current-token enforcement. A passing bounded model is not proof of
the production system or full membership/notification privacy.

## Migration and rollback

Apply `2026-09-16_social_v2_fan_effects.sql` after #409 and its prerequisites. It
adds/replaces **one function**, with PUBLIC execution revoked; the existing trusted
application owner invokes it. No new table, index, destructive down migration,
backfill, automatic production migration registry entry or new production flag.
No historical profile/edge/notification is deleted or converted into consent.

Deploy both follow and unfollow handlers as one version and drain old writers
before relying on the serialization guarantee or activating social enforcement.
Old handlers do not participate in these locks or retirement. Read compatibility
does not qualify mixed old/new writer concurrency. Install all functions before
directing requests to the adapter; partial installation intentionally fails closed.

Pause existing process/UI flags and `social_v2_runtime.enabled` while retaining
the durable latch, adapter and source data. Artist subscriptions and authorized
cleanup remain usable; implicit effects stay retired. Rollback must retain this
boundary or disable the routes; do not restore unsafe writers or reset the latch.
New subscriptions/engagement records survive pause/reapply. No destructive cleanup
of historical fanout is part of rollback. Explicit profile editing/join flows and
historical notification serving need separate privacy qualification before rollout.

Monitor aggregate follow/unfollow successes, denies, serialization retries, lock
waits and latency. Do not log tokens, names, notification bodies, or pair identities.
No implicit-effect retirement count is evidence of a useful collaboration. Keep
accepted relevant connections, leads, bookings and sales as the product outcomes;
this PR demonstrates no production conversion gain. Subscription toggling is not
reputation, and preactivation notification spam controls remain unqualified.

## Reproduce and verify

```sh
TLA_JAR=/path/to/tla2tools-1.7.2.jar bash scripts/social/check-fan-effects-model.sh
# CI exports DOT, regenerates FanEffectsModelCases.hs and compares it exactly.
TDF_SOCIAL_FAN_EFFECTS_BENCHMARK=1 TDF_SOCIAL_HTTP_NATIVE=1 bash scripts/social/test-http.sh
stack --stack-yaml tdf-hq/stack.yaml test --fast
TDF_SOCIAL_SCHEMA_NATIVE=1 bash scripts/social/test-schema-compatibility.sh
npm run audit:catalog-lists
```

Only private synthetic fixtures; no application `.env` or shared database. The
benchmark compares one legacy first-follow probe (10s timeout, no percentile) with
5 warmups/40 repaired-handler samples at 100 and 10,000 club members. Thresholds
declared before measurement: repaired p95 <=100ms and <=250ms respectively, with
no member-profile/graph/notification side effects. It includes the shared handler's
role/session checks and database work, excludes initial authentication, HTTP/JSON
encoding and network, and runs on a shared workstation. No production-scale claim.

The first HTTP run (before adding competing unfollow) passed **1,046 examples**.
Final results and acceptance statuses are recorded with the evidence before PR
publication. The first full Stack build failed for **disk exhaustion**; the first
Docker PostgreSQL 17 fixture failed when its filesystem became **read-only**.
A later build caught an incomplete extraction of the old unfollow body; that edit
was corrected before the final verification. These are recorded failures, not
passing checks. No shared Docker restart, user-file cleanup, assertions removal,
CI timeout extension or production change was used to address them.


## Final local evidence and acceptance — 2026-09-16

[Committed evidence](evidence/fan-effects-20260916/) includes model output and four
expected counterexamples, actual HTTP/benchmark output, complete-schema fixture,
backend test excerpt with full-log fingerprint, frontend output and parent CI JSON.
All commands below exited zero unless explicitly labeled failed/blocked.

| Criterion | Status | Actual result / limitation |
|---|---|---|
| Implement subscription/effect separation and current-token serialized writes | **Satisfied** | Shared production handlers, unchanged paths/DTOs; historical rows preserved |
| Bounded safety/progress plus implementation refinement | **Satisfied** | TLC 12,738 distinct states; four detected counterexamples; 708 generated observations among **1,141 HTTP examples, zero failures** |
| Backend compilation and existing regression suite | **Satisfied** | Stack GHC 9.10.3, **2,542 examples, zero failures** |
| Complete-schema apply/reapply/pause | **Satisfied for native PG16.10** | 102 registered migrations and preserved-write checks passed; hosted PG17 qualification still pending |
| Local Docker PG17 | **Failed environment / blocked** | Read-only Docker filesystem; not counted as successful PG17 validation |
| Synthetic handler performance | **Satisfied within stated bounds** | 100 members: legacy single probe 298.93ms; repaired p50 12.80ms / p95 49.22ms. 10,000 members: legacy exceeded 10s and rolled back; repaired p50 12.50ms / p95 50.05ms. 40 samples each; no production throughput/cost claim |
| Frontend static checks and selector contracts | **Satisfied** | App TypeScript and changed-page ESLint exit 0; existing selector suite **11/11** |
| Full browser/native journey for this member-profile state | **Blocked / not run** | Static checks do not establish runtime accessibility or native behavior |
| New production flag/infrastructure/data cleanup | **Intentionally deferred** | Reuse inactive gates and durable latch; function-only additive migration, no history deletion |
| Historical notifications/counts, artist-follow GET and explicit member-profile lifecycle | **Blocked** | Next privacy integration work; no whole-domain authorization claim |
| Hosted checks at this implementation head | **Blocked pending PR CI** | Parent #409 full CI succeeded; parent evidence does not qualify this head |
| Useful outcome/conversion improvement | **Blocked until authorized later rollout** | Measurement hypothesis only; no production experiment |

The member-profile empty state now distinguishes absent profile data from club
membership, uses the existing Spanish design system and offers a return link.
Following no longer promises profile publication after retirement. The existing
editing form only renders for an existing profile: an explicit, consented profile
creation journey and actor-scoped profile caches remain prerequisites for cutover.
This copy change does not qualify those APIs or their privacy policy.

Two additional failed development runs are retained: the full backend suite caught
a changed unfollow self-error string (restored exactly); a real HTTP race fixture
used `rawExecute` with SELECT result rows, throwing before releasing its barrier.
Changing that test fixture to `DO/PERFORM` fixed its SQL execution contract; no
assertion or timeout was weakened. Mistaken local Vitest and UI-local binary paths
never ran tests; the correct hoisted Jest/TypeScript commands above subsequently
passed. No screenshots or production runtime results are claimed.

The earlier automatic Vercel/Cloudflare deployment exception in the consolidated
handoff still requires owner review/removal. This work issued no deployment or
activation command and uses the existing social-branch/provider-skip protections.
