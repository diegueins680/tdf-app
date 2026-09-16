# Legacy profile read boundary — 2026-09-15

Status: implemented locally; draft dependent delivery, no activation or deployment.
Dependency: [DM/API PR #390](https://github.com/diegueins680/tdf-app/pull/390),
code base `44a1c7bededf0ba4b54068d482667ac24299f8e4`. Client isolation #391 and
verification #394 are parallel dependent work, not merged into this branch.

## Audit and policy decision

`TDF.Server.socialListProfiles` and `socialGetProfile` ignored the authenticated
principal. Their `loadSocialPartyProfilesDTO`/`loadSocialPartyProfileDTO` helpers
read Party and FanProfile directly. Both `/social/profiles?partyId=…` and
`/social/profiles/:partyId` could therefore return display name, avatar URL, bio
and city after a canonical block or closure. Existing tests covered identifier
validation, not the real bearer/policy/persistence boundary. **Repair**, retaining
the DTO and existing single/batch URLs. No new public or private profile setting.

Party owns identity/default name; FanProfile owns its optional display name,
avatar reference, bio and city. Canonical pair/preference rows own social denial;
ApiToken owns bearer authority. No cache, connection, follow, purchase, role or
projection independently grants this read. Global Admin does not bypass a block.

| Context | Existing five-field profile response |
|---|---|
| No social foundation installed | Original handler, unchanged compatibility stage |
| Foundation present but profile adapter incomplete | 503, including empty/never-activated foundation; never race a first block with a legacy fallback |
| Never activated, no canonical pair or closure involving either identity | Preserve existing profile eligibility; still revalidate bearer token in repaired adapter |
| Activated once, canonical pair exists, or either identity closed | Both accounts must be live under canonical account-only policy; either direction of block denies |
| Mute, Discover opt-out, no follow/connection | Does not independently deny a direct profile read |
| Closed identity | Deny; separate authenticated preference/closure controls remain separate |
| Missing/denied single target | Identical generic 404; no profile fields or diagnostic reason |
| Mixed batch | Filter entire denied/missing rows, preserve input order, no denied counts |
| Invalid/duplicate/nonpositive ID or >100 IDs | Existing 400 validation contract |
| Revoked/reassigned/ineligible captured bearer | Current-session 401, even empty batch |

Activated policy currently excludes organization identities. Delegated management,
organization membership and global role revocation are not qualified by this
account-only pilot. Do not activate broadly until those integrations are addressed.
Discoverability is a recommendation preference, not a profile audience. These
endpoints already expose the five fields to authenticated users; this patch does
not create consent to publish additional fields.

## Evidence to decision

Sources re-read 2026-09-15 (local access date):

| Problem | Primary evidence and date | TDF choice / rejected alternative | Validation |
|---|---|---|---|
| Policy and content disagree during concurrent changes | [PostgreSQL 16 function volatility](https://www.postgresql.org/docs/16/xfunc-volatility.html), versioned official docs; page update unavailable | STABLE policy + payload in one statement snapshot. Reject separate preflight then content queries and IMMUTABLE authorization. | Model Read action, PostgreSQL generated outcomes, STABLE catalog assertion |
| Block scope confused with publication/recommendation scope | [ActivityPub §6.9](https://www.w3.org/TR/2018/REC-activitypub-20180123/#block-activity-outbox), W3C Recommendation 2018-01-23 | Project-specific bilateral profile exclusion; mute and discoverability remain separate. ActivityPub does not specify this local profile policy or promise retrieval of public copies can be prevented. | Three negative model variants, actual HTTP missing/denied equivalence and preference tests |
| Per-candidate authorization costs too much | Paired synthetic benchmark below; TDF measurement, not external evidence | Materialize viewer state once and join requested identities to authoritative rows. Reject introducing a graph engine or raising the failed threshold. | Same model cases and 50ms p95 threshold before/after |

## Implementation and concurrency boundary

`TDF.API.SocialProfiles` shares the exact existing Servant types with the production
router and focused HTTP harness. `TDF.Social.Profiles` wraps both original handlers,
uses `withCurrentSession ReadSession`, and decodes only authorized rows. Unicode
name trimming/fallback stays in Haskell to match the prior `Text.strip` behavior;
other fields are unchanged. Existing clients keep the same URLs and DTOs.

`2026-09-15_social_v2_profile_reads.sql` adds two STABLE functions:
`social_v2_profile_eligible` is the batch policy and `social_v2_profiles` validates,
joins payload and preserves request order. Viewer state is materialized once;
existing Party/FanProfile/pair/preference and active-credential indexes support
bounded lookups. One content/policy statement, no per-candidate application queries,
no new table/index/cache/backfill, and no derived authorization cache to invalidate.
The bulk predicate is a refinement of the same authoritative activation, pair,
closure and liveness semantics; its generated cases guard against drift.

At READ COMMITTED, the content statement snapshot is the read boundary. A block or
closure committed before it denies; a concurrent change after that boundary may
leave the earlier response valid. Token authority is separately locked and checked
inside the same transaction by the already modeled session mechanism. No claim of
retracting downloaded fields or invalidating independently public media URLs.
The model's progress assumes an available database and weak fairness of Read;
SQL failure/lock failure can still produce terminal HTTP errors. SQL details never
reach the response; deadlock/serialization failures produce retryable 503.

## Executable model and traceability

TLC distribution 1.7.2 (TLC reports 2.17), Java 17; pinned JAR SHA-256
`fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22`.
`ProfileReads.tla`: one viewer, two existing identities, one absent identity, six
bounded unique request sequences, bilateral block abstraction, live/closed sets,
activation latch, mute/discoverability and one terminal observed read. No transport,
media cache, hard-delete cascade, delegate/organization roles or unrestricted ID
cardinality is proved. Identifier boundary behavior is separately tested at 0/100/101.

| Requirement | Property/action | Code mechanism | Automated evidence |
|---|---|---|---|
| PROFILE-01 No unauthorized fields | FieldsAuthorized / Read, Block, Close, Revoke | STABLE eligible join + current-session wrapper | 2,880 checked observations → real PostgreSQL; actual bearer HTTP |
| PROFILE-02 Pause cannot restore rights | FieldsAuthorized / Activate, Pause | Persistent activated_once, pair tombstones and closure | UnsafePause counterexample; paused HTTP and full-schema fixture |
| PROFILE-03 Direct access is distinct from recommendations | DirectReadContract / Mute, Discover | No mute/discoverable/connection gate | UnsafePreferences counterexample; real HTTP preference/no-consent case |
| PROFILE-04 Stable requested order and missing filtering | DirectReadContract / Read | Ordinality + ORDER BY; whole-row exclusion | Mixed batch model cases and HTTP DTO assertions |
| PROFILE-05 Cached projection cannot grant | FieldsAuthorized / Read | Authoritative SQL state; no cached allow | UnsafeCache counterexample; deliberately unsafe SQL policy fails generated case 51 |
| PROFILE-06 Captured token is current | SessionBoundary model / Read | withCurrentSession locks + token identity/active/purpose | Shared session race suite; captured revoked-token profile/empty batch tests |
| PROFILE-07 Eligible operation progresses | Progress / WF(Read) | Available atomic read or terminal HTTP error | TLC temporal-property run; broader database/network availability assumed |

Model result: **20,166 generated / 5,760 distinct states, depth 11**, safety and
progress passed. All three unsafe configurations violated their intended invariant.
The DOT exporter labels guarded transitions `Next`; the generator identifies the
unique false→true `done` transition, which only Read can take, and consumes its
actual `observed.returned` sequence. Empty extraction fails; expectations are not
recomputed from SQL. **2,880** cases generated and passed. The intentionally
permissive SQL policy was rejected at case **51**, then the real policy passed.
This is bounded evidence about this model, not proof of the whole production app.

A first HTTP run had 103 successes and one fixture failure: ordinary UPDATE could
not erase activation memory to represent a fresh installation. The private test
now explicitly resets that trigger-protected latch only during fixture setup.
The production trigger and assertions remain intact. The corrected run passed
**104 examples, zero failures**, including 11 profile examples. Full Stack ran
**2,542 examples, zero failures**; final-source rerun exited 0 and is recorded in [evidence](evidence/profile-read-boundary/results.json). The catalog audit exited 0 after the pinned mobile worktree was initialized (the earlier incomplete checkout correctly reported 97 stale decisions).

## Measured performance (synthetic, not production)

Acceptance fixed before execution: protected SQL p95 **≤50ms**, batches 1/100,
10,005 accounts, degrees 0/10,004, five warmups then 40 alternating paired samples.
The reference query is an equivalent unprotected joined projection, **not** the
full legacy two-query handler. Both projections must return identical ordered
payloads. Native PostgreSQL 16.10 over local loopback; shared developer host with
other builds running. Session/token checks, HTTP, caches and background lag are
excluded; one SQL statement per measured projection independent of batch size.
No additional infrastructure/storage cost beyond two functions; no hosting estimate.

| Actor degree | Batch | Reference p50/p95 ms | Protected p50/p95 ms |
|---:|---:|---:|---:|
| 0 | 1 | 3.55 / 12.22 | 7.99 / 26.66 |
| 0 | 100 | 7.25 / 13.94 | 8.51 / 43.76 |
| 10,004 | 1 | 1.94 / 4.28 | 7.05 / 13.00 |
| 10,004 | 100 | 7.40 / 18.52 | 7.11 / 12.11 |

All final cases pass the declared threshold. The earlier scalar-policy version
failed sparse batch-100 at p95 **76.33ms** (reference 5.65ms), prompting the bulk
query. Host load differed across runs: these are actual observations, not a
controlled estimate of improvement or production capacity. High-degree data is
synthetic and point lookup behavior is not evidence for arbitrary graph traversal.
The first bulk DDL attempt also failed on reserved output name `position`; corrected
to `ordinal` before the successful SQL/HTTP/schema runs. No check was weakened.

## Migration, pause and recovery

1. Keep process/production gates inactive. Apply foundation, read models, DM write
   boundary and chat adapter prerequisites, then the additive profile SQL in one
   transaction on a reviewed non-production fixture first.
2. Install the new API on every serving instance before future activation. Old
   profile readers bypass policy; mixed old/new serving is only a preactivation
   compatibility stage. Do not call coexistence privacy-safe after activation.
3. No profile data is rewritten, reidentified or backfilled. Reapply is idempotent.
   Complete production-schema fixture verifies apply/reapply and pause retains
   blocks, existing messages, commands/publications and eligible profile reads.
4. Pause new social behavior through existing pause procedure while retaining the
   profile reader, activation latch and denial data. Do not restore old readers or
   drop these functions after activation. If code rollback is necessary, retain
   this adapter in the rollback build or disable these two routes; never erase
   post-migration user writes to make rollback appear successful.
5. Before any activation, a failed initial migration rolls back transactionally;
   a never-used additive function can be removed only with a compatible old API
   and no authoritative denial state. Destructive cleanup is a separate review.

Monitor aggregate endpoint status/latency and denied-read rates without logging
profile text, requested IDs, relationship reasons or tokens. Alert on adapter 503s,
reconcile function presence/volatility before routing traffic, and use a synthetic
canary pair to verify block + pause behavior in an authorized test environment.
There is no projection to rebuild. Existing source/graph reconciliation remains
separate. Product outcomes remain the existing collaboration/bookings/sales plan;
this security repair does not claim conversion uplift or add personal analytics.

## Reproduction

```sh
# Set TLA_JAR to the pinned jar and TDF_SOCIAL_JAVA to Java 17 if needed.
bash scripts/social/check-profile-reads-model.sh
java -cp "$TLA_JAR" tlc2.TLC -workers 1 -deadlock \
  -metadir /tmp/profile-states -dump dot,actionlabels /tmp/profile-traces.dot \
  -config formal/social/ProfileReads.cfg formal/social/ProfileReads.tla
python3 scripts/social/generate-profile-read-cases.py /tmp/profile-traces.dot /tmp/profile-cases.sql
diff -u scripts/social/profile-read-model-cases.sql /tmp/profile-cases.sql
TDF_SOCIAL_HTTP_NATIVE=1 bash scripts/social/test-http.sh
bash scripts/social/test-schema-compatibility.sh
TDF_SOCIAL_HTTP_NATIVE=1 TDF_SOCIAL_PROFILE_BENCHMARK=1 bash scripts/social/test-http.sh
(cd tdf-hq && stack test --fast)
npm run audit:catalog-lists
```

`test-http.sh` only accepts its own private database or the explicitly named empty
CI fixture. PostgreSQL 16 native / PostgreSQL 17 container, no shared database.
CI regenerates the checked cases, rejects the unsafe policy and runs the HTTP
fixture through the existing backend job. Hosted results must be recorded against
the actual commit; a configured workflow is not a successful run.

## Acceptance status and remaining scope

- **Satisfied locally:** bounded model/progress/negative controls; generated SQL
  refinement; both endpoint contracts and current bearer boundary; profile HTTP
  tests; complete-schema migration/reapply/preserved-write pause.
- **Satisfied locally:** optimized synthetic SQL threshold in all four workloads.
- **Satisfied locally:** final-source Stack (2,542 examples), catalog audit, shell
  syntax and clean diff checks. Raw [evidence](evidence/profile-read-boundary/results.json).
- **Blocked pending CI:** hosted CI has not run for this branch yet.
- **Blocked overall rollout:** legacy followers/friends/suggestions/search and
  notifications/media can still expose identities elsewhere; delegated entities,
  suspension/deletion integration and all affected native/browser journeys remain
  unqualified. Profile cache withdrawal outside #391 is not completed here.
- **Intentionally deferred:** private-profile audiences, new infrastructure,
  schema cleanup, production experiments, merging/deployment/activation.
- **Prior exception remains open:** provider-triggered Vercel/Cloudflare deployment
  reported in earlier handoff; target/removal unverified. No deployment command was
  issued during this continuation. This PR is not evidence of remediation.
