# Verification register — 2026-09-15 (partial delivery)

## Formal scope and limits

`formal/social/Relationships.tla`: two distinct principals, version bounds 4
(positive) and 3 (negative), explicit bilateral consent, unilateral blocks,
revocation of club membership, irreversible actor deletion, stale cache, queued
requests with separate immutable authorization revisions, idempotent terminal delivery. Operations are atomic database transactions;
trusted handlers supply authenticated actor identity. Fair scheduling of Finish(r)
and eventual database availability are required for Progress. No external network
exactly-once claim. Role grant creation, token rotation, multi-organization
impersonation, media/CDN copies and crash recovery of an external delivery are not
modeled. These require separate refinement evidence before rollout.

`formal/social/Feed.tla`: five monotonically published immutable positions,
eligibility removal, descending one-item pages. Start captures a traversal high-water
mark before the first page. No duplicate or skipped eligible item at or below that
mark and above the returned cursor; later publications belong to the next traversal. It assumes publication order is assigned in commit
order; ordinary PostgreSQL sequences alone do **not** establish this assumption.
If an implementation uses preallocated IDs or editable dates, it does not refine
this model. Eligibility becoming newly allowed after a cursor passes is outside
this no-gap guarantee; refresh begins a new traversal.

The negative stale-cache configuration must discover AuthoritativeDenial violation:
request A, request B, cache consent, block/revoke/delete, read via stale authority.
This is a mutation of the model used to validate its ability to detect a defect;
it is not evidence that this precise sequence was executed in production.

## Traceability and acceptance

| Requirement | Property/action | Required implementation mechanism | Current evidence |
|---|---|---|---|
| S-AUTH | AuthoritativeDenial, DeliveryAuthority / Read, Finish | authoritative policy at read/delivery snapshot | TLC: 101,855 distinct states; observable read safety + availability and liveness passed |
| S-CONSENT | ConsentIntegrity, OwnConsentOnly / Request(a, revision), Withdraw(a, revision) | own consent only, unique pair, transactional revoke | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
| S-BLOCK | ConsentIntegrity, NoConsentResurrection, BlockOwnership / Block, Unblock | common row locks with accept/send, no resurrection | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
| S-DELETE | ConsentIntegrity / Delete | tombstone/revision, stale commands rejected | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
| S-RETRY | AtMostOnce, RequestAdmission, ReplayEquality, BindingImmutable / Queue, Finish, Submit | request identity + immutable recipient/payload equality + transaction + terminal dedup | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
| S-PROGRESS | Progress / Finish(r) | bounded retry or terminal rejection; fairness | TLC Progress passed; worker not qualified |
| S-FEED | StablePagination, CompleteAtEnd / Start, Publish, Page, Hide | traversal high-water mark + immutable commit-ordered cursor and eligibility before LIMIT | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
| S-CLIENT | account isolation/accessibility | scoped query keys, selectors, recovery states | baseline 44 tests passed |

Baseline tests/builds are evidence of the starting point only. See timestamped logs;
no unrun test, migration, screenshot, CI or performance result is counted as passed.

## Performance acceptance (set before measurement)

Synthetic PostgreSQL 16, one local container: sparse degree 0/1/5 and hub degree
10,000, 100,000 total relationships, page size <=50. Target p95 <=200ms warm DB
query time for following/discover, <=300ms mutation outside intentional lock waits,
bounded SQL query count independent of page size, zero unauthorized rows/duplicates.
Report hardware and distributions; no production SLO claim. Complete-schema
migration and rollback must preserve writes created after migration. Storage and
cost must be measured/estimated separately; no new-service operating cost assumed.

## Executed TLC results before review repair

Java 17.0.12; tla2tools-1.7.2.jar SHA256
`fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22`.
Final runs: Relationships 19,377 generated / 5,060 distinct states, depth 12;
Feed 2,901 generated / 1,009 distinct states, depth 16. No invariant/liveness error.
Negative StaleCache: expected AuthoritativeDenial violation, depth 6. Logs under
`evidence/models/`. Earlier attempts exposed an omitted terminal feed stutter and
unparenthesized boolean assignments; both were corrected before these final runs.
Tool invocation asserts the specific expected invariant failure, not any nonzero exit.

Reproduce: set `TLA_JAR` and optionally `TDF_SOCIAL_JAVA`, then
`bash scripts/social/check-models.sh`. Obtain the pinned jar from
https://github.com/tlaplus/tlaplus/releases/tag/v1.7.2 and verify the checksum above.

## Review repair: independent consent and request identity (2026-09-15)

Withdrawal now removes only the authenticated actor's intent. Block and deletion
still clear the relationship consent explicitly as policy requires. OwnConsentOnly
records whether a withdrawal ever changes another principal's consent.

The model has two distinct request IDs independent of the authorization revision.
Queue records the revision per request; Finish rechecks current authority and that
revision, then reaches an idempotent delivered/rejected terminal result. Distinct
requests can progress at the same revision, including after another has finished;
reusing a pending/terminal request ID cannot create a second delivery. AtMostOnce
checks per-request delivery counts; RequestAdmission detects accidental suppression
of distinct requests by revision-based deduplication. Progress assumes weak fairness
of Finish for each request, not fairness of unsubmitted user intentions.

Rechecked with Java 21 and the same pinned TLC jar: Relationships 55,530 generated /
14,987 distinct states, depth 10; Feed 2,901 / 1,009, depth 16. Both safety and
liveness checks passed. Three independently configured negative controls detected
exactly AuthoritativeDenial, OwnConsentOnly and RequestAdmission respectively.
The runner rejects unexpected tool errors as evidence of a counterexample.
Repaired-run logs are committed under `docs/social/model-evidence-2026-09-15/`; the
pre-repair logs above retain their original 5,060-state historical result.

These bounded model results do not qualify SQL/HTTP/legacy implementations.
Downstream #356 must regenerate/review its transition-derived SQL assertions against
this corrected actor-specific withdrawal model before claiming model refinement.

## Second review repair: reads, traversal bounds, and replay parameters

Read now records an observable grant/denial and the authority snapshot used for
that decision. AuthoritativeDenial independently checks consent, blocks, actor
liveness, membership and privacy in that snapshot. AuthorizedReadAvailable also
rejects an implementation that always denies permitted reads. Later revocations
do not retroactively invalidate a read already authorized at its transaction.

Feed Start captures the publication high-water mark for one traversal, including
an empty initial traversal. Publications arriving above that mark are deferred
to a refresh. StablePagination checks all currently eligible positions within the
mark above the cursor independently of Candidates; CompleteAtEnd checks that an
exhausted traversal has returned every still-eligible position in its boundary.
This is a required refinement contract for a future API, not a claim that current
clients already transport a high-water mark.

RequestReplay separately explores two IDs, two recipients and two payloads.
Submit binds the full parameter record on first use, acknowledges only an equal
retry, and rejects conflicting reuse while pending or terminal. BindingImmutable
checks that accepted identity bindings never change. Finish permits at most one
effect per ID. This protocol model complements Relationships' authorization and
revision checks; composition with SQL/HTTP workers still requires refinement tests.

The complete runner passed three positive configurations and five specific
negative controls. Relationships: 348,055 generated / 102,833 distinct, depth 11.
RequestReplay: 2,384 generated / 272 distinct, depth 6. Current logs for all models
and counterexamples are committed under
`docs/social/model-evidence-second-review-2026-09-15/`. Earlier result sections
remain historical evidence of their explicitly described model versions.

## Active actor follow-up

Block and Unblock now require the authenticated actor to remain alive.
InactiveActorCannotMutate checks that neither command is enabled for a deleted
principal. The DeletedActor negative control removes that guard and must violate
this exact invariant. Current complete run: Relationships 339,559 generated /
101,245 distinct states, depth 11; Feed 4,717 / 1,674, depth 17; RequestReplay
2,384 / 272, depth 6. All three positive configurations and six specific negative
controls passed. Logs: `docs/social/model-evidence-active-actor-2026-09-15/`.

## Tombstones and stale relationship commands

Request, Withdraw, Block and Unblock now carry an explicit expected revision and
can mutate only at the current revision. Block and Unblock require both principals
to remain live; a surviving principal cannot modify a tombstoned pair. The model
independently checks that all four commands are disabled against deleted targets
and for every earlier revision. This covers delayed relationship commands as well
as Queue/Finish delivery revisions; no resurrection or revision reset is modeled.
Membership revocation, privacy changes and deletion remain external authoritative
policy transitions, not user relationship commands.

Three positive models and eight specific negative controls passed. Relationships:
332,969 generated / 100,031 distinct states, depth 11. Feed and RequestReplay retain
4,717 / 1,674 and 2,384 / 272 respectively. DeletedTarget and StaleCommand must fail
DeletedTargetCannotMutate and StaleRelationshipCommandsDenied respectively.
Exact logs: `docs/social/model-evidence-tombstone-2026-09-15/`. SQL/HTTP refinement,
including propagation of expected revisions through legacy clients, remains a
separate activation requirement for downstream implementations.

## Independent delivery and ownership guarantees

Finish records the delivery-time revision and authority snapshot separately for
every delivered request. DeliveryAuthority independently requires the queued
revision to match that snapshot, bilateral consent, live principals, no block,
and the relevant membership/privacy policy. A StaleDelivery mutant bypasses the
Finish authorization guard and must fail this invariant after authority changes.
A later revoke does not retroactively invalidate a delivery already authorized at
its transaction boundary.

OwnConsentOnly now records both Request and Withdraw preserving the other
principal's intent. RequestOwnership attempts to synthesize the peer's consent
and must fail. NoConsentResurrection is an action property requiring Unblock to
preserve the pre-command consent set. BlockOwnership separately requires it to
preserve all other principals' blocks. Their negative configurations restore
consent or clear all blocks and must violate the named action property.

Final complete run: three positive configurations and twelve specific negative
controls passed. Relationships: 337,265 generated / 101,855 distinct, depth 11;
Feed: 4,717 / 1,674, depth 17; RequestReplay: 2,384 / 272, depth 6. Exact logs:
`docs/social/model-evidence-independent-guarantees-2026-09-15/`. These are bounded
model results; downstream SQL, HTTP and legacy-client refinement remains required.
## Downstream SQL refinement after concurrent review

The authority branch is rebased onto the reviewed audit commit `3151106f2` plus
preview suppression `45aa887ff`; unrelated application branches were not merged.
`ConsentTraces` now invokes actor-specific Withdraw and checks OwnConsentOnly.
The generator derives the withdrawing actor from the removed consent, instead of
always assuming actor A. TLC explored 32 trace states / 74 generated states and
emitted 30 distinct SQL assertions.

Running those assertions against the previous SQL failed at **model state 23**:
with both intents true, actor A's disconnect erased B's intent. The repair clears
only the caller's intent; block and social closure still clear both. The same
native PostgreSQL fixture then passed all 30 transitions and the existing checks.
Before/after logs are retained in `evidence/reviewed-sql-before.txt` and
`evidence/reviewed-sql-after.txt`. This is an observed model-to-code counterexample,
not merely a synthetic negative configuration of the model.

The reviewed positive models were rerun here using Java 17.0.12 and the pinned TLC
jar: Relationships 101,245 distinct states, Feed 1,674, RequestReplay 272. The runner
also requires the six specifically named negative-control violations. These remain
bounded specifications; external worker and full-platform refinement is unfinished.

For the descending feed, the first response establishes an implicit traversal
high-water mark. A later publication position is greater than that mark and also
greater than the returned last-position cursor, so it cannot enter later pages.
An empty response terminates the traversal; a new first-page request is a refresh.
The implementation does not transport the exact initial mark. This mapping covers
monotonic publication and eligibility removal only; newly eligible old content and
an explicit resumable traversal token require further qualification.

The subsequent audit head `6d5c25c20` adds independent delivery, tombstone, stale
command and ownership properties. ConsentTraces passes the current revision to
all four commands and supplies every negative-mode constant explicitly. Its 32
states / 30 generated transition cases passed again against native PostgreSQL;
see `evidence/consent-independent-review.txt` and `evidence/sql-independent-review.txt`.
The SQL byte sequence of the generated assertions did not change. Delivery remains
a model-level guarantee until the legacy worker integration is implemented.
## Implementation refinement actually executed

| Requirement | Model action / property | Code boundary | Automated evidence and scope |
|---|---|---|---|
| S-AUTH | Read / AuthoritativeDenial | `social_v2_feed`, `social_v2_relationship` and read functions marked STABLE; `Social.Server` derives actor from auth | `read-model-tests.sql`: membership removal, blocked GET and snapshot classification; `HttpSpec.hs`: real token/actor/organization denial |
| S-CONSENT | Request, Withdraw / ConsentIntegrity | `social_v2_mutate`, ordered pair primary key and independent consent columns | 30 TLC graph-derived transitions in `model-cases.sql`, request/accept fixtures; HTTP explicit acceptance and injected-actor rejection |
| S-BLOCK | Block, Unblock / ConsentIntegrity | ordered actor/credential/pair locks, block constraint, denial before replay | `test-postgres.sh`: both block and accept observed waiting at an explicit barrier; blocked reader cannot poll revision |
| S-DELETE | Delete / ConsentIntegrity | `social_v2_close` tombstone and preserved revisions | social closure + stale replay tests; NOT full account erasure/refinement |
| S-RETRY | Queue, Finish / TypeOK | unique actor/request key, payload equality, current authorization and version check | directed follow/unfollow retries, stale revision, payload conflict, database constraints and rate limit; external worker/delivery not refined |
| S-FEED | Publish, Page, Hide / StablePagination | serialized publication batch commits before read, immutable unique position, membership before page limit | SQL ties, edit, late insert, deletion and revocation; 50k-post synthetic workload; HTTP page/cursor limits |
| S-REACTION | SetActive / RetrySafe | `toggleMomentReactionDb` row lock and atomic evidence insert | Reaction TLC 15 states; historical candidate backend test passed in the 2,540-example run; refreshed upstream helper requires separate CI qualification |
| S-PROGRESS | Finish / Progress | model fairness/availability assumptions | model passed; existing notification/reindexing worker progress has NOT been implemented or qualified by this work |

Eight HTTP examples passed on native PostgreSQL 16.10 with actual Servant and bearer
authentication, object-compiled through Stack GHC 9.10.3. The Docker-backed HTTP
attempt stalled after its API became unavailable (observed API 500); it was stopped
and is not a pass. The interpreter attempt hit GHC's bytecode breakpoint-index limit.
The native fixture starts/stops a private cluster. See `evidence/http-refreshed-final.txt`.

The pre-refresh Stack-built complete backend test binary ran **2,540 examples, zero failures**.
The Stack command itself failed afterward trying to copy an unbuilt executable;
these outcomes are distinct. CI #365 independently linked the application and passed
its backend tests, then failed on the existing merch-migration prerequisite.

The refreshed dependency includes upstream onboarding repairs. Full web type
checking, Vite build and bundle budget now pass (`evidence/ui-build-refreshed.txt`);
this supersedes the historical baseline failures without attributing upstream
repairs to the social implementation. Fifteen focused social tests passed again
(`evidence/ui-refreshed-final.txt`); scoped ESLint passed before this model-only refresh. The local browser uses
synthetic API/session fixtures; it does not establish full-app authenticated E2E or
mobile runtime behavior. Screenshot and axe evidence is committed.

## Final independent model rerun (2026-09-15)

After rebasing onto audit `6d5c25c20`, the pinned Java 17/TLC runner passed **all four
positive configurations and all twelve specifically expected negative controls**.
Relationships explored 337,265 generated / 101,855 distinct states, depth 11; Feed
1,674 distinct; RequestReplay 272; Reaction 15. Logs are in
`evidence/independent-final-models/` with the command summary in
`evidence/independent-final-run.txt`. The “Error” lines in that summary are required
counterexamples for deliberately unsafe configurations, not ignored test failures.
The runner exited zero only after matching each exact invariant/action-property.
The consent adapter also passed and regenerated byte-identical 30 SQL assertions.

The workflow guard suite passed 12 tests (`evidence/workflow-refreshed-final.txt`).
Final browser artifact uploads now use a fresh temporary directory, so an early CI
failure cannot upload old committed local screenshots as if the job produced them.


## Complete schema qualification

The separate compatibility harness restored the schema-only repository baseline,
applied its synthetic catalog fixture and all 102 registered migrations, then
passed the social migration, publication/legacy-source-write and preserved-data
pause assertions on native PostgreSQL 16.10. The final script passed again; see
`evidence/schema-compatibility-final.txt` and [scope/commands](schema-compatibility.md).
No assertions or baseline schema objects were removed to obtain this result.


## Refreshed backend tests

The newly Stack-linked test binary ran 2,542 examples with zero failures on the
refreshed application source (exit 0). This is independent of the still-running
application executable compilation. Command, hashes and final output are recorded
in `evidence/backend-refreshed-result.txt`; the earlier 2,540-example result remains
historical. Full-app runtime and mobile social flows still require qualification.
