# Verification register (in progress)

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
eligibility removal, descending one-item pages. No duplicate and no skipped eligible
item above the returned cursor. It assumes publication order is assigned in commit
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
| S-AUTH | AuthoritativeDenial / Read, Finish | authoritative policy at read/delivery snapshot | TLC: 14,987 distinct states; safety + liveness passed |
| S-CONSENT | ConsentIntegrity, OwnConsentOnly / Request, Withdraw(a) | own consent only, unique pair, transactional revoke | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
| S-BLOCK | ConsentIntegrity / Block, Unblock | common row locks with accept/send, no resurrection | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
| S-DELETE | ConsentIntegrity / Delete | tombstone/revision, stale commands rejected | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
| S-RETRY | AtMostOnce, RequestAdmission / Queue(r), Finish(r) | request identity + transaction + terminal dedup | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
| S-PROGRESS | Progress / Finish(r) | bounded retry or terminal rejection; fairness | TLC Progress passed; worker not qualified |
| S-FEED | StablePagination / Publish, Page, Hide | immutable commit-ordered cursor and eligibility before LIMIT | TLC model only; SQL/HTTP/legacy refinement is not included in this foundation |
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
Final runs: Relationships 19,377 generated / 14,987 distinct states, depth 12;
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

These bounded model results do not qualify SQL/HTTP/legacy implementations.
Downstream #356 must regenerate/review its transition-derived SQL assertions against
this corrected actor-specific withdrawal model before claiming model refinement.
