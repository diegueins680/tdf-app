# Legacy relationship read boundary — 2026-09-16

Dependency: [profile PR #397](https://github.com/diegueins680/tdf-app/pull/397),
`7c452efcbc7b45e797b7ebd590dfffccf0fc9751`; that PR depends on #390. This is an
additive compatibility repair of four GET routes. No production activation,
deployment, relationship backfill or consent conversion.

## Audit findings and response

| Surface | Source and defect | Decision and acceptance |
|---|---|---|
| `/social/followers`, `/following` | `Server.socialListFollowers`/`socialListFollowing` read PartyFollow then Party names without the canonical policy | **Repair:** whole-edge eligibility before names/metadata, same DTO, direction and historical timestamps |
| `/social/friends` | `socialListFriends` interprets reciprocal legacy follows as a list; one writer can manufacture both directions | **Compatibility repair:** preserve the historical list, filter denied identities; explicitly no accepted-connection or DM authority |
| `/social/suggestions` | `socialListSuggestedFriends` follows two hops through legacy edges and returns `sfMutualCount` without disclosure consent | **Retire after enforcement starts:** empty array; reuse the separate canonical Discover view for recommendations. Preserve old inference only in the never-activated/no-canonical-state compatibility stage |
| Old relationship writers | `socialAddFriend`, `vcardExchange`, `socialRemoveFriend` and their response DTOs remain outside this change | **Incomplete follow-up:** consent-preserving writer adapter and response authorization; broad rollout remains blocked |
| Web/native consumers | `tdf-hq-ui/src/api/social.ts`, legacy `SocialPage`, `tdf-mobile/src/api/social.ts`, native social tab | Existing URLs/arrays/DTOs retained. Empty states already supported. No client may treat a historical mutual-follow row as canonical acceptance. Old UI migration and cache withdrawal remain separate |

PartyFollow remains authoritative for historical directed edges/NFC provenance,
Party for names, canonical pair/preference/credential rows for denial, ApiToken for
current bearer identity. No historical edge, reciprocal link, shared membership,
Admin role or purchase grants canonical communication rights.

## Policy and compatibility

- No foundation: original handlers remain available for old installations.
- Foundation but incomplete reader migration: all four routes return 503, even
  before activation; do not race the first block with a legacy fallback.
- Repaired routes revalidate current bearer identity in the domain transaction.
  Anonymous or captured revoked tokens return 401, including empty/retired lists.
- Lists use the same profile eligibility as #397: pair-specific enforcement before
  activation; persistent global enforcement after activation; closure always wins.
  Block in either direction removes the whole relationship row. No names, NFC
  provenance, date or row count for the denied identity is returned.
- Mute and Discover opt-out do not independently erase the owner's direct lists.
  Organization/delegated identities remain excluded by the activated account-only
  pilot. This is not a new organization authorization policy.
- Self-links and dangling references are excluded from repaired projections;
  historical rows are not deleted. Surviving reverse edges do not remain mutual
  after one direction is deleted. Unblocking may reveal a historical edge again;
  it never recreates canonical follow/connection consent.
- Legacy suggestions are enabled only while `activated_once=false`, the entire
  canonical pair table is empty, and no canonical closure exists. Missing runtime
  state fails closed. Once any canonical state exists, inference returns `[]`, even
  during pause. This conservative global transition avoids leaking third-party
  seed relationships or counts that cannot be explained safely with the old DTO.
- Do not substitute zero "mutual" counts on unrelated Discover candidates: that
  would misuse the old UI's reason label. No new source signals or audience consent
  are invented. Discover remains the recommendation product, behind existing gates.

## Research to decision

Primary sources accessed 2026-09-15; implementation packet completed 2026-09-16.

| Problem | Evidence/date | Project-specific choice and rejected alternative | Validation |
|---|---|---|---|
| Relationship existence does not imply permission to disclose it | [ActivityPub followers/following collections](https://www.w3.org/TR/2018/REC-activitypub-20180123/#followers-collection), W3C Recommendation 2018-01-23, permits access restrictions | Preserve directed history separately from disclosure/communication authority; retire unconsented aggregate explanations. The specification does not prescribe TDF's retirement rule. Reject treating reciprocal imports as bilateral consent. | NoPrivateCounts, actual preactivation/activation/pause HTTP cases; existing canonical consent models remain separate |
| Repeated candidate policy work | [PostgreSQL 16 WITH materialization](https://www.postgresql.org/docs/16/queries-with.html), official versioned docs, page update unavailable | Materialize the actor's legacy edge set, call existing bulk eligibility once, join metadata afterward. Reject a graph engine or application per-edge queries. | Paired SQL benchmark and generated projection cases |
| Concurrent read/revoke/delete | [PostgreSQL 16 function volatility](https://www.postgresql.org/docs/16/xfunc-volatility.html), previously researched official versioned docs | STABLE read functions share statement snapshot; current-session locks are reused. Reject stale edge snapshots or preflight permission checks followed by independent payload reads. | Delete/Block/Revoke model actions and unsafe controls; exact shared HTTP adapter |

## Mechanisms and guarantees

`TDF.API.SocialRelationships` shares the exact existing Servant contracts with the
production router and HTTP harness. `TDF.Social.RelationshipReads` supplies four
adapters; all use `withCurrentSession ReadSession`. SQL errors are sanitized;
deadlock/serialization failures are retryable 503s. No actor supplied by the client.

The additive SQL creates one reverse legacy-edge index and three STABLE functions:
`social_v2_relationship_rows`, `social_v2_legacy_suggestions_enabled`, and
`social_v2_legacy_suggestions`. Eligibility is delegated to #397's shared bulk
profile policy. Metadata is read within the same calling statement snapshot.
Lists use descending `(created_at,id)` for deterministic ties. Dates explicitly
use UTC, preserving Haskell's old `utctDay` conversion despite session timezone.

The deployment assumption is PostgreSQL READ COMMITTED (the existing pool default).
A denial committed before the content statement snapshot takes effect immediately
there. An overlapping read may validly return the earlier snapshot. Later mutations
cannot retract bytes already delivered. Token locks/checks remain separately
qualified by SessionBoundary. No new worker/outbox/cache or reconciliation stream;
source rows are read directly. Catalog assertions verify all three read functions are STABLE.
Generic SQL projection checks supplement, not replace,
the real bearer HTTP tests and the full compiled production-router build.

The historical list DTO has no cursor. Preserving it means O(degree) response and
memory growth; it is **not suitable for unlimited-degree rollout**. No truncation
or invented cursor was silently added. Legacy two-hop suggestion inference is
also retained without a new fanout cap only before enforcement; it is retired after
the transition above. Paginated replacement contracts and old-client cutover remain
explicit scale work. No database/infrastructure change beyond the index is justified.

## Formal scope and traceability

TLC distribution 1.7.2, Java 17; pinned JAR SHA-256
`fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22`.
`RelationshipReads.tla` has one viewer, three identities, four bounded source graphs,
four routes, pair-specific block, activation memory, closure/credential changes for
two identities, source edge deletion and one terminal observed read. Directed edges
are sets; existing database uniqueness prevents duplicates. It does not model
unbounded actors, timestamps, role delegation, legacy mutations or client caches.
The two-seed count and UTC/tie-order cases are additional implementation fixtures.

| Requirement | Model property/actions | Mechanism | Test |
|---|---|---|---|
| RELREAD-01 Denied identities expose no row fields | RowsAuthorized, Read/Block/Close/Revoke | Shared authoritative eligible join | Generated PostgreSQL cases; both block directions and Admin HTTP |
| RELREAD-02 Old inferred counts stop at enforcement | NoPrivateCounts + ExactProjection, Activate/Pause/Block/Close | STABLE global retirement predicate | Generated count observations; activation/pause and unrelated-pair/closure HTTP |
| RELREAD-03 Deletion cannot be undone by stale reads | RowsAuthorized + ExactProjection, Delete/Read | Current source table, same statement snapshot | UnsafeDeleted counterexample; generated subsets and deletion HTTP |
| RELREAD-04 Direction/reciprocity/order are preserved | ExactProjection, Read/Delete | Direction selectors, reciprocal EXISTS, deterministic order | Generated edge sets plus UTC/NFC/tie-order/self-link HTTP/SQL fixtures |
| RELREAD-05 Current authenticated principal | Existing SessionBoundary properties | withCurrentSession in operation transaction | Actual anonymous/captured-token HTTP on all four routes; shared session race suite |
| RELREAD-06 Read eventually terminates | Progress, weak fairness of Read | Available atomic read or terminal HTTP error | TLC temporal check; database/network availability assumed |

Positive run: **76,624 generated / 24,192 distinct states, depth 13**, safety and
progress passed. Three unsafe variants each violate their intended property:
policy bypass, private counts and stale deleted edges. The generator consumes the
actual terminal observed row set/count (only Read changes `done`), not a duplicate
implementation of eligibility. It produced **8,064** distinct SQL cases; all passed.
SQL controls that bypass eligibility and keep inference enabled fail at cases
**229** and **272**, respectively. These expected failures are retained as evidence.

Actual local verification: full Stack **2,542 examples, zero failures**; complete
production-schema fixture apply/reapply/pause passed on PostgreSQL 17; catalog audit
exit 0. Final bearer HTTP suite: **116 examples, zero failures**.
[Actual logs and source fingerprints](evidence/relationship-read-boundary/README.md)
include the final self-link and cross-timezone assertions. A bounded model
check is evidence for this stated model, not a proof of the complete platform.

## Performance and failed fixture

Acceptance declared before measurement: protected SQL p95 ≤250ms at degree 100 and
≤1500ms at degree 10,004; 10,005 synthetic accounts; three warmups and 20 alternating
paired samples per collection. Native PostgreSQL 16.10, local loopback/shared host.
Reference is an equivalent unprotected joined projection, not the old full handler.
Both outputs must be identical in rows, order and metadata. HTTP/session overhead,
production concurrency and hosting cost are excluded. Each measured projection is
one statement; no application query per row.

| Degree | Collection | Reference p50/p95 ms | Protected p50/p95 ms |
|---:|---|---:|---:|
| 100 | Following | 1.64 / 2.52 | 2.95 / 3.63 |
| 100 | Followers | 1.65 / 2.08 | 2.89 / 3.43 |
| 100 | Friends | 1.98 / 2.22 | 3.00 / 3.45 |
| 10,004 | Following | 126.99 / 146.65 | 200.26 / 255.01 |
| 10,004 | Followers | 122.98 / 139.77 | 193.69 / 216.22 |
| 10,004 | Friends | 130.08 / 137.67 | 201.16 / 214.10 |

All thresholds passed. The first benchmark correctly rejected a row mismatch:
the last HTTP denial test left account 2 as an organization, so the guarded read
excluded it while the reference did not. The benchmark now resets account types
before measuring the all-eligible workload; no assertion or threshold changed.
This is fixture correction, not evidence that organization exclusions were removed.

## Migration, compatibility, pause and operations

1. Apply #397 prerequisites, then `2026-09-15_social_v2_relationship_reads.sql` on
   an authorized non-production fixture. The transaction is additive/idempotent;
   no stored relationships, identity, consent or timestamps are rewritten.
2. The incoming index build takes a write-conflicting lock. Before a future large
   deployment, measure its duration and prepare the same index concurrently outside
   the migration transaction if needed; no production lock-time claim from this
   fixture. No provisioning/deployment occurs in this task.
3. Install this API on every serving instance and drain old requests before future
   activation. Old readers/writer response paths remain unsafe; coexistence with
   old binaries is only a never-activated compatibility stage.
4. Rollback pauses new social behavior while retaining these readers, their index,
   activation latch and canonical denial data. Never drop enforcement or restore
   old readers after activation. If a rollback build cannot retain the adapters,
   disable these routes until repaired; preserve all post-migration source writes.
5. The complete-schema fixture inserts a legacy edge after migration, reapplies,
   pauses, asserts that the stored edge survives and its blocked projection stays
   absent. Retired suggestions remain empty. No destructive down migration/backfill.
6. Monitor aggregate endpoint latency/503s and response cardinality without logging
   identities, edges, mutual-count reasons or tokens. Verify function presence and
   STABLE classification before routing traffic. No derived view requires rebuild;
   existing canonical/source reconciliation remains the owner of orphan/duplicate
   detection. Do not remove historical rows as an automatic repair.

Product measurement stays the established consented collaborations/bookings/sales
plan with block/report/latency guardrails. This repair adds no personal analytics,
private signal imports or conversion claims. New production gates stay inactive.

## Reproduce and acceptance status

```sh
bash scripts/social/check-relationship-reads-model.sh # TLA_JAR / Java 17 required
java -cp "$TLA_JAR" tlc2.TLC -workers 1 -deadlock \
  -metadir /tmp/relationship-states -dump dot,actionlabels /tmp/relationship-traces.dot \
  -config formal/social/RelationshipReads.cfg formal/social/RelationshipReads.tla
python3 scripts/social/generate-relationship-read-cases.py /tmp/relationship-traces.dot /tmp/relationship-cases.sql
diff -u scripts/social/relationship-read-model-cases.sql /tmp/relationship-cases.sql
TDF_SOCIAL_HTTP_NATIVE=1 bash scripts/social/test-http.sh
bash scripts/social/test-schema-compatibility.sh
TDF_SOCIAL_HTTP_NATIVE=1 TDF_SOCIAL_RELATIONSHIP_BENCHMARK=1 bash scripts/social/test-http.sh
(cd tdf-hq && stack test --fast)
npm run audit:catalog-lists
```

- **Satisfied locally:** executable model/progress/negative controls, generated SQL
  refinement, read adapters with unchanged DTOs, full build/tests, schema migration
  and preserved-write pause, declared synthetic performance bounds, catalog audit.
- **Satisfied locally:** final 116-example bearer HTTP suite.
- **Pending:** hosted CI for this branch; no prior PR result substitutes for it.
- **Incomplete:** legacy friend/vCard mutations and their response authorization,
  old-client semantics/cache withdrawal, search/media/notifications, delegated
  entities, full lifecycle/moderation integration and outcome instrumentation.
- **Intentionally deferred:** paginated versioned legacy-list replacement in this
  PR; needed before high-degree rollout. Graph infrastructure, irreversible cleanup,
  merges, deployments and production activation are also deferred.
- **Prior exception:** historical provider-triggered deployment target/removal remains
  unverified; no deployment command was issued here. Broad rollout remains blocked.
