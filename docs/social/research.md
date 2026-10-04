# Research-to-decision matrix

Accessed 2026-09-14. Primary sources only. **Finding** summarizes the source;
**TDF choice** is a project inference. No production-scale benefit is claimed.

| Problem | Primary evidence; publication/update | Candidates / TDF choice / rejected alternatives | Expected benefit and validation |
|---|---|---|---|
| Graph storage | [TAO](https://engineering.fb.com/2013/06/25/core-infra/tao-the-power-of-the-graph/), 2013-06-25: graph abstractions backed by relational storage and caches at Facebook scale | Explicit typed edges in PostgreSQL. Reject provisioning TAO/Neo4j without demonstrated traversal workload. TAO's cache architecture addresses a scale not established here. | Constraints and bounded indexed queries; synthetic sparse/high-degree benchmarks, then representative measurements. |
| Transactional graph updates | [RAMP-TAO](https://engineering.fb.com/2021/08/18/core-infra/ramp-tao/), 2021-08-18; [PostgreSQL 16 isolation](https://www.postgresql.org/docs/16/transaction-iso.html), versioned docs, update date unavailable | Prefer existing PostgreSQL transactions and consistent lock ordering; retries must handle conflicts. Do not reproduce a distributed transaction protocol. | Atomic consent/block effects; real two-session races and model traces. |
| Consent and semantics | [ActivityPub](https://www.w3.org/TR/2018/REC-activitypub-20180123/), W3C Recommendation 2018-01-23: Follow and Accept are distinct | Separate directed following from bilateral connection consent and scoped membership. No federation is implied. Reject auto-follow from shared club or static vCard. | Unilateral request cannot authorize DM; request/accept/block state-machine tests. |
| Privacy and caches | [Zanzibar](https://www.usenix.org/conference/atc19/presentation/pang), USENIX ATC July 2019: authorization must respect causal changes | Shared policy query on authoritative storage; revalidate protected reads and work. Reject using stale projected relationship data as authority. No Google-scale external auth service. | Denied read/delivery after boundary; stale-cache negative TLC configuration. |
| Blocking | [ActivityPub §6.9](https://www.w3.org/TR/activitypub/#block-activity-outbox), 2018-01-23 | Directed block with bidirectional interaction exclusion; mute remains viewer-only preference. Block does not retract independently public content or downloaded copies. | No private relationship reasons, counts or queued delivery to blocked principals. The Bluesky block article redirected to an empty browser result; not used as evidence. |
| Feed pagination | [PostgreSQL 16 indexes/order](https://www.postgresql.org/docs/16/indexes-ordering.html) and [row comparison](https://www.postgresql.org/docs/16/functions-comparisons.html); versioned docs, dates unavailable | Keyset cursor over immutable publication order and unique tie-breaker; eligibility before LIMIT. Reject OFFSET for changing feeds and editable timestamps as cursors. | No duplicates or avoidable gaps within stated snapshot/commit assumptions; TLC plus concurrent insert/delete fixtures. |
| Sparse discovery | [Rules of ML](https://developers.google.com/machine-learning/guides/rules-of-ml), Martin Zinkevich, current page accessed, publication not stated | Start with public declared interests and useful nonpersonalized fallback. Reject ML without data; reject sprawling heuristics too: the source recommends moving beyond complex heuristics once evidence supports it. | Compare to uniform/chronological baseline; dismissal, opt-out, smaller-artist exposure and sparse fixtures. No conversion uplift claimed. |
| Retries/delivery | [AWS Builders' Library: idempotent APIs](https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/), Malcolm Featonby; PDF copyright 2020 | Stable request identity + parameter equality, transactional result recording, bounded retry. Reauthorize replay; idempotency cannot restore revoked access. No new broker. | Duplicate/out-of-order processing tests and terminal progress under fairness/availability assumptions. |
| Formal toolset | [Lamport: high-level TLA+](https://lamport.azurewebsites.net/tla/high-level-view.html), updated 2021-08-10 | TLA+/TLC 1.7.2 distribution (TLC identifies itself 2.17); available Java 17. Finite models before implementation, with explicit bounds and liveness assumptions. Reject calling prose/typechecking proof. | Reproducible safety/liveness runs plus an intentionally unsafe stale-cache variant that must fail. |

## Product and architecture decisions

Following is the default reading mode; Discover is separately labeled. Optimize
for accepted relevant connections and consented downstream outcomes, with blocks,
reports, latency, opt-outs and exposure concentration as guardrails. Those are
product hypotheses, not research-demonstrated gains for TDF. A post-release study
must distinguish a social click/lead proxy from a completed booking or paid sale.
No experiments or new services are provisioned in this task.

### Read-boundary refinement (accessed 2026-09-15)

[PostgreSQL 16 function volatility](https://www.postgresql.org/docs/16/xfunc-volatility.html)
(versioned official documentation; page update date unavailable) specifies that
STABLE functions use the calling query's snapshot; VOLATILE functions obtain new
snapshots for their internal queries. TDF inference: mark read-only social functions
STABLE so a sequence of policy/response SELECTs shares the model's single read
boundary. Reject IMMUTABLE for database-backed policy because cached plans could
retain an obsolete value. Mutations remain VOLATILE. Validation: PostgreSQL fixture
asserts function classification and exercises revocation between calls; this does
not prove arbitrary application code follows the same transaction discipline.

### Session revocation refinement (accessed 2026-09-15)

| Problem | Primary evidence; publication/update | Selected / rejected alternatives | Expected benefit and validation |
|---|---|---|---|
| Token revoked after authentication | [PostgreSQL 16 explicit locks](https://www.postgresql.org/docs/16/explicit-locking.html), versioned official docs, update unavailable | TDF inference: retain internal token ID; account/credential/token row locks and current token checks in the domain transaction. Reject auth-time-only checks or a process cache as current authority. | Observed old-handler 200 after revocation becomes 401; bounded model, 64 generated outcomes, real lock races, paired overhead benchmark. See [session boundary](session-boundary.md) for assumptions and actual results. |

### Legacy messaging compatibility (accessed 2026-09-15)

| Problem | Primary evidence; publication/update | Selected / rejected alternatives | Expected benefit and validation |
|---|---|---|---|
| Older DM writers bypass new pair policy; pausing loses enforcement | [PostgreSQL 16 triggers](https://www.postgresql.org/docs/16/trigger-definition.html) and [isolation](https://www.postgresql.org/docs/16/transaction-iso.html), official versioned docs, update unavailable | TDF inference: additive existing-table write trigger, ordered current-authority checks and retained activation memory. Reject flag-off legacy fallback and relying only on a new endpoint. Require READ COMMITTED explicitly. | Real old-INSERT counterexample; 27 checked-model outcomes; block/send races; complete-schema pause preserves messages; fixture INSERT overhead. Legacy readers and HTTP error mapping remain blockers. See [DM write boundary](dm-write-boundary.md). |

### Legacy DM reads and client cache scope (accessed 2026-09-15)

| Problem | Primary evidence; publication/update | TDF selection / rejected alternative | Validation |
|---|---|---|---|
| Policy and message cursor checks can observe different authority | [PostgreSQL 16 function volatility](https://www.postgresql.org/docs/16/xfunc-volatility.html), official versioned docs, update unavailable; re-read on access date | STABLE read functions combining policy, cursor and payload in one statement snapshot. Reject separate preflight authority then content queries. Preserve existing DTOs. | DmReads safety/progress, three specific counterexamples, 1,440 generated PostgreSQL observations and HTTP fixtures; overlapping reads can return the earlier authorized snapshot. |
| Cached conversations shared between signed-in accounts | [TanStack Query v5 query keys](https://tanstack.com/query/latest/docs/framework/react/guides/query-keys), first-party live docs, update unavailable | Include actor in every related chat query key; hide/remove stale display data after errors and remount draft state on account switch. Reject route/thread ID alone as cache scope. This is display isolation, not server authorization. | Denied-refetch and account-switch component tests using the actual query cache; existing-session cleanup remains separately necessary. |
