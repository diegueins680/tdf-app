# Legacy DM read and API boundary — 2026-09-15

Status: implemented; verification register below distinguishes executed checks from
pending ones. Depends on the [DM write boundary](dm-write-boundary.md), PR #386.
This slice does not authorize production activation or complete the social cutover.

## Problem and resulting behavior

Legacy thread previews and message history checked thread participation but did not
apply the new canonical block, closure or consent policy. The write trigger in
#386 prevented disallowed inserts but could surface an unhandled database error.
`TDF.Social.Chat.chatPolicyServer` now adapts all four existing chat routes to shared
PostgreSQL functions. `TDF.API.Chat` preserves the existing route and DTO contract;
web and native clients need no wire-contract migration. Existing legacy handlers
remain available only before the social schema/policy requires enforcement.

| Surface | Authority and returned fields | Denied behavior |
|---|---|---|
| GET `/chat/threads` | Current token; participant; `dm_required`/`dm_allowed`; peer name, latest preview and timestamps from one statement snapshot | Exclude entire thread, including name, preview and its contribution to counts |
| GET `/chat/threads/:id/messages` | Same current authority; check thread before looking up cursor; bounded 1–200 messages | Same generic 404 for absent/forbidden thread regardless of cursor existence |
| POST `/chat/threads/dm/:other` | Current token; ordered party/credential locks; canonical permission or never-activated legacy compatibility | 403; administrator context cannot bypass canonical consent/block |
| POST `/chat/threads/:id/messages` | Current token and both parties locked in the transaction; participant; shared policy; retained write trigger | 403 for known policy denial; transient aborted SQL transaction maps to 503 with Retry-After; SQL details excluded |

`withCurrentSession` checks the bearer identity, current token owner, active state
and permitted purpose in the operation transaction. `withSocialSession` layers its
existing account-only liveness rule on that helper. A token check is not domain
permission. The compatibility adapter deliberately preserves never-activated legacy
organization behavior rather than silently granting organization delegation in the
canonical account-only pilot. Current credential/liveness policy is applied by the
canonical SQL whenever required. Global-role revocation for legacy administrator
compatibility and delegated-entity authority remain unqualified.

## Concurrency and pagination contract

Read functions are STABLE: policy, cursor and payload share the calling statement
snapshot. A block/revocation committed before that snapshot denies fields. An
operation overlapping a later commit may return the earlier authorized snapshot.
Session row locks additionally order token revocation against the transaction.
This does not retract content already delivered to a device or public internet.
There is no policy cache, search projection or relationship-distance grant.

Writes retain the ordered authority locks and trigger from #386. Thread creation
normalizes the two participants and uses the existing unique pair constraint;
repeated open requests return the same thread. Message send preserves the existing
non-idempotent POST contract: retry after an uncertain successful response can
create another message. No claim of exactly-once messaging is made.

Message pages retain the existing numeric message-ID cursor and ascending response
order; newest/before pages select descending IDs then return ascending IDs, and
`afterId` selects ascending IDs. Foreign-thread cursors are rejected only after
thread authorization. Deleted cursor IDs require a client refresh. These are legacy
message-ID semantics, not the immutable commit-order feed cursor. Concurrent direct
legacy writers can allocate IDs before commit; complete no-gap incremental DM
polling under those writers remains unqualified. No message update/delete API is
introduced here. Thread participants are assumed immutable through supported APIs.

Thread lists remain unpaginated for compatibility. One indexed LATERAL preview
query replaces per-thread application lookups, but database work and response size
still grow with eligible threads. Constant application query count is not constant
runtime or production-scale evidence. A versioned thread-list cursor is remaining
work, as are representative high-degree latency/lock-duration benchmarks.

## Executable model and refinement

`formal/social/DmReads.tla` models one thread, two members and one outsider, directed
consent reduced to one bit per member, activation/pause, block/unblock, closure,
credential revocation, thread/message reads and local/foreign/no cursor. The read
observation stores the fields actually emitted and the authority at the boundary;
its invariants do not merely restate an eligibility helper. Weak fairness of Read
establishes progress for the finite available-reader model. Network failure,
unbounded queues, token locking, SQL deadlocks and full graph topology are outside
this model; session/write models and runtime tests cover their stated subsets.

| Requirement | Property/action | Implementation | Automated evidence |
|---|---|---|---|
| DM-R1 No protected field without current authority | FieldsAuthorized / Read | STABLE threads/messages; participant + authoritative policy before fields | Generated SQL outcomes; real bearer preview/history tests |
| DM-R2 Pause cannot restore a denied read | Activate, Pause, Block, Close, Revoke | Retained `activated_once`; API independent of process/UI flag | Generated outcomes; paused HTTP and complete-schema fixtures |
| DM-R3 Cursor errors cannot reveal denied thread context | NoCursorLeak / Read | Authorization before scoped cursor lookup | Cursor negative control; SQL outcomes; same-body HTTP assertions |
| DM-R4 Outsider cannot read by guessing thread ID | FieldsAuthorized / Read | Participant predicate before policy/cursor | Outsider negative control; generated SQL and HTTP |
| DM-R5 Eligible read eventually terminates under availability | Progress / fair Read | Synchronous bounded message query, terminal HTTP error | TLC liveness; no unbounded-worker liveness claim |
| DM-R6 Revoked token cannot authorize a late operation | SessionBoundary properties | `withCurrentSession`, canonical domain checks | Existing 64 generated session outcomes/races plus captured-token HTTP cases |
| DM-R7 Retry open preserves uniqueness; send denial is atomic | LegacyDm plus database constraint | Unique normalized pair; ordered locks; existing trigger | Open retry and sender-injection HTTP; inherited insert/block races |

Pinned tools: TLC distribution 1.7.2, SHA256
`fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22`, Java 17.0.12.
Positive configuration explored **50,058 generated / 9,648 distinct states**, depth 9.
Cache, outsider and preauthorization-cursor negative controls each produced their
expected specific invariant violation. Trace export restricts symmetric viewers to
member `a` and outsider `c`: **33,372 generated / 6,432 distinct states**, depth 9.
`scripts/social/generate-dm-read-cases.py` consumes actual checked Read observations,
producing **1,440 distinct cases**, not a hand-written duplicate of SQL policy.
A deliberately unsafe membership-only SQL projection fails at generated case 36;
the real functions pass all cases. This is a regression control, not an execution
of the historical server binary. Each PostgreSQL case rolls back its fixture subtransaction, including activation
memory, while retaining ordinary assertion failures. Raw DOT is reproducible and
not required in source control. CI regenerates and diffs the committed SQL cases.

```sh
TLA_JAR=/path/to/tla2tools-1.7.2.jar bash scripts/social/check-dm-reads-model.sh
java -cp /path/to/tla2tools-1.7.2.jar tlc2.TLC -workers 1 -deadlock \
  -metadir /tmp/dm-read-states -dump dot,actionlabels /tmp/dm-read-traces.dot \
  -config formal/social/DmReadTraces.cfg formal/social/DmReads.tla
python3 scripts/social/generate-dm-read-cases.py /tmp/dm-read-traces.dot /tmp/dm-read-model-cases.sql
diff -u scripts/social/dm-read-model-cases.sql /tmp/dm-read-model-cases.sql
TDF_SOCIAL_HTTP_NATIVE=1 bash scripts/social/test-http.sh
TDF_SOCIAL_SCHEMA_NATIVE=1 bash scripts/social/test-schema-compatibility.sh
stack --stack-yaml tdf-hq/stack.yaml test --fast
```

Native scripts create and clean up private fixture clusters. Default scripts use
throwaway Docker PostgreSQL; CI HTTP uses an explicitly empty named PG17 database.
`TDF_SOCIAL_CHAT_SQL_ONLY=1` runs SQL refinement only and explicitly reports that
HTTP did not run. A passing bounded model is not proof of the complete application.

## Migration, compatibility and rollback

Apply `2026-09-15_social_v2_chat_api.sql` after foundation/read-model/DM-write
migrations, before any activation. It is additive and transactional; no backfill,
identity rewrite, consent inference, or destructive down migration is needed.
Existing threads/messages remain authoritative. Three supporting indexes and five
functions can be reapplied. Index creation is ordinary transactional DDL; measure
lock duration on representative non-production data before approving rollout.
No migration is registered for automatic production startup in this slice.

The adapter checks function availability per request. All required functions mean
use the adapter. Absent foundation permits the unchanged legacy handler. Any existing foundation with missing functions fails closed with 503, including
never-activated/empty state: checking that no pairs exist before falling back would
race the first block. Install the complete migration before serving this application
version against a foundation schema. Missing runtime state cannot select a legacy
reader once the adapter is installed.
Manual schema deletion or hostile database-owner changes are outside the boundary.

Keep `SOCIAL_V2_ENABLED=false`, UI flags off, and `social_v2_runtime.enabled=false`.
Before first activation/no canonical policy, the adapter preserves legacy participant
reads and mutual-follow/admin writes; it never converts follows into consent.
After activation, pause leaves all blocks/consents/messages and denial functions in
place. Rollback means disable new UI/mutations and retain the safe reader/trigger,
not deploy an old reader or drop these functions. Old writers remain guarded by
#386; old readers still bypass the new read policy. Therefore a rolling release
with old readers serving protected chat after activation is **not qualified**.

Monitor aggregate 403/404/503/500 rates and transaction latency, without token,
message text, peer identity or cursor payload in analytics. Investigate migration
availability and database errors before considering retries. Reuse existing
reconciliation for authority; functions are live queries and require no projection
rebuild or cache invalidation. No new infrastructure or production experiment.

## Verification register

- **Satisfied:** positive/three negative TLC configurations and checked trace export.
- **Satisfied:** all 1,440 observed SQL cases on private PostgreSQL 16.10.
- **Satisfied:** complete-schema additive apply/reapply and preserved-message pause.
- **Satisfied:** actual bearer HTTP/SQL suite: **93 examples, zero failures**;
  11 new adapter examples plus the 82 existing social/session examples.
  Evidence: `evidence/dm-read-boundary/http.txt`.
- **Pending:** final full Stack build/tests and current-head CI.
- **Failed/incomplete for overall scope:** profile/search/notification/media policy,
  global-role/delegation lifecycle, message idempotency/polling gap qualification,
  thread pagination, native/full-app journeys and outcome instrumentation.
- **Intentionally deferred:** activation, merges, deployments, destructive cleanup.

See the global [handoff](handoff.md) for the retained automatic-provider deployment
exception. This change does not resolve or conceal that earlier exception.

## Hosted fixture readiness repair

The initial child PR #391 model job passed the models/read refinement, then failed
before complete-schema loading with `terminating connection due to administrator
command`. The fixture's socket readiness check could see PostgreSQL's temporary
initialization server. The [official image entrypoint](https://github.com/docker-library/postgres/blob/master/docker-entrypoint.sh)
(accessed 2026-09-15; update date unavailable) explicitly starts that server without
TCP and shuts it down before the final server. The harness now waits for TCP and
asserts readiness after its bounded wait, matching the other social fixtures. No
assertion, migration or timeout was weakened. Hosted rerun remains separately tracked.
