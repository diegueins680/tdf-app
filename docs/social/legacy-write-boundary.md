# Legacy relationship writes — 2026-09-16

Dependency: [draft #402](https://github.com/diegueins680/tdf-app/pull/402), exact
parent `8b2d1557a5e72e0346af1fd43dc7cf36033da067`, on #397 → #390. This packet
repairs three explicit legacy relationship APIs. It does **not** qualify activation
of the whole social platform. All production flags remain inactive.

## Audit, decision and compatibility

| Capability / evidence | Response | Acceptance |
|---|---|---|
| `Server.socialAddFriend`, POST `/social/friends/:partyId` unilaterally upserts both PartyFollow directions | Share transactional compatibility adapter; retire for governed pairs | Before enforcement: same one-row DTO; afterward: 410, no mutation or names |
| `vcardExchange`, POST `/social/vcard-exchange` repeats that behavior with NFC provenance | Same adapter; NFC is provenance, never bilateral consent | Two-row historical DTO before enforcement; 410 afterward |
| `socialRemoveFriend`, DELETE `/social/friends/:partyId` deletes both directions | Retire the ambiguous command for governed pairs | Before enforcement: idempotent delete, existing 200 NoContent; afterward: no historical or canonical changes |
| V2 commands already model each principal's intent, revisions and request keys | **Reuse** `/social/v2/relationships/:partyId`; do not silently reinterpret legacy commands | Follow, request, accept, disconnect and unfollow remain distinct |
| `Server.fanFollowArtist` auto-creates reciprocal PartyFollow edges with every existing FanClubMemberProfile, and emits artist-follower notifications | **Separate repair required**, not fixed by this PR | Eliminate uncontrolled fanout/implicit member relationships during cutover; audit lifecycle and notification eligibility. Rollout remains blocked |
| Web `SocialPage` and mobile social/vCard consumers | Same URLs/payloads and successful DTOs before activation; existing failure paths handle denied requests | Web displays response message and restores optimistic state; mobile direct Axios consumers show generic status errors. V2 control cutover and native error-message polish remain required before activation |

The legacy payloads contain neither consent nor an expected revision/request key.
Translating one into an accepted canonical connection would invent the other
party's intent; returning a pending canonical state in PartyFollowDTO would also
lie to the old UI. Retirement is the explicit compatibility stage. No historical
reciprocal rows are backfilled as accepted connections.

This adapter returns 410 when the global durable activation latch is set, a
canonical pair exists in either direction (including an empty/unblocked tombstone),
or either account has closed social participation. The response is identical for
these cases, includes `Cache-Control: no-store`, and contains no names or block
reason. Once globally retired, pausing either process/UI or database flags cannot
restore the old operation. V2 availability during pause is not promised: disable
old controls and explain maintenance rather than reopening them.

An installation without the foundation keeps the old behavior. A foundation
without this adapter fails closed with 503. Normal bearer authentication remains
required; the repaired path additionally revalidates the captured token and owner
inside the mutation transaction. Invalid/self IDs return 400. Before enforcement,
missing target add/vCard returns 404 and missing target delete remains idempotent
200. Before global activation, legacy organization/credential eligibility remains
the historical contract; the governed branch retires the command completely.
No global role, organization proximity or purchase bypasses retirement.

## Authority and transaction boundary

One function, `social_v2_lock_legacy_write`, added by
`2026-09-16_social_v2_legacy_writes.sql`, uses the existing authoritative
`social_v2_dm_required` predicate (activation, canonical pair and closure). It does
not grant DM rights. `TDF.Social.RelationshipWrites` owns all three explicit routes,
with exact route aliases shared by `TDF.API` and the HTTP test harness.

1. Require READ COMMITTED. Lock the singleton runtime row `FOR SHARE`.
2. Lock both Party rows, then credentials, in ascending identity order `FOR UPDATE`.
3. Evaluate retirement after lock waits, using a fresh statement snapshot.
4. Reuse `withCurrentSession WriteSession`: same ordered accounts/credentials,
   followed by the actual bearer token lock and current active/owner/purpose check.
5. If allowed, perform both historical edge upserts/deletes and construct DTOs in
   this same transaction. UniquePartyFollow remains the authoritative uniqueness
   constraint. Upserts preserve publication/history timestamps and update NFC only.
6. Commit before releasing locks. Deadlock/serialization errors return retryable
   503; SQL internals are not returned. Unexpected SQL errors remain 500.

Activation waits for a write holding the runtime lock. Canonical pair mutation and
closure share account locks. Token updates/deletes conflict with the held token
lock. A write that acquired its locks first may commit before those changes; a
write waiting behind a committed change must re-evaluate it and deny. Network
response delivery can occur later than a concurrent denial; bytes already returned
are not retractable. These operations have no projection, cache, outbox or worker.
There is a fixed number of identity lookups for a two-account operation, independent
of degree. No new infrastructure is justified.

**Trust boundary:** this is an application adapter, not RLS or a table trigger.
Trusted owners/raw SQL, old application binaries and the separate fan-club writer
can still change PartyFollow. No accepted V2 consent is derived from those rows,
and repaired reads/DMs enforce the canonical policy independently. Do not claim
all legacy relationship writes are closed until the fan-club path is repaired and
old binaries are drained. Avoid administrative transactions taking Party locks
before runtime locks; use the documented order or handle transaction abort/retry.

## Research to decision

Primary sources accessed **2026-09-16**. Versioned PostgreSQL page update dates
are not specified; RFC 9110 was published June 2022.

| Problem | Primary evidence | TDF inference / rejected alternatives | Validation |
|---|---|---|---|
| Activation or revocation after a preflight check | [PostgreSQL 17 row locks](https://www.postgresql.org/docs/17/explicit-locking.html#LOCKING-ROWS) describes conflicting row locks and transaction duration | Hold runtime/account/token locks through the operation. Reject independent preflight+write transactions. Adds contention only to overlapping principals/cutover; runtime shared locks coexist | TLC unsafe-lock counterexample; real wait-event/barrier races |
| Stale view after waiting for locks | [PostgreSQL 17 READ COMMITTED](https://www.postgresql.org/docs/17/transaction-iso.html#XACT-READ-COMMITTED) describes fresh command snapshots | Explicitly require READ COMMITTED for the guard; take locks before subsequent policy query. Reject an old repeatable-read snapshot that can miss a newly created pair | Real stale-isolation rejection and pair/closure races |
| Legacy contract cannot represent a new consent protocol | [RFC 9110 §15.5.11](https://www.rfc-editor.org/rfc/rfc9110.html#name-410-gone) defines intentional persistent unavailability and notes heuristic caching | 410 for permanently retired commands after durable cutover; `no-store` because the response depends on current principal/context. The RFC does not prescribe TDF's consent rule. Reject silent conversion or fabricated successful DTOs | Generated HTTP status/effect cases, no-name denial and response-header check |

## Formal scope and traceability

`formal/social/LegacyWrites.tla` was checked **before** implementing the adapter.
TLC tool distribution 1.7.2 (prints TLC 2.17), Java 17.0.12, jar SHA-256
`fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22`.

Bounds: one unordered two-person pair, four possible historical directed-edge
sets, three legacy operations, two attempts including a retry, two activation
booleans, pair/closure/token state, and transaction phases. Pair creation abstracts
canonical request/block/unblock history; legacy commands never mutate canonical
consent. Existing Relationships and SessionBoundary models separately qualify
canonical consent and token-owner/purpose checks. Role/delegation, arbitrary SQL,
fan-club fanout, transport delivery, storage outages, and unbounded callers are not
proved by this model. Weak fairness assumes available database operations eventually
run; each bounded attempt reaches success or denial. It does not guarantee progress
during an outage or promise starvation-free PostgreSQL lock scheduling.

| ID | Property/action | Implementation | Executable evidence |
|---|---|---|---|
| LW-01 | `AuthorizedEffect`; Activate/Govern/Close/Revoke, Check/Commit | Durable latch + pair/closure guard, current bearer transaction | 288 generated Commit observations through real shared HTTP routes; authority-wins races |
| LW-02 | `DeniedPreservesHistory`; Commit | Denial before edge mutation/DTO construction | Stored directions checked for every generated outcome; no-name denial |
| LW-03 | `LegacyEffect`; Commit/Retry | UniquePartyFollow and upsert/delete, no writes to canonical pair | Generated effect checks, repeated NFC/add/delete, competing reciprocal requests; canonical consent/follows remain false |
| LW-04 | `AuthorizedEffect` after Pause/Retry | Monotonic activation memory, tombstones remain authoritative | UnsafePause and UnsafePair counterexamples; activation/pause HTTP test |
| LW-05 | `Progress`; fair Runtime/Accounts/Check/Commit/Retry | Bounded transaction/error outcome; conflict mapped to retryable 503 | TLC liveness and real barrier races; no fault-injected availability proof |

The generator consumes **observed Commit results** from the checked DOT graph,
not a second implementation of the permission predicate. CI regenerates and diffs
`LegacyWriteModelCases.hs`; the backend job runs them against PostgreSQL over HTTP.
Positive model: **9,600 generated / 4,320 distinct states, depth 15**, safety and
progress passed. Each of UnsafeLocks, UnsafePause and UnsafePair violates
`AuthorizedEffect`; the checker requires that exact invariant failure.
These bounded results are evidence about this model, not proof of all production
social behavior. Legacy retries before enforcement still lack request keys: a
delayed add can recreate an ungoverned deleted edge. After enforcement, retry is
denied and cannot resurrect it. V2 idempotency/revision controls remain the proper
replacement, rather than claiming stronger legacy semantics.

## Migration, rollout and rollback

Apply after foundation, DM activation memory, and the previous adapters. The
migration creates/replaces one function and revokes PUBLIC invocation; the existing
trusted application database owner executes it. No production migration registry
entry, automatic boot activation, backfill, new table/index, or destructive down
migration. A pre-existing function grant for a separate application role must be
managed by that deployment's established ownership procedure.

Stage all reader/writer adapters before enabling flags. Missing-function 503 is an
intentional incomplete-installation outcome, including before first activation.
Old/new instances can coexist only in the never-activated compatibility stage
without governed pairs/closure; old handlers do not share this enforcement.
Repair fan-club auto-follow, complete client cutover and drain old instances and
in-flight requests before any activation. Broader blockers in `handoff.md` also apply.

Pause both existing process/UI gates and `social_v2_runtime.enabled`; retain the
latch, authoritative pairs, historical PartyFollow rows, and this function/adapter.
Rollback can disable affected routes/UI, **not** restore the old unsafe binary.
No post-migration writes are removed. Reapply is idempotent and verified on the
complete PostgreSQL 17 schema fixture after pause. Reconciliation continues to use
existing read/graph scripts; do not rewrite legacy rows to force count equality.
Database drops/function removal and resetting the durable latch are not authorized
rollback procedures. Tests that reset the latch use throwaway fixtures only.

Monitor aggregate 401/410/503/error rates and lock wait/transaction latency, without
logging tokens, request bodies, profile names or pair identities. A rise in 410s
signals clients still using retired controls, not successful social conversion.
Keep accepted canonical connections, collaboration leads, bookings and sales as
outcome measures; this security repair has no observed conversion/retention result.

## Reproduction and acceptance

```sh
TLA_JAR=/path/to/tla2tools-1.7.2.jar bash scripts/social/check-legacy-writes-model.sh
# CI includes DOT export, case regeneration and exact comparison.
TDF_SOCIAL_HTTP_NATIVE=1 bash scripts/social/test-http.sh
TDF_SOCIAL_LEGACY_WRITE_BENCHMARK=1 TDF_SOCIAL_HTTP_NATIVE=1 bash scripts/social/test-http.sh
stack --stack-yaml tdf-hq/stack.yaml test --fast
bash scripts/social/test-schema-compatibility.sh
npm run audit:catalog-lists
```

Use only the throwaway fixture harnesses, never a development/production `.env`.
The native harness uses private PostgreSQL 16; schema compatibility uses Docker
PostgreSQL 17. No full-app screenshot or native runtime result is claimed here.

Actual local evidence is in [evidence/legacy-write-boundary](evidence/legacy-write-boundary/).
The final source files are fingerprinted there; full-log hashes identify excerpts.

| Acceptance criterion | Status / actual evidence |
|---|---|
| Executable bounded safety, progress and negative controls | **Satisfied**: 4,320 distinct states; three specific invariant counterexamples |
| Model-to-implementation refinement and real bearer routes | **Satisfied**: 288 generated cases; total **417 HTTP examples, zero failures**, including unchanged parent suites |
| Two-way race ordering, retries, no fabricated consent and DTO compatibility | **Satisfied** within the three-route/READ COMMITTED scope; actual wait-event/barrier, concurrent caller, no-foundation and partial-migration tests |
| Full backend build/tests | **Satisfied**: Stack/GHC 9.10.3, **2,542 examples, zero failures**, executable also built |
| Additive complete-schema migration/reapply/pause preserving writes | **Satisfied** on synthetic complete-schema PostgreSQL **17.10**; no production migration ran |
| Catalog authority audit | **Satisfied**: actual `npm run audit:catalog-lists` exit 0 |
| Performance threshold | **Satisfied** for private PostgreSQL 16.10 guard-only fixture, warm p95 <=50ms; values below |
| Hosted CI for this new PR | **Blocked pending remote execution**; never infer from parent checks |
| Fan-club write/notification integration, client cutover, native journeys, other lifecycle/privacy gaps | **Intentionally deferred to dependent repairs; blocks activation** |
| Production conversion improvement/capacity | **Blocked pending authorized later rollout and observation**; no claim |
| Merge/deployment/new production flags | **Intentionally pending**, all PRs remain unmerged; historical provider exception remains unresolved |

Synthetic benchmark: 10,005 accounts, 5 warmups and 40 alternating reference/guard
pairs per degree. Reference is the unguarded predicate, not the old full handler.

| Canonical degree | Reference p50/p95 (ms) | Guard p50/p95 (ms) |
|---|---|---|
| 0 | 1.599 / 4.299 | 5.054 / 8.063 |
| 10,004 | 3.628 / 10.107 | 7.797 / 12.224 |

The 50ms threshold was declared before measurement. Both queries return the same
eligibility result. Measurements ran on a shared local workstation and include pool
transaction overhead but exclude bearer checks, actual edge mutations, HTTP, and
network delivery. No production latency, throughput or operating-cost improvement
is demonstrated; no new service/storage cost was introduced.

Hosted CI must be reported at the actual pushed head, separately from local checks. Current parent #402 formal/client/browser/migration checks passed;
its backend was still running at the latest inspected snapshot.

Known development failures: the first model draft left IF expressions unparenthesized
and TLC correctly rejected an incompletely assigned successor, before implementation.
That construction was corrected; it was not counted as a passing negative control.
The first full Stack build rejected an inferred polymorphic name helper; an explicit
PartyId signature fixes it. An initial catalog command used a nonexistent npm script;
the documented command above is the actual repository check. No assertions or CI
protections were weakened to address any of these failures.
