# Session revocation boundary — 2026-09-15

## Problem and implemented boundary

`loadAuthedUser` previously discarded the API token identity. A request authenticated
before revocation could reach `/social/v2` afterward with a still-active Party and
succeed. Running the new captured-session HTTP regression against the preceding
handler reproduced **expected 401, actual 200** (73 examples, one failure; fail-fast).

Authentication now retains only `Maybe ApiTokenId` in the internal `AuthedUser`.
It does not retain or log the bearer secret. `withSocialSession` rechecks the exact
token's existence, owner, active flag and authenticatable purpose, then the actor's
current social liveness. The same Unicode-aware token-label predicate is used at
initial authentication and this boundary. Missing synthetic session identity returns
401; socially closed/ineligible actors return 404. Other active tokens for the same
account remain independent. No request/response contract changed.

Each social API query runs this guard and its domain operation in one `runSqlPool`
transaction. Lock acquisition is sorted Party rows, sorted credential rows, exact
token row, then the domain's existing pair locks. Reads use `FOR SHARE`; writes use
`FOR UPDATE`. Ordinary token UPDATE/DELETE conflicts with those locks. Credential
locks precede token locks, matching password-reset writes. A revoke that wins the
lock causes denial; a guarded operation that wins may commit first. This is the
boundary, not a promise to retract bytes already returned to a device. The first-page
publication batch remains a separate committed, guarded transaction before feed read.

The helper does **not** grant content, membership, connection or organization rights.
Existing domain policy still runs. This stage supports the account-only pilot;
managed entities and global-role revocation require their own current authority.
Party-to-token binding prevents substituting another account; it is not delegated
entity support. Legacy endpoints have not acquired this guard merely because the
internal auth record contains a token ID.

## Research and tradeoff

Official [PostgreSQL 16 row-lock documentation](https://www.postgresql.org/docs/16/explicit-locking.html)
(accessed 2026-09-15, page update date unavailable) describes conflicting row locks
and their transaction lifetime. [Function volatility](https://www.postgresql.org/docs/16/xfunc-volatility.html)
(accessed the same date) describes the statement snapshots used by STABLE functions.
TDF inference: retain the existing read-model snapshot boundary while holding current
session authority stable through the operation. Reject auth-time-only checks and
process-local token caches: neither coordinates with database revocation. No new
infrastructure or schema is needed. Shared read locks permit concurrent readers;
exclusive writes contend per account. The full successful single-query path has six
top-level SQL operations by code inspection, versus one unguarded domain SELECT;
this is not a captured database query-count measurement. Multiple credentials add
locked rows; larger account distributions remain a performance qualification task.

## Executable model and refinement

`formal/social/SessionBoundary.tla` models authentication separately from current
read/write authority, revocation, actor closure, lock acquisition and completion.
The positive configuration has two actors, two tokens owned by the first actor,
and two requests: **17,280 generated / 7,464 distinct states, depth 9**. TLC checked
`TypeOK`, `AuthorityAtResult`, `LockIntegrity` and fair `Progress`. Three deliberately
unsafe configurations each produce the specific `AuthorityAtResult` counterexample:
cached reads, unlocked writes and identity bypass. Logs are in
`evidence/session-models/`; a detected expected counterexample is not a passing
unsafe design.

The one-request `SessionTraces.cfg` explored **344 distinct states**. Its actual
exported DOT graph is committed. `generate-session-cases.py` reads observed `Read`
outcomes from that graph and emits 64 deterministic cases. Real PostgreSQL tests
compare those model outcomes with the Haskell guard, including different active
sessions and attempted acting-account substitution. The generator does not invent
expected permission results from implementation code.

| Requirement | Model property/action | Mechanism | Executed regression |
|---|---|---|---|
| S-SESSION-READ | AuthorityAtResult / Authenticate, Read, Revoke | token ID retained; current token and actor checked in transaction | 64 generated cases; captured-session handler GET/PUT returns 401 after revoke |
| S-SESSION-WRITE | AuthorityAtResult, LockIntegrity / Acquire, Finish, Revoke | account/credential/token locks held through domain operation | both lock acquisition orders; one committed effect or denial, no stale extra effect |
| S-SESSION-IDENTITY | AuthorityAtResult / Bound | current token owner equals acting account | model account substitution; deleted/reassigned token; synthetic actor denied |
| S-SESSION-LIVE | AuthorityAtResult / Close | social_v2_live after credential locks | generated active-credential cases and closure tombstone denial |
| S-SESSION-PROGRESS | Progress / fair Advance | terminating database operation and transaction release assumptions | timed barriers; password-reset lock order and simultaneous shared reads |

The model aggregates one pair's lock; it is **not a proof of multi-pair deadlock
freedom**. Reads are atomic at the modeled boundary. PostgreSQL tests separately
exercise held read locks. Fair progress assumes database availability, eventual
lock-holder completion and weakly fair request advancement. Token IDs are not reused;
reactivation, changing model token ownership, global-role catalogs, network delivery
and delegated authority are outside this model. Reassigned/deleted-token tests extend
coverage beyond the modeled transitions. No full-system formal proof is claimed.

## Actual verification

- Final native PostgreSQL 16.10 HTTP/guard fixture: **82 examples, zero failures**:
  eight existing real-bearer HTTP examples, 64 model-derived checks, ten additional
  session regressions and concurrency cases. `evidence/session-http-final.txt`.
  Captured-session tests deliberately emulate a request whose real authentication
  already completed; they do not claim a second fresh bearer check at dispatch.
- Before-fix handler: **73 examples, one failure**, expected 401 versus actual 200.
  `evidence/session-http-before.txt` (trailing terminal whitespace normalized). Restoring the guard fixed the same regression.
- Concurrency tests observe actual `pg_stat_activity` lock waits and use barriers,
  with ten-second failure bounds; success is not inferred from a sleep duration.
- Opt-in loopback benchmark: five synthetic accounts, five warmups, 40 alternating
  paired samples. Predeclared added warm p95 limit: 20 ms. Baseline p50/p95
  **2.951 / 3.963 ms**; guarded **5.376 / 7.808 ms**; paired added p95 **4.841 ms**.
  Passed for this fixture only. This does not qualify high-degree HTTP or production
  performance; it is separate from the existing SQL feed benchmark.
- External-database adapter on private PostgreSQL 16.10: **82 examples passed**;
  wrong-name and populated-database attempts were rejected, fixture rows preserved.
  `evidence/session-http-external.txt`. PostgreSQL 17 hosted execution is separate.
- Workflow/scope tests: **21 passed**. Full new Stack backend build: **2,542 examples, zero failures**, application
  executable built, exit 0 at source `a68f235ca8a7d1987fd94defdba078228f77cd1c`.
  `evidence/session-backend-result.txt`. New hosted CI is tracked separately;
  parent checks do not qualify this change.

Toolchain: Stack 3.7.1, GHC 9.10.3 / lts-24.42, Java 17.0.12,
TLC distribution 1.7.2 (reports 2.17), jar SHA256
`fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22`.

```sh
TLA_JAR=/path/to/tla2tools-1.7.2.jar bash scripts/social/check-session-model.sh
java -cp /path/to/tla2tools-1.7.2.jar tlc2.TLC -workers 1 -deadlock \
  -metadir /tmp/session-states -dump dot,actionlabels /tmp/session-traces.dot \
  -config formal/social/SessionTraces.cfg formal/social/SessionBoundary.tla
python3 scripts/social/generate-session-cases.py /tmp/session-traces.dot /tmp/SessionModelCases.hs
diff -u scripts/social/SessionModelCases.hs /tmp/SessionModelCases.hs
# Default HTTP harness uses a private Docker PostgreSQL instance.
bash scripts/social/test-http.sh
# Optional native PostgreSQL 16 fallback and fixture benchmark:
TDF_SOCIAL_HTTP_NATIVE=1 TDF_SOCIAL_SESSION_BENCHMARK=1 bash scripts/social/test-http.sh
(cd tdf-hq && stack test --fast)
```

CI regenerates and compares the model cases. The backend job creates the dedicated
empty `tdf_hq_social_session_test` database, then runs the same HTTP harness against
its PostgreSQL 17 service. The external-database adapter rejects any other database
name or pre-existing public tables/views before fixture DDL. Never supply production
configuration. CI does not enable the optional machine-sensitive latency gate.

## Compatibility, rollout and rollback

Depends on #377 and its stack. No migration, backfill, new infrastructure or client
contract change. Existing internal synthetic `AuthedUser` constructors explicitly
use `Nothing`; they cannot impersonate an enabled social session. Existing domain
test assertions remain intact. Both existing social gates and the web flag remain
off in production. The patch has no effect on disabled social routes.

Before activation, reverting this additive internal change preserves database
writes and old contracts. After activation, keep this minimum enforcement version:
reverting to a handler that ignores token revocation would restore the demonstrated
bypass. Pause UI/new work while retaining authority checks and persisted blocks.
The wider legacy/privacy cutover remains incomplete and activation stays blocked.
