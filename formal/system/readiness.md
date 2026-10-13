# Database readiness: DEPLOY-READINESS-001

The public `GET /health` boundary performs a fresh `SELECT 1` through the current
application pool after startup completes. Only the expected database result admits
HTTP200 with `status=ok`, `db=ok`. Synchronous failures or the two-second deadline
produce HTTP503 with fixed `status=degraded`, `db=unavailable`. Boot uses its
existing fixed starting503 response. Every response has `Cache-Control: no-store`;
unavailable/starting responses also carry `Retry-After: 5`. `/version` remains the
separate runtime identity boundary. The old OpenAPI version property was incorrect.

The timeout includes pool acquisition and query execution. `System.Timeout`
provides cooperative cancellation; an uninterruptible native call or process
scheduling stall is outside a strict wall-clock guarantee. External monitors must
have their own request timeout. Asynchronous cancellation propagates rather than
becoming a successful check. Private exceptions are neither rendered nor copied
to the public response. The SQL is fixed, non-mutating and cannot be supplied by a
caller. Each request probes anew; one success says nothing about later requests.

A pool keeps idle connections PostgreSQL has already closed (restart, failover,
terminated sessions); the first use of such a connection fails and the pool
destroys it. One probe therefore makes up to one more attempt than the pool holds
connections (`readinessAttempts`, bound to the pool size of `TDF.DB.makePool` plus
one by `scripts/verify-readiness.py`; the model showed that a budget of exactly
the pool size is defeated when every connection is stale), all inside the same
two-second deadline, and
answers from the first attempt that returns: a result, ready or not, is final.
A single request after database recovery answers ready; before 2026-10-12 the
probe made one attempt and answered 503 once per stale connection although the
database was back (AUTHORITY-078). An unavailable database fails every attempt,
so each probe costs at most pool-size-plus-one connection attempts.

Actual Haskell tests cover success, false results, private synchronous errors,
unavailable pool acquisition, stale-then-live retry, attempt exhaustion, final negative results,
real pool exhaustion and recovery, deadline expiry and cancellation (also through a retry). The source-bound
`scripts/verify-readiness.py` also requires five broken implementations to fail:
removed query, removed deadline, swallowed cancellation, a single attempt and no retry,
and checks that the attempt budget equals the pool size. Handler tests traverse
the real HTTP application with a failed pool and a real SQLite query. PostgreSQL
connection failure/recovery is also checked through the actual HTTP application:
after six simultaneous probes have left more idle pooled connections than the degraded probes below consume,
the owned fixture database disallows connections, terminates only its sessions,
requires fixed503 repeatedly, then reenables connections and requires200 from one probe. The
maintenance connection remains separate; shared PostgreSQL itself is not stopped.
`ReadinessProbe.tla` checks the attempt budget against every stale count and database state for a pool of four (the one-attempt and pool-size budgets fail).
The final deployed handler still requires a separate exact execution receipt. These finite tests and the bounded model are not a universal proof.

Readiness alone does not verify migrations, provider credentials, worker progress,
release exclusion, backups or API conformance. The deployment lane must bind image
identity, migration evidence and independent safe smoke tests separately. Startup
must never be skipped merely because the process accepts an HTTP connection.
