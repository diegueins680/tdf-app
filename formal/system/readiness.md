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

Actual Haskell tests cover success, false results, private synchronous errors,
unavailable pool acquisition, deadline expiry and cancellation. The source-bound
`scripts/verify-readiness.py` also requires three broken implementations to fail:
removed query, removed deadline and swallowed cancellation. Handler tests traverse
the real HTTP application with a failed pool and a real SQLite query. PostgreSQL
connection failure/recovery and the final deployed handler require separate exact
execution receipts. These finite tests are not a universal proof.

Readiness alone does not verify migrations, provider credentials, worker progress,
release exclusion, backups or API conformance. The deployment lane must bind image
identity, migration evidence and independent safe smoke tests separately. Startup
must never be skipped merely because the process accepts an HTTP connection.
