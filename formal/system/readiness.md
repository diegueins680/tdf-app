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
unavailable pool acquisition, real pool exhaustion and recovery, deadline expiry and cancellation. The source-bound
`scripts/verify-readiness.py` also requires three broken implementations to fail:
removed query, removed deadline and swallowed cancellation. Handler tests traverse
the real HTTP application with a failed pool and a real SQLite query. PostgreSQL
connection failure/recovery is also checked through the actual HTTP application:
the owned fixture database disallows connections, terminates only its sessions,
requires fixed503 repeatedly, then reenables connections and requires bounded
eventual200. Reopening PostgreSQL does not revive terminated idle connections in
the application pool: individual probes can correctly return503 while those
resources are discarded. The synthetic recovery check permits at most16 probes
over30 seconds, accepts only the exact documented503 between probes, and fails
immediately on an unexpected status, body or header. Sanitized observations record
contract classifications without retaining arbitrary response data. A witnessed
[ten-connection diagnostic](evidence/2026-10-15-readiness-pool-recovery/observations.json) at source `669ca45ba` returned seven503 responses after
reopening before a fresh connection returned200; it exposed the former invalid
first-probe-success assertion. These fixture bounds are not a public recovery-time
guarantee or a guarantee under continuing database faults. The
maintenance connection remains separate; shared PostgreSQL itself is not stopped.
The final deployed handler still requires a separate exact execution receipt. These finite tests are not a universal proof.

Readiness alone does not verify migrations, provider credentials, worker progress,
release exclusion, backups or API conformance. The deployment lane must bind image
identity, migration evidence and independent safe smoke tests separately. Startup
must never be skipped merely because the process accepts an HTTP connection.
