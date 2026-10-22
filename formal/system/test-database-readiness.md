# Event-operations API test database readiness

`SYS-CI-DB-001` covers only `scripts/test-event-operations-api-migration.sh`.
The runner owns its container, database and synthetic credentials. Its existing
30-attempt deadline, diagnostic logging, SQL assertions and cleanup remain intact.

Hosted [postgres-api run 39712279548](https://github.com/diegueins680/tdf-app/actions/runs/39712279548)
failed its first fixture SQL with “the database system is shutting down”. A
socket query can succeed while the PostgreSQL image is still initializing. The
[official entrypoint](https://github.com/docker-library/postgres/blob/master/docker-entrypoint.sh)
starts that temporary server with empty `listen_addresses`, stops it after
initialization, and then starts the final process. The readiness SELECT now uses
`-h 127.0.0.1`, so the temporary socket-only server cannot admit fixture execution.

Local verification uses actual PostgreSQL 16.13 and the extracted old/repaired
queries, forwarding the container CLI to that owned server. The old query accepts
the socket-only phase; the repaired query rejects it and the stopped phase,
then accepts the final TCP listener. The first API fixture applies afterward.
The owned database is removed and server stopped. This is a protocol-level
reproduction, not execution of the full Docker migration job.

Fresh exact-head Linux `postgres-api` qualification and independent review remain
required. This bounded repair establishes neither complete database isolation nor
readiness for other fixture runners. No production operation is involved.


## Explicit client authentication

The owned image now receives `POSTGRES_INITDB_ARGS=--auth-host=scram-sha-256`.
This makes omission of a client password observable in the complete Linux runner,
including loopback connections. The readiness `docker exec` explicitly supplies
`PGPASSWORD=event-operations-api-test`. The existing initialization variable
`POSTGRES_PASSWORD` sets the server credential; it is not a libpq client password.
All values are the existing synthetic fixture credentials. Socket-based fixture
commands, SQL assertions, retry bound, diagnostics and cleanup are unchanged.

The [PostgreSQL16 client-variable documentation](https://www.postgresql.org/docs/16/libpq-envars.html)
defines `PGPASSWORD`; [initdb authentication options](https://www.postgresql.org/docs/16/app-initdb.html)
define `--auth-host`. The official image passes `POSTGRES_INITDB_ARGS` to initdb.

A fresh actual PostgreSQL16 rehearsal exercises 15 controls: socket-only bootstrap,
stopped server, final SCRAM TCP listener, old TCP query with only
`POSTGRES_PASSWORD`, wrong client password, correct explicit client password,
first real API fixture and owned cleanup. Missing and wrong credentials fail for
the expected authentication reasons; the correct credential returns the readiness
result. Every owned database is removed and the cluster stopped. This uses an
extracted-command environment-forwarding adapter; it is not the full Docker
migration run. The earlier trust-host rehearsal retains its original identity.
The canonical evidence field records protocol results only; previous hosted runs
are historical, source-bound PR evidence rather than a current-head pass. A fresh
current-head Docker job and independent review remain required. Conformance stays
PARTIAL.
