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
