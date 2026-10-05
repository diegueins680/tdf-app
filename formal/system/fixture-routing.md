# Disposable PostgreSQL fixture routing

`SYS-VERIFY-004` specifies the shared `disposablePostgresUrl` admission boundary.
Before its first SQL command, each adopting fixture runner requires a PostgreSQL
URL addressed to `127.0.0.1` or `localhost`, or the `postgres` service when CI is
explicitly enabled. The plain database name must end in `_test`. Query strings,
fragments, path escapes and `PGHOSTADDR`, `PGSERVICE` or `PGSERVICEFILE` overrides
are rejected rather than handed to libpq.

The executable contract is in `scripts/lib/disposable-postgres-url.mjs`. URL
controls include local and CI positive cases and override/remote negative cases.
`scripts/__tests__/merch-runtime-safety.test.mjs` invokes the actual merchandise
, provider-retry and ticket-admission shell runners with an instrumented `psql`. Invalid targets
must never reach SQL; the positive fixture reaches the sentinel and stops. No
provider or database is contacted by those controls.

**Routing is not ownership.** A local database named `production_test` is not
safe to erase merely because it passes this validator. Operators/runners remain
responsible for nonce ownership, clean environment, database lifecycle and
cleanup. This contract does not approve every historical test runner or claim
transaction isolation. Other consumers are indexed but require their own
execution evidence. No production database mutation is authorized by a test URL.

The ticket runner additionally requires exactly `tdf_ticket_admission_test`.
Its directly callable Haskell harness independently rejects routing overrides
before opening a pool. It intentionally admits only complete loopback URLs
(default/5432 port, current OS user) or the exact opt-in CI service URL; arbitrary
credentials/ports require an explicitly reviewed runner change. The standalone
`TicketDatabaseRoutingMain.hs` checks this narrow allowlist with negative inputs.
