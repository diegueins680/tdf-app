# Authenticated HTTP verification contract

This test-only increment refines EO-003/EO-009/EO-045/EO-051/EO-055 using the already checked
`EventLifecycle`, `ReceiptReplay` and `SnapshotRead` models. No new business transition or production
feature is enabled. The same bounded checks must still pass; HTTP tests do not extend their proof scope.

Mount the production `EventOperationsAPI` behind the production `authContext`, using the production
`eventOperationsServer` and PostgreSQL functions. Send real loopback HTTP requests through Warp and
http-client. Use disposable PostgreSQL, synthetic credentials and a complete test configuration with
all external integrations disabled. Do not replace authentication, handlers, SQL or response bodies
with mocks. Do not start application workers, load runtime secrets or contact deployed endpoints.

Required observations:

- Missing, unknown, inactive, reset-only, duplicate and conflicting credentials fail authentication.
- Event IDs do not confer authority. Unknown/unreadable snapshots reveal no event payload.
- The [command privacy refinement](command-privacy-contract.md) also requires identical POST
  status, raw body and non-Date headers for absent/unreadable targets, including occupied keys,
  changed bodies and accepted/rejected receipt replays after revocation. Visible read-only targets
  retain a 403 mutation denial; private durable diagnostics are not rewritten to hide them internally.
- Required idempotency headers, strict JSON, versions and transition states are enforced at HTTP.
- Authorized commands and reads agree on canonical state/version. Exact replays preserve every
  historical result field, with only `replayed=true` in response metadata; the stored receipt stays
  unchanged. Changed content conflicts. Concurrent retries yield one durable transition/receipt.
- Competing versions yield one accepted command and one explicit conflict, never two transitions.
- Approval requires a different actor; external-effect transitions stay unavailable.
- Reusing a still-active session after event-scope revocation cannot disclose the stored receipt or
  mutate state. Deactivated tokens fail the next authentication attempt. Read-only downgrades allow
  authorized historical reads but reject new writes. Expired grants and inactive assigned canonical
  roles fail closed on subsequent requests.
- Feature disable and invalid database projections fail without a fake success or sensitive body.

Check durable state, receipt and audit counts as well as HTTP status/body. Request rejection is not
equivalent to transaction rollback: modeled domain rejection may intentionally persist an audit and
receipt. Malformed HTTP input and failed authentication must not execute a command.

The [session fence refinement](session-fence-contract.md) now adds post-authentication barriers to
real HTTP GET/new/replay requests and rejects revocation before event execution. Direct production
handler tests cover token mutations; real PostgreSQL blocking PID observations check both lock
orders, cancellation and RC/RR/Serializable outcomes. These are not timing-only race assumptions.

Limitations: a focused production subrouter is not the entire `mkApp` middleware/router, browser,
mobile or offline queue. Global role/catalog revocation and permanent credential-revocation epochs
remain explicit review items. Test fixtures reproduce the columns used by canonical authentication but
do not constitute a rehearsal against the full production schema. No production activation follows.

## Reproducible runners

`npm run test:event-operations-http` creates a fresh PostgreSQL 16 container with a random port
published only on loopback, applies the existing foundation/API migrations and the synthetic auth
fixture, then compiles and executes the test with the repository's Stack environment. Warp binds
explicitly to `127.0.0.1`. The wrapper removes its own container on exit, including failure.

`scripts/run-event-operations-http-harness.sh` caches only this executable's objects under the
ignored `.stack-work/event-operations-http` directory. This avoids repeatedly interpreting the
Persistent models and keeps the regular backend build separate. It requires both a test DSN and an
explicit disposable-test guard. Dependencies must already be installed through Stack.

The existing `backend-quality` CI job runs `scripts/test-event-operations-http-ci.sh` after its
ordinary backend build/test gate. This runner requires `GITHUB_ACTIONS=true`, uses the fixed
`postgres` service with PostgreSQL 17, and creates the uniquely named test database; it refuses to
reuse an existing database and has no drop operation. The service container is ephemeral. It uses
the same fixtures and compiled harness as the local Docker runner. No existing CI gate is skipped
or weakened, and a failed preceding gate may prevent this new step from executing.
