# PR draft: Verify the authenticated event operations HTTP boundary

## Scope and dependency

Base: `fix/event-snapshot-privacy-boundary`, draft PR 342, head
`db81fdbe71d9b8e83d40d279b98c69df3749b316`. This is a test/CI/documentation increment, not a new
event system, public route, migration, feature flag or production activation.

The [HTTP verification contract](http-verification-contract.md) refines EO-003/EO-009/EO-045/EO-051/
EO-055 against the previously checked `EventLifecycle`, `ReceiptReplay` and `SnapshotRead` models.
The complete pinned TLC/Alloy runner was rerun successfully with the same documented finite bounds,
including all eight expected negative controls and all eight Alloy assertions. No unbounded proof
or new coverage of in-flight token revocation is claimed.

## Implementation and security effects

`EventOperationsHttpMain` mounts the production `EventOperationsAPI` with production `authContext`
and `eventOperationsServer`. The configured Warp listener binds loopback explicitly. Real HTTP
requests exercise the production token parser, canonical token/role queries, strict DTO parsing,
handler error mapping, request hashing and existing PostgreSQL command/snapshot functions.

Cases cover credential rejection and session cookies; event-ID isolation; malformed headers/body;
exact replay and changed-key conflicts; simultaneous retries and competing versions; event-grant
revocation, read-only downgrade and expiry; token deactivation on subsequent requests; independent
approval and fail-closed publication; feature disable; and sanitized non-success on a real missing
SQL-function failure. Durable transition, receipt and audit counts supplement HTTP assertions.

The complete `HttpTestConfig` has no provider credentials, external workers or runtime environment
loading. Only synthetic local test tokens and disposable database credentials are used. Fixtures
provide the canonical authentication query tables without fabricating authenticated identities.
No business endpoint is mocked. The deliberate database-function rename is fault injection only,
restored in a bracket and confined to the disposable database.

## CI and rollout

- Local: `npm run test:event-operations-http` uses a fresh PostgreSQL 16 Docker container with a
  random loopback-only port and cleans up its own container on success/failure.
- CI: the existing backend job runs the same compiled harness after the ordinary build/test gate,
  using a fresh database in its ephemeral PostgreSQL 17 service. No arbitrary external DSN or
  destructive drop operation is accepted by the CI fixture runner.
- The runner compiles 16 relevant modules through `stack exec -- ghc -O0 -threaded`, caching its
  own objects separately from the normal backend build. It does not compile or boot `TDF.Server.mkApp`.
- Path classification now selects backend validation for every new runner and its guard tests.
  Pipeline tests require both new checks and reject `continue-on-error` in that job.
- The formal workflow's four `actions/checkout@v4` references were inconsistent with the existing
  repository test requiring `v7`. They now use that same required major. The upstream v7 ref was
  verified through the GitHub API; no policy/test assertion was relaxed.

No production schema changes or rollback are needed. Reverting this PR removes only its tests,
runner/CI additions and documentation; it must not revert the preceding snapshot security fix.
The preceding feature remains disabled by default and outside the production migration manifest.

## Completed local verification (2026-09-14)

- Full pinned `scripts/verify-event-operations-formal.sh`: PASS, unchanged finite bounds, all eight
  expected negative-control violations, Alloy scenario SAT and eight assertions UNSAT in scope.
  The exact Java/JAR environment is the one recorded in PR 06 and the formal README.
- `sh scripts/test-event-operations-http.sh`: PASS on two clean PostgreSQL 16 databases after the
  replay expectations were corrected; 16 examples, 0 failures on each run. The second run uses
  the final non-deprecated HTTP manager construction. Production auth, database boundary and event
  handler compiled successfully together with their dependencies (16 modules plus linking).
- `npm run test:event-operations-http`: PASS on third and fourth fresh databases, 16 examples,
  0 failures each, confirming the documented npm entrypoint and warm-cache execution. The final
  runner probes the actual database over loopback TCP, excluding Docker's temporary socket-only
  startup server.
- `npm run test:event-operations-http-runners`: PASS, 3/3 safety-guard tests.
- `npm run test:ci-pipeline`: PASS, 21/21, including new runner path-selection and job-presence checks.
- `npm run verify:formal`: PASS, 0 critical/errors and 351 advisory warnings. This repository
  heuristic audit is separate from TLC/Alloy. `npm run test:formal`: PASS, 4/4.
- `npm run quality:repo`: PASS after installing the lockfile dependencies locally with
  `npm ci --no-audit --no-fund` (1,565 packages; no lockfile or dependency declarations changed).
  This also ran the existing internship, loop, release-tooling, pipeline, visual-artifact and
  persona-program tests. Release-tooling tests use their temporary fixtures, not production.
- Shell syntax for all three new runners, both changed workflow YAML files and whitespace checks:
  PASS. There are no new production schema changes requiring a rollback rehearsal in this PR.

Observed failures were corrected without changing business behavior: the original readiness probe
accepted PostgreSQL's temporary startup server before the database existed; it now queries the
actual database. The interpreted harness was stopped before success and replaced with separately
cached compilation. A missing `Maybe Value` annotation initially prevented the test helper from
compiling. The first full HTTP run had 13/16 passes: its three failures incorrectly expected
byte-identical replay envelopes. The existing SQL, SQL regressions and replay contract already
specify `replayed=true`; the corrected assertions check all other fields exactly and verify that
the persisted receipt still contains `replayed=false`. The initial repository check lacked
`@testomatio/reporter/jest`; the locked install fixed the local setup. The existing checkout-major
test first failed and passed after the four formal-workflow pins were aligned, not waived.

This is not a fresh full `stack test`/application build, whole-app HTTP journey, browser screenshot,
mobile test, payment-sandbox test, penetration test or performance benchmark. Those remain separate
gates. No production data, credentials, feature flags or live-money actions were touched.

## Remaining limitations

This focused subrouter verifies real production auth and handlers, not every route or middleware
in `mkApp`, full production schema compatibility, browser/mobile UX, accessibility, performance or
complete offline synchronization. No payments, providers or notification workers run here. Token
deactivation is checked between requests; the read-to-command window for in-flight authentication
still needs a separately modeled transaction boundary. Scope-administration APIs remain pending.
The POST absent/inaccessible status distinction also needs a private-event existence-privacy review;
GET opacity and denied mutations alone do not establish that stronger property.

Follow-up: [PR 08](pr-08-command-privacy.md) adds the paired formal model and SQL/HTTP regression
for that response-envelope gap. The results above remain the historical evidence for this PR.

PR 342's exact inspected head passed hosted formal verification and all three event PostgreSQL
jobs. Hosted repository/catalog/general-migration/preview jobs had failures and backend checking
was still running at inspection. Those results are not waived or treated as full green CI. This
branch's hosted results must be read independently after publication. No merge/deployment follows.
