# PR 27 — real RACI browser integration

## Scope and dependency

Branch `test/event-raci-full-stack`, based on draft
[PR 388](https://github.com/diegueins680/tdf-app/pull/388), exact head
`061553d085d3106a9c8877ca08ccb3e2420cdd09`. Test-only integration of the existing editor,
production session getter/authentication context/event handlers, and existing SQL migrations.
No application feature, API, generated type, migration, production flag or provider changes.

The [BI01–05 contract](raci-browser-integration-contract.md) precedes the harness.
Database fixtures extend the existing HTTP fixture only for canonical party/locale session
queries. Fixed test-only tokens authenticate real requests; no browser session JSON or
successful command receipt is manufactured. The narrow application does not start the main
server's workers or expose login, email, password or locale mutation handlers.

The runner compiles through Stack, creates an owned PostgreSQL 16 container, applies the
existing migration chain, starts an ephemeral loopback Warp listener and an owned Vite server,
then runs desktop and phone Chromium journeys. Vite does not load `.env` or inherit `VITE_*`
configuration. Browser foreign traffic is blocked; only the session/event API is proxied.
Unknown shell APIs are unavailable, which can produce the existing offline shell indicator.

## Verification commands and evidence

Execute from the repository root:

```sh
node --test scripts/__tests__/event-raci-browser-runner.test.mjs \
  scripts/__tests__/event-raci-web-editor.test.mjs \
  scripts/__tests__/event-raci-editor-context-runner.test.mjs
sh scripts/test-event-raci-browser.sh
npm run quality:repo
```

Prerequisites: repository Stack toolchain, a built canonical backend, Docker, locked npm
dependencies and Playwright Chromium. `stack build tdf-hq:exe:tdf-hq-exe` in `tdf-hq` installs
dependencies and generates the canonical `Paths_tdf_hq` module needed by the real session
handler's imports; `./node_modules/.bin/playwright install chromium` installs the test browser.
The runner fails clearly if generated metadata is missing; it never substitutes fake metadata.
Do not pass a production DSN. The shell entrypoint creates and owns the only database target.

Executed (2026-09-15, America/Guayaquil): ten runner/regression controls passed, shell/JavaScript syntax
checks and workflow YAML parsing passed. `npm run quality:repo` passed, with no generated
fixture drift; its heuristic audit reported 9,768 findings, zero critical/errors, 355 warnings
and 9,413 informational findings. That heuristic is separate from model checking.
The repository-quality loop/release tests use disposable local fixtures, not real main-branch
merges, production releases or provider operations.

The unchanged pinned formal suite passed again: 23 positive TLC configurations, 53 named
negative controls, 13 PlusCal integrity tests, two SAT Alloy scenarios and 13 UNSAT assertions.
`RaciWebEditor`: 154 generated/120 distinct states, depth nine. Exact local command:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

Finite bounds, abstractions and fairness assumptions remain those of the existing models;
this is not an unbounded proof or automatic implementation refinement. No feature code changed.
An initial cold compile was interrupted (exit 130) during severe local load, then
restarted using local cached GHC objects without disabling recompilation checks. The copied
objects are ignored local build artifacts, not repository source or deployment binaries.
The first standalone compilation exposed a missing `Paths_tdf_hq` search path. The harness now
uses the canonical backend's Stack-generated autogen directory; CI builds that prerequisite.
The real session module imports the monolithic API and catalog models, so even this narrow
application has a substantial cold compilation dependency graph. This increment retains those
production modules rather than maintaining a second session implementation. CI caches the
Stack dependency/build directories; reducing that coupling is a separate architecture change.

The first completed browser run compiled/linked the actual application and applied all fixtures:
six scenarios passed and the two revocation variants failed an incorrect test expectation.
Screenshot inspection and `AppShell.tsx:183` established that the real shell redirects expired
sessions to `/login`, rather than displaying the child task's signed-out notice. The corrected
test requires the exact local login destination/return path, login heading, absent private
dialog/table and unchanged database state. No application guard, test timeout or assertion of
authorization/data integrity was weakened.

The subsequent complete run passed **8/8 browser tests** in Chromium desktop/phone (69.3 seconds,
zero retries/skips/flaky cases), using a newly created PostgreSQL 16 container and the actual
80-module Stack-compiled session/event application. The real lost-response command produced
revision 6, exactly one immutable audit entry and one receipt; identical retry returned that
receipt with `replayed: true` and no further database changes. Stale commands returned 409,
revoked credentials 401 plus login redirect/private-data removal, forged reader writes 403,
and assigned outsiders' reads 404. Tests inspect persisted state, not just HTTP/UI messages.

Both actual review dialogs passed browser axe checks with no serious/critical violations.
Desktop and phone screenshots were generated and visually inspected; controls and the wrapped
request key remain readable. Artifacts are ignored under `artifacts/event-raci-browser/`;
the JSON report was independently checked for eight expected, zero unexpected/skipped/flaky
results. These are integration fixtures and loopback development UI, not a production capture.

A final fresh-container rerun after cleanup hardening also passed **8/8** (77.5 seconds,
zero retries/skips/flaky cases). Docker acknowledged removal of the owned container, and a
separate inspection confirmed it no longer existed. Only reproducible generated test data
was removed. The final runner/regression suite again passed all ten checks.

The dedicated CI job preserves existing gates. Its additional Stack setup action is pinned to
the dereferenced v2 commit, checked against the [official action](https://github.com/haskell-actions/setup).
Hosted execution is distinct from local evidence and is not claimed before a run completes.

## Security, rollback and limitations

The four scenarios assert actual database assignments/revisions/audit/receipts after lost-response
replay, stale reviewed commands, token revocation and reader/outsider access. Response loss is
injected only after a real successful server response. Concurrent conflict is a deterministic
interleaving, not a stress test of simultaneous transactions (covered by earlier SQL tests).
The inactive-token expectation follows the actual `client.ts`/`authEvents.ts`/`SessionContext`
expiry path: an identified authentication failure clears local task and dialog data. It must
not be treated like a generic network failure that preserves an uncertain review for retry.

Fixtures are partial canonical tables, not a production-schema migration rehearsal. Existing
complete-schema tests remain authoritative for that boundary. This does not verify the full
`mkApp` middleware, login, CORS, TLS, a deployed build, invitations, notifications, durable offline
queues, native mobile or payments. The phone project tests responsive web, not the mobile app.

Rollback removes only this harness, fixtures, dedicated CI and documentation. Existing data,
receipts, audit, APIs and application behavior are unchanged. Only the owned ephemeral database
is removed by cleanup; it contains generated test data and is reproducible by rerunning.
Cleanup preserves failing test exit codes and fails visibly if removal of the owned container
fails, rather than silently reporting a successful runner with leaked infrastructure.
No merge, deployment, production activation or real-money operation is part of this increment.
