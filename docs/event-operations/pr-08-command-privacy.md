# PR draft: Hide unreadable event existence from transition responses

## Scope and dependency

Base: `test/event-operations-http-boundary`, draft PR 345, head
`dbe975d3143446c5e7cd9b132b6f994067065e7b`. Branch: `fix/event-command-existence-privacy`.
Refines EO-003/EO-009/EO-045/EO-051/EO-055; see the
[operation contract](command-privacy-contract.md) and [traceability](traceability-matrix.md).
No new event system, authorization domain, public route, lifecycle edge or provider is introduced.

## Formal specification before implementation

`CommandPrivacy` compares the complete abstract status/body observation for absent and existing
targets without current read authority. Bounds: one request, four receipt classes, two version
classes, three grant classes; grants can change before the atomic authorization/response decision.
TLC checked 81 generated / 49 distinct states, depth 4, with no invariant failure. Both mutation
configurations exited 12 with `OpaqueTarget`: distinct existence errors and premature receipt lookup.
The full existing TLC/Alloy runner passed before the production SQL edit; all ten expected negative
controls were detected, the Alloy scenario was SAT and all eight assertions were UNSAT in scope.
This finite observation check assumes the existing authorization fence. It is not a universal proof,
a session-revocation proof or a timing-channel guarantee.

Exact command (same pinned binaries/checksums as the formal README):

```sh
JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
bash scripts/verify-event-operations-formal.sh
```

## Implementation, security and compatibility

The two unreadable return paths in `event_operation_apply_transition` now return exactly
`{"error":"not_found"}`, which the unchanged Servant mapping exposes as 404 with
`{"code":"not_found"}`. Authorization still precedes receipt/conflict disclosure. Fresh readable
commands without mutation authority retain 403. Internal rejection receipts still record
`forbidden`; immutable receipts, denial audit and accepted side effects are preserved. After read
access is restored, an actor may replay their own historical rejection under the existing binding.

OpenAPI descriptions and generated web comments document the deliberate unreadable 403-to-404
change. No typed request/response shape, error enum or SQL function signature changes. No table,
index or data backfill is added. The unmerged, disabled-default API migration remains outside the
production manifest and is corrected in place; no deployed migration checksum is replaced.

Rollback: use the existing API rollback to disable the feature and drop the entry points while
preserving receipts/audit/authorization epochs. Reapplication stays disabled. Do not re-enable the
old leaking path. Production activation still requires the complete security and rollout review.

## Completed local verification (2026-09-14)

- Before implementation, `sh scripts/test-event-operations-api-migration.sh` failed with exit 3
  at the new paired SQL assertion: `target existence leaked ... {"error":"forbidden"}`. This is
  the intended reproduction, not a passing test or a production probe.
- Full pinned formal runner: PASS, as detailed above. The pre-existing PlusCal translation warning
  remains; no regeneration or unbounded proof is claimed.
- After implementation, `npm run test:event-operations-api-migration`: PASS against fresh disposable
  PostgreSQL 16, including exact JSON comparisons, private rejection/history preservation, current
  clock checks, RC/RR/Serializable revocation races, feature-disable race, apply twice, rollback twice
  and reapply. Existing 403 assertions for readable-but-unauthorized commands remain in force.
  A second fresh run also passed explicit post-reapply checks for all three new receipts, the one
  accepted transition and all nine audit entries in the privacy fixtures.
- `npm run test:event-operations-http`: PASS, 18 examples, 0 failures, 7.9129 seconds test time
  (excluding database/bootstrap/compilation). Tests use production auth, handlers and SQL, real
  loopback HTTP and disposable PostgreSQL. They compare raw body/status and every non-Date header,
  and check accepted/rejected receipts, other-actor keys, changed bodies, expiry/revocation,
  read restoration, independent approval, concurrency and failure recovery.
- `npm run generate:api:ui`: PASS; inspected generated diff contains only the three updated
  documentation comments. `node scripts/mobile-workspace-ready.mjs` exits 1 because
  `tdf-mobile/package.json` is absent here; mobile generation/testing was not claimed or faked.
- `npm run quality:repo`: PASS, including the 21 CI pipeline tests and the repository heuristic
  formal audit (0 critical/errors, 352 advisory warnings; separate from TLC/Alloy). Its release/loop
  tests operate on temporary fixtures, not the repository's real main branch or production systems.
- `npm run test:event-operations-http-runners`: PASS, 3/3 safety guards. Both edited shell scripts
  pass syntax checks; the changed workflow and OpenAPI parse as YAML; `git diff --check` passes.
  Read-back with `docker ps` confirmed no remaining event-operations test containers.

## Limitations and hosted checks

Follow-up: [PR 09](pr-09-session-fence.md) implements the bounded current-token fence for event
transactions. The in-flight-session limitation below describes this PR 08 checkpoint; global roles,
other domains and permanent revocation semantics remain outside that correction.

The in-flight authentication-to-command token-revocation window is still open. Constant-time
behavior, rate limiting, full `mkApp` middleware, full production-schema rehearsal, browser/mobile,
actual offline queues and later product phases remain separate gates. No live charges, deployments,
production migrations/flags, credentials, merges or auto-merge were performed.

At the inspected PR 345 head, hosted formal and all three event PostgreSQL jobs passed, as did
repo-quality, API checks and mobile-quality. Catalog, UI, broad migrations, production migrations
and preview checks had failures; backend/persona jobs were still running at inspection. This is a
status observation, not a diagnosed cause or a waiver. This branch's hosted results must be checked
independently after publication; no globally green CI claim follows from the focused local checks.
