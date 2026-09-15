# PR draft: Fence event transactions with their authenticated session

## Scope and dependency

Base: `fix/event-command-existence-privacy`, draft PR 346 at
`892038c7d7ec64082b39917127f67748e6d1dca7`. Branch: `fix/event-session-transaction-fence`.
EO-003/EO-045/EO-051/EO-055 map to `SessionFence.CurrentBoundSession`; see the
[contract](session-fence-contract.md) and [traceability matrix](traceability-matrix.md).
This is a scoped authentication-to-event correction, not a new event or session domain.

## Formal gate and implementation

The full pinned formal runner passed before editing production code. `SessionFence` explored
12,685 generated / 1,153 distinct states, depth 5: one transaction, 32 token records, two actors,
with/without witness and read/new/replay operations. All six new negative configurations exited 12
with `CurrentBoundSession`; the full runner detected sixteen expected negative controls. Alloy had
one SAT integrated scenario and eight UNSAT assertions within the existing relational scopes.
The exact Java/JAR command is the same as PR 08 and the formal README. No unbounded proof is claimed.

`loadAuthedUser` captures an opaque token ID, original party and SHA-256 credential fingerprint from
the same canonical token record used for authentication. `withCurrentAuthSession` locks that row
with `FOR SHARE`, then validates current activity, purpose, captured actor/token-owner binding and
credential (`constEq`). The event handler wraps GET/new transition/replay with this guard in the
same `runSqlPool` transaction; revocation cannot commit between guard and event completion. The
witness has no JSON instance, and its Show instance redacts all fields.

The optional Haskell field requires explicit `Nothing` for synthetic fixture users and the existing
notification-reviewer projection. Those contexts cannot call the fenced event handler. Role/module
semantics and global authentication routes are unchanged. Other domains do not automatically gain
transaction fencing. The formal workflow now also triggers on `TDF.Auth` changes.

## Schema, compatibility and rollback

No new tables, token columns, migrations, production checksum edits, public DTO shapes, generated
client changes or API paths. Existing bearer/cookie parsing remains authoritative. Refused stale
sessions return the fixed existing authentication-style 401 before any event SQL, receipt or audit
write. An operation that locks first may commit before revocation; a revocation that locks first
causes refusal (or serialization failure), with fresh authentication required on retry.

Rollback: keep event operations disabled, then revert this Haskell/context/fixture increment
coherently. Existing SQL rollback still disables/drops event entry points and preserves history;
do not enable the old unfenced handler. No production token or credential changes are part of tests.

## Verified evidence and limitations

- Pre-implementation regression: 19 examples, 1 failure, specifically the already-authenticated
  snapshot after token revocation; all 18 preceding HTTP scenarios passed. This was a disposable
  database reproduction, not a production probe.
- Full pinned TLC/Alloy runner: PASS before production edits, with bounds described above.
- Initial corrected HTTP/handler/database suite: 34 examples, 0 failures (9.8429 seconds test time,
  excluding bootstrap/compilation). Production auth/handler modules compiled through Stack.
  Added checks cover all token mutations, original receipt/state/audit preservation, missing/copied
  witnesses, redacted Show, real GET/new/replay HTTP requests paused AFTER actual authentication,
  and PostgreSQL blocking PID observations for both lock orders, cancellation, RC/RR/Serializable.
- Final `npm run test:event-operations-http`: 36 examples, 0 failures on another fresh PostgreSQL 16
  database (17.1551 seconds test time). This includes 21 real HTTP examples plus 15 direct
  handler/session/database examples. It also verifies fresh authentication after credential
  rotation, the explicit same-credential reactivation policy and sanitized guard-query failure/recovery.
- `npm run quality:repo`: PASS, including 21 CI pipeline tests and the heuristic formal audit
  (0 critical/errors, 352 advisory warnings; not the TLC/Alloy proof boundary).
- After staging the new test module, `npm run verify:formal` again passed with 0 critical/errors
  and 354 warnings. The two additional warnings match test labels containing "lock" in
  `waitBlocked`/`revokerPid`; those paths observe PostgreSQL blocking and execute inside bracketed
  `withWorker` lifetimes. No audit rule or threshold was suppressed or relaxed.
- `npm run test:event-operations-http-runners`: PASS, 3/3. The formal runner passes shell syntax,
  the changed workflow parses as YAML, relative documentation links resolve and whitespace checks
  pass. Read-back confirmed the disposable event test containers were removed.

The initial optimized compatibility build, `stack test tdf-hq --no-run-tests --no-terminal`, was
interrupted explicitly with exit 130 after module 52/209 when optimization changes forced a full
rebuild. It is NOT a passing build or test run. The replacement compatibility command uses the
normal Stack fast profile: `stack test tdf-hq --fast --no-run-tests --no-terminal`; its result must
be recorded separately. It is still in progress at the draft checkpoint, not a verified full build
or full test run. CI/production optimization and existing test requirements are unchanged.

The contract tests current token validity. Explicit reactivation of the same credential is still
reauthorization; permanent revocation must rotate credentials or keep old tokens inactive. Global
role/catalog changes, token storage hardening, other domain transactions, full `mkApp`/production
schema rehearsal, mobile/browser/offline flows and later product phases remain separate gates.
No full backend build/test result is implied by the focused harness. Hosted checks must be inspected
for this exact head independently. Parent PR 346 passed hosted formal, three event PostgreSQL,
repository and API jobs; catalog/UI/persona/broad-migration/production-migration/preview checks
failed and backend was still running at inspection. No failures are waived or attributed without logs.

No merge, auto-merge, production deployment/migration/activation or real-money action was performed.
