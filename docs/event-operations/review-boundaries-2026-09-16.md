# Review boundary follow-up

The create/update logistics handlers now write the activity and its dependency/
assignment snapshot in one transaction. A rejected final graph rolls back the
activity status/version and relation snapshot together. This is the existing
implementation from PR #338, commit f500b62c453e7da8981bc66138418fa59a586565,
by continuous-improvement-loop[bot], recovered into the foundation so installations
without the successor cannot commit partial writes. The original successor branch
and attribution remain intact; no superseded PR is closed before integration.

The full Stack backend suite passed 2546 examples and the production executable
built successfully. Docker returned HTTP 500 when starting the additional SQL
regression; its local isolated PostgreSQL result is recorded separately after
execution. No API or production migration manifest change.

The isolated PostgreSQL follow-up passed: SQLSTATE 23514 rolled back activity,
version and dependencies; a valid transaction completing its prerequisite also
committed. See the audit log events-logistics-atomic-local-postgres.log.


## Status-only invitations and retained dependencies

Recipient UI responses omit the message field when no edit was supplied. Explicit
message changes still use the existing payload and remain subject to authorization.
Dependency replacement now deletes only removed edges and inserts only new edges;
retained rows keep their identity inside the activity transaction. The deferred
new-edge guard and version-bound completion override remain unchanged.

Regression commands: `npm test --workspace=tdf-hq-ui -- --runTestsByPath
src/api/socialEvents.test.ts` and `stack test --fast`. The optional real-PostgreSQL
case uses `TDF_EVENT_RELATIONS_DATABASE_URL` with the foundation fixture/migration
and `test/integration/event_relations_fixture.sql` in a disposable database. It
calls the same Haskell dependency replacement function used by create/update, verifies a
valid blocked-completion override preserves the original edge ID, and verifies
a newly acquired incomplete edge raises 23514 and rolls back. This bounded test
does not claim full HTTP authorization or permission for later activity versions.

Consolidation retains concurrent fa7c2e43 (including main and its SQLite regression)
and adds the PostgreSQL call through that narrower dependency helper. No shared
commit was rebased or replaced.

## Accepted-token revocation and append-only history

The invitation security row now preserves consumption when a later revocation is
recorded; revocation cannot precede consumption. Reapplication upgrades the old
mutually-exclusive constraint. Token digest uniqueness, expiry, purpose, scopes
and version checks remain intact. This is schema compatibility with the documented
accepted-to-revoked lifecycle, not a new unauthenticated revocation endpoint.

All five append-only history tables now reject statement-level TRUNCATE with
55000, including empty tables and CASCADE. Row UPDATE/DELETE guards remain.
Non-destructive application rollback retains these history guards. The fixture
checks old-constraint upgrade, accepted-then-revoked history, invalid ordering,
TRUNCATE denial before/after rollback/reapply and unchanged row counts. No runtime
role receives new privileges; a database superuser can still deliberately disable
triggers, which is outside application authorization.

Final consolidated verification: Stack target `tdf-hq:test:tdf-hq-test --fast`
completed successfully (2547 examples, zero failures) and installed the production
executable. The actual PostgreSQL helper regression passed on a second fresh
database; all18 API client tests and repository quality passed. The complete
foundation Docker runner passed upgrade/reapply, all existing concurrency and
rollback checks, accepted-token revocation and history TRUNCATE rejection.
