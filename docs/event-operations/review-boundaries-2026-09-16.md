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
calls the same Haskell replacement function used by create/update, verifies a
valid blocked-completion override preserves the original edge ID, and verifies
a newly acquired incomplete edge raises 23514 and rolls back. This bounded test
does not claim full HTTP authorization or permission for later activity versions.
