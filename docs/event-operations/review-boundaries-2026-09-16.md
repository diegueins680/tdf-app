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
