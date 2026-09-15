# Invitation and lifecycle review repair

Invitation updates now lock the event and invitation, re-read the current owner,
recipient and status, validate the command, and write in one transaction.
PostgreSQL uses event-then-invitation row locks; SQLite obtains its write lock
before reading. A transferred invitation therefore cannot be reclaimed by a
recipient request that was authorized against an earlier snapshot.

`FollowHandlerSpec` exercises recipient transfer and terminal-state denial through
the actual handler. Its optional PostgreSQL case holds an organizer transfer
uncommitted, observes the recipient waiting on a database row lock, commits the
transfer, and requires HTTP 403 with the transferred recipient and pending status
unchanged. Run `scripts/test-invitation-update-concurrency.sh` with Docker, or run
`stack test tdf-hq:test:tdf-hq-test --fast --test-arguments='--match=invitation'` from `tdf-hq` with
`TDF_INVITATION_TEST_DATABASE_URL` pointing to an empty disposable PostgreSQL database.
The fixture creates its own three tables and must not run against an application DB.

EventLifecycle now separates event and finance approvers. A second positive
configuration starts at every lifecycle boundary, so the two-command bound does
exercise settlement approval. Accepted records are checked against the independent
required-authority predicate. AuditAppendOnly checks the entire unchanged old
prefix. Negative controls must detect an event approver settling finances and a
same-length rewrite of an existing audit record.

Formal verification completed with eight positive TLC configurations, the two
specific negative controls, one satisfiable Alloy scenario and eight UNSAT Alloy
assertions. See `formal/event-operations/README.md` for finite bounds and counts.
These results do not establish an unbounded proof or qualify future APIs.

## Executed verification

- Production executable: `stack build tdf-hq:exe:tdf-hq-exe --fast` passed.
- Invitation suite with a fresh PostgreSQL 16.10 database:
  `stack test tdf-hq:test:tdf-hq-test --fast --test-arguments='--match=invitation'`
  passed, 15 examples and zero failures, including the actual row-lock race.
- The test executable now links the threaded runtime with two capabilities.
  PostgreSQL blocking calls otherwise prevented the test's organizer thread from
  releasing its transaction. The production executable already used threads.
- The fixture includes `external_event_ref`, consulted by the ordinary-user
  visibility guard. Earlier incomplete-fixture and nonthreaded attempts were
  diagnosed and are not counted as successful verification.
- The unchanged CI policy regression suite passed 12 tests.
