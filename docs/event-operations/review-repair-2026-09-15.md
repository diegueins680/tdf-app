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


## Follow-up: archive authority and task commit boundary

`RecordsManager` is now distinct from owner, event approver and finance approver.
Both archival transitions require that actor, matching the existing SQL policy.
The owner-archive mutant fails `AcceptedAuditIsAuthorized`; the final five-actor
runs visited 9,941 draft-start states and 139,174 all-boundary states. All eight
positive TLC configurations, three specific negative controls and nine Alloy
commands completed with their expected results.

Foundation SQL now incorporates the event-scoped write fence and deferred task
checks from PR #339, commit `a6cd9d1892ddafda06eab73a4e71389e802a5f7a`, by
continuous-improvement-loop[bot]. They also protect foundation-only installations:
completed tasks cannot acquire blocked dependencies through a later relation write,
and concurrent RACI writers serialize before checking the remaining assignments.
The write fence makes stale Repeatable Read/Serializable writers abort instead of
certifying counts from an old snapshot. Whole-command retry remains a caller duty.
The originating PR retains its formal models, integration tests and other work;
no replacement has been merged and that PR is not redundant or closed.

RACI validation uses `[valid_from, valid_until)` at validation time. Expiry does not
fabricate a revocation actor. An authorized SQL caller may invoke
`event_operation_retire_expired_raci(activity, actor, reason)` and insert replacement
assignments in the same transaction. Deferred checks require valid coverage at
commit; the existing unique indexes remain authoritative. Old intervals, actors
and reasons remain in the table. The SECURITY INVOKER function has PUBLIC execution
revoked; a future authenticated API must still verify current event authority.
This is an internal SQL primitive, not an exposed command or provider activation.

Rollback removes the added enforcement triggers and retirement function while
preserving fence revisions and RACI history. It does not erase the new metadata or
promise deletion compatibility with legacy writers; coordinate rollback before any
activation. The production manifest remains unchanged. PR #339's own migration
must retain these validity checks when the dependent stack is reconciled, since an
older CREATE OR REPLACE definition must not overwrite this fix.

The expanded PostgreSQL 16 fixture passed application/reapplication, preserved-history
rollback, post-completion dependency denial, actual timed expiry, attributed
replacement, future-dated responsibility denial and PUBLIC permission denial.
All three observed writer races passed under READ COMMITTED, REPEATABLE READ and
SERIALIZABLE; the second transaction waited on a real lock before its failure.
The gate connection is confined to the disposable test container and is terminated
only to release that fixture's synchronization lock. No application DB is touched.

## Follow-up: atomic logistics writes

Activity creation/update and assignment/dependency replacement now share one
`runSqlPool` transaction. A deferred graph or accountability rejection rolls back
the activity status, optimistic version and all relation changes together. A stale
version updates zero rows and does not replace relations. Travel verification still
runs only after the activity transaction succeeds.

The PostgreSQL foundation regression replaces a completed prerequisite with an
incomplete one while completing a task. It requires rejection with the old status,
version and graph preserved, then verifies a valid replacement commits. The full
foundation suite passed locally, including concurrency, rollback and reapplication.
This SQL regression exercises the database transaction boundary, not the HTTP route.

No new schema or data migration is introduced. Existing checks and optimistic
concurrency semantics remain intact. Roll back application code only with awareness
that the earlier handler can partially commit rejected requests; a forward repair
is preferred. No production data was changed during verification.

## Follow-up: recipient replies and retained dependency edges

The web invitation helper omits `invitationMessage` when no message was supplied,
so accepting/declining does not attempt to clear an organizer-owned field. Explicit
message changes remain subject to the existing server authorization guard.

Dependency replacement now applies the requested set difference. Unchanged edges
keep their IDs and creation timestamps; only removed edges are deleted and only
new edges are inserted. This preserves a valid blocked-completion override for an
existing graph without admitting a new blocked edge. A real database helper test
checks identity, provenance, deduplication and isolation from other activities;
the PostgreSQL suite exercises override completion with a retained edge.

Overrides remain bound to the exact activity version. A later version-changing
edit while prerequisites remain incomplete still requires fresh authorization;
the repair deliberately does not extend an old override to future commands. The
regression verifies both rejection without that authorization and success with it.
# Invitation creation authority

Creation now locks and revalidates the event organizer inside the same transaction
that inserts the invitation. PostgreSQL uses the existing event-row lock; SQLite
acquires its write lock before reading current authority. Validation of sender,
recipient and pending-only creation remains intact. Losing ownership while the
request waits cannot authorize a stale invitation.

The real PostgreSQL invitation regression now waits for a concurrent ownership
clear, verifies creation blocks on the row lock, commits that clear, and requires
403 with no new invitation. `quality:backend` runs the existing concurrency runner
against its freshly built test binary, avoiding a second compile and failing if
no matching tests are found. No schema or permission relaxation is introduced.
The concurrent repair's actual dependency-helper test is also mandatory in
`quality:backend` through `scripts/test-event-relations-runtime.sh`, using the
foundation migration and an independently disposable PostgreSQL fixture.
