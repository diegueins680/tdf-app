# Interaction write budget — SOC-ABUSE-001

Every signed-in write to comments, reactions, reports, blocks and interaction
preferences is charged to the account before it runs. The charge is one row per
account and database calendar minute in `directory_rate_limit` (scope
`interaction:write`, account stored as a SHA-256 hash), incremented by
`interaction_consume_write_budget`.

## Rule

- A write attempt is charged after the caller's session has been confirmed current
  and before the write statement runs.
- Charges 1 to 90 of a minute let the write run. Charge 91 and later answer
  `429` and the write statement is not run; the refused attempt is still counted.
- The charge is kept when the write is answered, refused by a domain rule (403,
  409, 422 and the others returned as a JSON error), or refused by the database
  with an error raised by the write statement.
- A minute's counter is its own. A new minute starts from zero.
- A write the database refuses is undone completely; only the charge remains.

## Defect repaired (AUTHORITY-059)

The charge and the write ran in one transaction with nothing between them. A write
that returned a domain refusal committed, so the charge was kept, and this is what
the HTTP regression covers. A write that raised a database error aborted the
transaction and the charge with it. One input reaches that path directly: a JSON
payload containing a NUL escape (`\u0000`), which PostgreSQL refuses when the
payload is converted to `jsonb`. The request answered `500` and cost nothing, so an
account could repeat it without limit, each time taking the session row locks and a
pooled connection. A deadlock (answered `503`) was refunded in the same way, as
would be a lock timeout or serialization failure where a deployment configures
those; the repository sets no lock timeout and uses the default isolation level
for these requests.

`chargedInteractionWrite` now takes a savepoint after the charge, runs the write,
and on a database error returns to the savepoint, so the transaction commits the
counter. A NUL escape refused by `jsonb` (`22P05`) is now answered `400 invalid`
instead of `500`, for reads as well as writes. Other database errors keep their
previous status codes. A write that loses to contention and is answered `503` is
now charged, and so is each retry.

## Evidence

- `formal/event-operations/InteractionWriteBudget.tla`: four attempts by one
  account over two minutes with a limit of two; an attempt begins, charges under
  the counter row lock, runs a write that is answered or raises, and ends. 4,845
  distinct states, three invariants: `ChargeCounted` (every ended attempt is in the
  stored counters), `LimitHolds` (no minute runs more writes than the limit),
  `RefusedOnlyWhenSpent` (an attempt is refused only when its own minute was
  spent). Four controls each change one guard: a raised write refunds its charge
  (`ChargeCounted`), the threshold admits one more (`LimitHolds`), the increment
  is not locked (`ChargeCounted`), one counter serves every minute
  (`RefusedOnlyWhenSpent`).
- `tdf-hq/test/TDF/InteractionWriteBudgetSpec.hs`
  (`scripts/test-interaction-write-budget.sh`, PostgreSQL, fully migrated schema,
  the migrated `interaction_consume_write_budget`): a write refused with `22P05`
  leaves the counter at 1; a write that inserts and then fails leaves no insert,
  a usable transaction and the charge; a successful write is charged once; in one
  minute attempts 1 to 90 run, attempt 91 is refused without running and the
  counter reads 91; eight simultaneous refused writes leave the counter at 8;
  an account that has spent its minute does not stop another account. Run
  locally on 2026-10-11 against PostgreSQL 16; the same statements run in one
  transaction without the savepoint leave the counter at 0.
- `scripts/interactions/test-api.py` (existing): through HTTP, commands refused by
  authorization exhaust the budget and the stored counter is exactly 91.

## Limits

- The model is bounded (four attempts, two minutes, limit two) and checks safety
  only. It holds one lock for the account; PostgreSQL locks one row per minute,
  and counters of different minutes do not interact.
- Requests rejected before any database work are not charged: a malformed
  identifier (400), an oversized payload (413), an out-of-range version (400), a
  session that is no longer current (401), and a paused or absent interaction
  runtime (404).
- A charge cannot be kept when the transaction itself cannot commit: a lost
  connection, a failure at commit (the two deferred foreign keys of
  `interaction_comment` are checked there), or a failure while taking the session
  locks before the charge. Only database errors raised by the write statement are
  caught; any other failure (a result that cannot be decoded, a cancelled request)
  still undoes the charge. No input that reaches these paths is known.
- The model represents the repair as one boolean guard; it says nothing about the
  savepoint mechanism, and its attempts always commit.
- The tests call `chargedInteractionWrite` directly. Nothing tests that the HTTP
  boundary commits after a raised write, and no test produces a deadlock.
- The window is a fixed calendar minute, so up to 180 writes can run within a few
  seconds across a minute boundary. `interaction_command` separately refuses more
  than 30 new commands per account in a rolling minute.
- Nothing prunes `directory_rate_limit`; rows for past minutes accumulate.
- The HTTP mapping of `22P05` to 400 is not covered by an HTTP test; the PostgreSQL
  test covers the charge and the undo.
- Every write now runs inside a subtransaction, which costs PostgreSQL slightly
  more per write; the effect at volume has not been measured.
