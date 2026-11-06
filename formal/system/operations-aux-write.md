# Operations auxiliary writes (OPS-AUX-WRITE-001)

Slice of 2026-10-12 on main `7d4375550`. Model and evidence only; no code
change.

## What the handlers do (read 2026-10-12)

Every auxiliary mutation runs through `runOperationsWrite`
(`tdf-hq/src/TDF/Operations/Server.hs`): the session row is locked
(`withCurrentAuthSession`), the actor's active role grants are read under
that lock and replace the token's roles, and the handler runs in the same
transaction. `resolveCommandScope` picks the actor's active membership in an
enabled organization and branch and `lockOperationsScope` re-checks the
organization and branch flags and locks those rows and the member rows
`FOR SHARE` through commit. Notes go through `withLockedWorkItem`
(the work item `FOR UPDATE`, then assignment-dependent visibility for
Teacher and Engineer, and the mentioned members, checked under the lock;
managers see every item, so there is no must-be-assigned rule) and insert note, mentions, stream event and audit in that
transaction. A failure retry (`replayFailureHandler`, Admin, Manager or StudioManager)
locks the failure row `FOR UPDATE`, admits only `open` or `dead_letter`
with `retryable` set, sets `retrying` and writes the audit. A saved view is an
owner-bound upsert on `(organization, owner, name)` followed by its audit. A
push registration stores only `pgp_sym_encrypt(token, key)` through a CTE
that inserts nothing when the key setting is empty (the handler answers
503), then writes its audit. Every rejection is a thrown error inside the
transaction, so no partial result survives. No discrepancy with the
requirement was found by reading; the concurrency claims are the locks
above.

## Model

`formal/event-operations/OperationsAuxiliaryWrite.tla`: two commands of one
actor, each a note, retry, view or push; the environment may revoke the
session, deactivate the grant, remove the membership, unassign the item or
resolve the failure, and waits while a command holds its locks. A command
locks, reads its authority once under the locks, then commits (result and
audit together) or is rejected. Invariants: `CurrentAuthority` (no commit
without a valid session, active grant, active membership and, for a note,
the assignment-dependent visibility; a retry additionally requires the failure
to be eligible at commit), `AtomicEvidence` (result iff audit), `OneRetry`,
`EncryptedPush`. 34,172 states generated, 13,744 distinct. Controls:
`Unlocked` (`LocksHeld` off: the environment moves between the read and the
commit, `CurrentAuthority`), `PartialAudit` (`AtomicAudit` off:
`AtomicEvidence`), `RetryTwice` (`SingleRetry` off: `OneRetry`),
`Unencrypted` (`EncryptionRequired` off: `EncryptedPush`). Abstractions: one
actor and one scope; a session, grant and membership are booleans (the
per-handler role gates differ: notes and views need a mutating role, push
also admits ReadOnly, retry needs Admin, Manager or StudioManager); the
retry's `open` stands for both admitted statuses and `retryable` is not
modelled; mentions,
view contents and the retry's downstream execution are not modelled; the
model states the guards the handlers are read to apply and does not refine
the SQL. Decision in AUTHORITY-072.

The integration review found that the original `CurrentAuthority` omitted
failure eligibility even though `Resolve` could run between a stale read and
commit when locks were disabled. It now checks the current failure state. The
additional `ResolvedRetry` control permits only failure resolution and command
steps, keeping session, grant and membership unchanged; it must violate
`CurrentAuthority` when the stale retry commits after resolution. This isolates
the missing property from the existing general revocation control.

## Evidence

| Check | Where | Result (2026-10-12) |
| --- | --- | --- |
| Model and four controls | `scripts/verify-event-operations-formal.sh` | PASS locally; controls fail on their named invariant |
| Handler conformance (notes, retry, views, push against PostgreSQL and HTTP) | `scripts/lib/operations_conformance.py` via `scripts/test-booking-conformance.py`, CI | CI evidence; not re-run locally for this slice (the local harness stops on a stale `yaml` module before the invoice section, unrelated) |
| Specification gates | inventory, traceability, state machines, new surfaces | PASS |

## Left open

- Real push delivery remains outside this model. The integrated
  [failure replay specification](operations-failure-replay.md) separately covers
  downstream projection retry and settlement; neither model refines SQL.
- A repeated preference or token replacement writes an additional audit row;
  the model's single command per kind does not cover repeats.
- Mentions of members who lose their membership after the note commits are
  not revalidated later; the note is historical evidence.
