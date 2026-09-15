# Lifecycle receipt replay authorization contract

EO-045/051: a saved command receipt is not a permission grant. Replay is a historical read,
not execution of a new transition. It requires current event read access and the exact original
actor, event and request hash. Losing write authority but retaining event read access permits
historical replay only; a new command still needs its current transition authority.

Before any receipt/conflicting-key response, serialize the command with grant/ownership mutations
on the existing event-state row and sample the wall clock after acquiring the lock. Scope writers
advance a separate authorization epoch, not the event's business version. Stale RR/Serializable
snapshots abort; RC commands observe committed revocations. Identity/event movement is not a
supported scope-edit operation: use explicit revoke/issue, with separate audited APIs still pending.

| Case | Observable result | Persisted effects |
|---|---|---|
| Same actor/event/hash, current read access | Original historical response, `replayed=true` | No new receipt or transition |
| Revoked, expired or not-yet-valid access | `forbidden`, no state/version/receipt payload | Append denial audit; never overwrite original receipt |
| No read access, guessed or different command/hash/actor | Same authorization denial | No disclosure of receipt existence through conflict response |
| Current read access but mismatched actor/hash | `idempotency_conflict` | Append conflict audit; no new transition |
| Read-only downgrade, new command | Current write-authority guard rejects | Existing rejection semantics |

Authorization is evaluated at the command's decision point, not browser delivery time. Revocation
cannot retract information already delivered by an earlier authorized read. Time expiration is
checked with a fresh clock, not transaction-start `now()`, including after a lock wait. Deadlock or
serialization failure requires a whole-command retry with fresh authentication; never retry a
partial SQL suffix. This correction does not implement an offline queue or grant-administration API.

`ReceiptReplay` abstracts one immutable pre-existing receipt and one read attempt, eight possible
actor/event/hash binding combinations, three scope states, and clock ticks 0–3 with expiry at 2.
The positive configuration combines reauthorization, fresh time and serialization. Three negative
controls respectively remove each safeguard and must expose `NoUnauthorizedDisclosure` failure.
It does not model HTTP authentication, guest conversion or network delivery. Existing lifecycle
and Alloy relational checks remain complementary; PostgreSQL tests establish the SQL boundary.

The lifecycle API migration is an unpublished-to-production, unmerged migration outside the
production manifest. Its idempotent forward definition may be corrected in this PR chain; no
production checksum is rewritten. Rollback disables the API and drops its write function while
preserving receipts/audit and the additive authorization epoch. Never restore unsafe replay while
the API is enabled. The broader read-snapshot handler and exception-log redaction remain separate
security gates; do not claim that this command correction completes all authorization/privacy work.
