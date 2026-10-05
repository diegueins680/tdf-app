# Messaging authority and transaction correspondence

Canonical obligations are `MSG-ACCESS-001` and `MSG-ATOMIC-001` in
`requirements.json`. Existing domain details remain in
[DM writes](../../docs/social/dm-write-boundary.md),
[DM reads/API](../../docs/social/dm-read-boundary.md) and
[web client isolation](../../docs/social/chat-client-isolation.md).
Those documents' historical receipts are not current-head execution evidence.

The consent policy intentionally distinguishes never-governed legacy compatibility
from canonical enforcement. Pausing a feature does not erase activation memory,
blocks, closure or consent. Administrator context does not bypass canonical policy.
Reads authorize within their statement snapshot; an overlapping later revocation
cannot retract already delivered content. Native UI cache isolation and complete
cross-device deletion are separate, unverified obligations.

Message creation has no request idempotency key. An accepted database commit whose
HTTP response is lost can be retried into a second message. The transaction-boundary
repair does not change that contract. It prevents a server-detected invalid or
denied database result from committing an operation and then reporting failure.
PostgreSQL sequence allocations may leave gaps after rollback; message rows and
thread timestamps must roll back together.

## ChatMutationBoundary model

`formal/event-operations/ChatMutationBoundary.tla` abstracts one authorized sender,
one thread, at most two sequential attempts and one staged message per attempt.
The effect counter represents message insertion and its thread update as one
transactional unit. `valid` abstracts the decoder's verdict, including known denial
envelopes and malformed results; it does not prove Aeson parsing or SQL policy.
No fairness is assumed and no liveness result is claimed. Bounded completion permits
stuttering. The model excludes competing senders, database isolation/lock graphs,
session revocation, provider effects, message delivery/retraction and sequence IDs.
Those need their separate domain models and actual PostgreSQL tests.

Safety properties require rejection without committed effects, one effect for an
accepted attempt, and validation before accepted completion. A lost response is
explicitly distinct from a server-rejected operation. Retrying a lost response may
produce a second accepted effect. The two controlled mutations commit before
decoding or omit rollback of a rejected staged effect; both must violate
`RejectedLeavesNoMutation`.

The implementation correspondence is the transaction enclosing
`TDF.Social.Chat.query` and response decoding. `scripts/social/ChatSpec.hs` replaces
the actual PostgreSQL send function in an isolated fixture with a wrapper that
performs the legitimate write and then returns invalid/denial envelopes. Tests
exercise bearer HTTP, compare all message rows and thread timestamps before/after,
restore the original function and verify the next valid send succeeds. These are
runtime negative controls, not a proof of whole-program refinement.

The shared formal gate runs the positive configuration and both mandatory failing
configurations. `scripts/social/test-http.sh` runs the PostgreSQL and HTTP tests;
its native mode uses a private throwaway cluster. Never point it at production.
