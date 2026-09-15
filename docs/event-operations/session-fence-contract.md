# Authenticated session transaction fence

EO-003/EO-045/EO-051/EO-055: a party ID captured during authentication is not proof that its session
is still valid when an event transaction runs. Reuse the canonical `api_token` row; do not create a
parallel session table, token store, grant registry or client-controlled identity parameter.

## Operation and ordering

After the existing successful token/role authentication, capture an opaque in-memory witness from
that same token record: token row ID, authenticated party ID and SHA-256 of the credential. Never
serialize it or include its fields in Show/log/error output. Synthetic/internal users lacking a
witness cannot call the fenced event boundary. Bind the supplied actor AND current token owner to
the captured party; copying a witness into a modified user record must not confer another identity.

For each event GET, fresh transition and historical replay:

1. Start the event database transaction and lock the witnessed token row by ID with `FOR SHARE`.
   Do not filter active/party/purpose in an earlier snapshot. A key-share lock is insufficient:
   updates to `active` must conflict with this lock.
2. Validate the locked current row: exists, active, normal authentication purpose using the same
   canonical label predicate, exact credential fingerprint and exact captured actor/party binding.
3. On failure return fixed 401, without executing event SQL or writing event receipts/audit. On
   success execute the existing event read/command in this SAME transaction. Hold the token lock
   until commit or rollback, including waits on the event/feature lock. Event scope checks remain.

A revocation/delete/rotation/rebinding that commits first causes refusal (or a whole-transaction
serialization failure). An operation that locks first may finish before revocation commits; the
revoker waits. No response can retract information already authorized before revocation. Lock order
is token then event boundary; deadlocks with other writers must abort and surface the existing safe
503, not silently retry a suffix. PostgreSQL READ COMMITTED is the production pool default; test
REPEATABLE READ/SERIALIZABLE stale transactions abort and require fresh authentication on retry.

## Formal gate and concrete verification

`SessionFence.tla` abstracts one authentication/transaction, 32 token records, two actor identities,
with/without witness and read/new/replay operations. Authentication fixes the original identity and
credential; adversarial actor copying and arbitrary token changes happen before or during the
transaction. `CurrentBoundSession` requires accepted work to have a live, purpose-valid, identity-
and credential-bound session at its commit decision. Six negative configurations respectively omit
recheck, the lock, captured-party binding, fingerprint binding, purpose validation and witness.
Require the positive TLC configuration to pass and every mutation to exit 12 on that invariant
before feature implementation. Keep existing TLC and bounded Alloy checks. No new relational
entities are introduced; `EventStructure` and contextual permission models remain complementary.

Tests must use real `loadAuthedUser`, the production event handler and PostgreSQL. Exercise stale
captured users for GET/new/replay, inactive/deleted/rotated/rebound/reset-purpose tokens, absent or
copied witnesses, unchanged history, concurrent operations in both lock orders, cancellation or
rollback releasing locks, and RR/Serializable failure. HTTP tests must continue through real auth;
any test barrier must wrap a production action rather than replace authentication/SQL with mocks.

## Bounds, compatibility and limits

No token-column migration, client wire shape or extra authentication route is needed. The Haskell
authentication context gains an optional opaque witness; explicit fixture users use `Nothing` and
all constructors must be updated. Other domains retain their current behavior until separately
reviewed; merely carrying this witness does NOT fence their transactions or global role/catalog
changes. Event authorization uses current event scopes, not the cached global role/module list.

SHA-256 collision resistance is assumed. Normal database administrators must not reuse a deleted
token identity/credential. Existing explicit reactivation of the SAME credential reauthorizes it;
this does not implement permanent revocation epochs. For permanent invalidation, rotate credentials
or issue a new token and keep the old one inactive. Captured session witnesses are request-local,
never offline credentials. Full global-role revocation, MFA, token storage hardening/retention,
whole-app middleware/schema rehearsal and offline client behavior remain separate gates.

Rollback keeps event operations disabled and reverts this Haskell increment coherently (including
fixture constructors). Never leave event routes enabled with the old unfenced handler. No production
credentials, token reactivation, deployments or real-money operations are authorized by these tests.
