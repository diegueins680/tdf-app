# Provider identity and contact creation prevention

Google authentication now uses the verified issuing system and exact subject, persisted against
an existing credential. Email remains contact data. The token's configured audience, issuer,
verified email, subject shape and expiration are checked before any database work. No existing
email-based Google account is backfilled or linked merely because its email matches.

An unbound Google identity can connect an existing account only with that active account's
current username/password, through the web or mobile login flow. Existing Google-only users
can first use password recovery. Separate people with shared contact details can deliberately
create separate accounts through the consented signup flow with `createNewAccount: true`.
Legacy clients that only resend consent cannot create an account for an unbound subject. A new account receives a stable
provider-derived username; an existing binding survives email changes. Binding one subject to
a different credential is rejected, even when both credential passwords are known. Contact
fields, passwords, roles, consent, and ownership are not transferred.

A transaction-scoped lock on issuer plus subject serializes signup, linking and retries. The
composite primary key is the final database constraint. The binding records the verification
method and creation time; PUBLIC has no privileges. The application database role is currently
a superuser, as documented in the reconciliation runbook. This change does not claim database
role isolation that the deployment does not have.

The additional contact creation forms (bookings, companies, leads and the
retained Records booking form) now reuse actor-scoped creation keys after failed responses or
later form-step failures. Independent form entries and explicitly completed or abandoned submissions receive
separate keys even when their contact fields are identical. This creates no shared-email/name
uniqueness rule and no cross-user contact lookup.

Validation: the isolated PostgreSQL runner checks wrong passwords, disabled credentials,
shared email, subject reassignment, email changes, concurrent linking, idempotent retries and
transaction failure. The full backend suite also validates token expiration boundaries. Web and
mobile tests exercise the explicit linking choice and submission of credential proof; contact
creation tests cover lost responses, retries, independent scopes and form instances.

Run `sh scripts/test-provider-identity.sh` with local PostgreSQL connection variables. It creates
and removes only its own disposable database. The migration is append-only in the existing
release manifest. Its schema rollback refuses to drop any established binding. Routine identity
merge rollback remains the operation-specific command in [reconciliation.md](reconciliation.md).
Revoking an authentication binding requires an authorized account-security review; rolling an
application back must not restore email-only Google authorization as a recovery shortcut.

Remaining work: the Live Session ingestion path still resolves musicians by email and
performs multiple database transactions, and older API/import clients can omit contact request
keys. Those paths require their own scoped submission identities and relationship review.
This follow-up does not claim comprehensive prevention until those remaining paths are repaired
and the client/backend rollout is verified.
