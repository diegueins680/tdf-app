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

The guarded release runner enforces that rule before issuing any rollback deploy.
For a target containing `2026-09-18_provider_subject_identity`, the captured prior
commit must contain `c53b33e7ef868fb7b64f876199ed66be0f617efc` (reviewed subject
binding and explicit signup intent). Missing commit history fails closed. The
check is unconditional on binding counts: a canary can accept a binding after a
read-only count, so an empty table is not permission to restore legacy authority.

Preflight blocks the first rollout from `2f01b20b...` until an explicit
`--recovery-sha <full-commit>` supplies a distinct reviewed ancestor that preserves
this authentication contract. The recovery and target must have identical
migration manifests and expanded SQL checksums. Both images are resolved to
verified immutable digests before any database or machine mutation. For this
transition the already-reviewed provider merge `f1ff05e6f591cb87ef71a9b66c5abeefa4c40095`
is the compatible recovery source; its existing Build Image pipeline must finish.

Preflight and the release report label recovery per machine as
`compatible-rollback` or `compatible-forward-recovery`. If canary/fleet validation
fails with a legacy prior, the same guarded deploy/smoke path uses the verified
compatible recovery artifact, preserving the captured operational flags and
provider bindings. The original verification error remains in the report. Never
restore an older database or drop bindings. If recovery itself fails, inspect the
report, machine state and release lease, and repair the compatible artifact or
cloud failure through the documented guarded lane before claiming recovery.

Subsequent compatible prior images retain automatic guarded rollback and do not
require the override. Ancestry
assumes reviewed commits have not deliberately reverted the authority contract;
it does not replace review, artifact integrity or runtime checks.

Remaining work: the Live Session ingestion path still resolves musicians by email and
performs multiple database transactions, and older API/import clients can omit contact request
keys. Those paths require their own scoped submission identities and relationship review.
This follow-up does not claim comprehensive prevention until those remaining paths are repaired
and the client/backend rollout is verified.
