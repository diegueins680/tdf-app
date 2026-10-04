# Identity claims and credential lifecycle

Canonical requirements: `ID-ARTIST-CLAIM-001`, `ID-SESSION-001`, and
`ID-SESSION-002` in `requirements.json`. This contract corrects implementation
behavior; matching contact data and an existing credential-free artist never
constituted sufficient proof to assume that identity.

## Principal and artist claim

Public password signup creates a distinct Party and credential. A positive legacy
`claimArtistId` is rejected with 403 before database effects; nonpositive IDs are
malformed (400). Existing clients may decode the deprecated field, but must not
send it. Neither absent nor matching artist email authorizes adoption. Password
signup grants its canonical customer policy, never the verified-artist policy.

A selected artist survives web signup as navigation context. The new authenticated
account prepares the existing artist directory target and submits a claim. A
submission is `submitted`, not approved. Only a distinct reviewer holding the Admin role and Admin module may approve
management. This matches the directory feature contract and permits multi-role
Admin composition; it does not introduce the stricter whitelist used by credential
administration. Approval grants directory capability to the
claimant's own Party; it never transfers the artist's Party or credential. This
contract does not assert that the existing reviewer/evidence workflow has received
complete authorization, privacy or concurrency verification.

## Credential and session transitions

`userId` in the admin API identifies a credential, not a Party-wide disable flag.
Multiple credentials can belong to one Party. On disabling a credential, replacing
its password, changing its username, or completing password change/recovery,
invalidate the Party's existing `password-login:`, `google-login:` and
`password-reset:` tokens. Labels are trimmed and case normalized for revocation.
Re-enabling a credential does not reactivate those tokens. A different active
credential can authenticate again. Custom/unlabelled service tokens retain their
separate lifecycle; this operation is not an account-wide access suspension.

Password login and Google subject login/link must re-read the current credential
after waiting for the lifecycle lock. Password proof is checked against that row.
For Google, the verified issuer/subject binding remains the authority; email is
contact data. Public password change accepts explicit username plus current
password proof; a bearer token is an optional identifier source, not a substitute
for password proof.

Recovery consumes exactly one active challenge bound to the credential Party and
purpose. Two simultaneous confirmations can have at most one successful commit.
The winning transaction replaces the hash, consumes the challenge, revokes old
interactive sessions and creates its replacement session. An issuance/profile
failure rolls back all these effects. Lost responses do not make consumed recovery
challenges reusable; request new recovery through the supported flow.

## Transaction and lock contract

For existing accounts on PostgreSQL, the order is provider-subject transaction
advisory lock (Google only), Party `FOR NO KEY UPDATE`, credential `FOR UPDATE`,
then token mutation. Initial discovery is not authorization: revalidate after
waiting. Recheck username resolution and credential owner. Keep locks until the
outer transaction ends, including replacement-token insertion. Never commit an
outer transaction from a session helper. Read-only replicas cannot issue sessions;
reuse of an old active token on write failure is retired.

Party `FOR NO KEY UPDATE` is compatible with the `FOR KEY SHARE` used by event
operations and foreign keys. The Party-first order matches Social's locking order.
The SQLite unit-test adapter reserves its single writer using an UPDATE matching
zero rows (no row changes or row triggers), then re-reads the credential. SQLite
checks do not establish PostgreSQL interleaving behavior. Unsupported database
backends fail explicitly. Out-of-band SQL and bootstrap/seed writes are outside
this lifecycle protocol and must not serve as production credential management.

Revocation rejects subsequent token authentication. Generic handlers that already
accepted an old token are not automatically canceled; event and Social transaction
fences have separate contracts. Universal linearizable revocation across every
HTTP handler remains a traceability gap.

## Executable evidence and abstraction bounds

`SignupIdentity.tla` has one registrant, one existing artist, independent signup,
claim submission, approval and rejection. It checks principal preservation and
review-before-management. Controls permit public takeover, automatic grant on
submission, and identity reassignment on approval; each must violate its named
invariant. The abstract approval transition assumes an authorized reviewer; it
is not an implementation proof of reviewer authentication.

`CredentialLifecycle.tla` has one Party, one credential, one Google session, one
login and two competing reset requests. Operations can fail; disabling is an
environment action that also revokes the recovery challenge. It checks single-use recovery, no sessions after disable and
atomic challenge consumption. Controls remove serialization, commit before session
issuance, or omit Google revocation. No fairness/liveness property is asserted;
finite terminal states are intentional. It excludes multiple credentials,
credential relocation, provider cryptography, SQL isolation details, arbitrary
workers and already-authorized requests. Those need implementation tests.

`ServerAuthSpec` property-tests positive Int64 claim IDs against the actual
validator. `LoginPage.test.tsx` checks independent signup and preserved review
navigation. `identity-http-runtime.mjs` exercises the real candidate backend on a
dedicated local PostgreSQL database: contact-data attacks, denied side effects,
independent signup, submitted claims, scoped revocation, re-enable, controlled
lock barriers for simultaneous reset and both login/disable orders, and injected
session-insert failure. Existing
provider and Social/Event fence suites remain required. Model success and source
fingerprints alone do not establish implementation refinement or deployment.

The audit's two intended models and six named controls were locally exercised on
2026-10-04. Exact final-head HTTP/PostgreSQL execution and deployment remain pending;
no current conformance PASS is asserted here.

The baseline recovery challenges had no expiry. ID-SESSION-003 below specifies
the additive repair; its runtime and rollout evidence remain pending.

## Directory review refinement — ID-CLAIM-REVIEW-001

Directory administration requires both the Admin role and Admin module, matching
`directory.admin` in the generated feature registry. An additional Artist role
must not remove that capability. The stricter credential-administration whitelist
is a different policy. A reviewer must be a different Party from the claimant,
including when the claimant is an administrator.

Claim decisions lock the current claim row `FOR UPDATE`, then validate the current
state, write any actual transition, create the approval's manager grant, append a
`directory_audit_event`, and construct the response in one transaction. A losing
terminal decision returns 409; it cannot leave a rejected claim with a grant from
that approval. Failure writing the grant, audit or response rolls back the whole
operation. An identical-state retry returns the current receipt without rewriting
reviewer/timestamp/version, adding an audit event or restoring revoked management.
Rejecting a claim does not revoke unrelated independently authorized managers.

`DirectoryClaimReview.tla` bounds four reviewer categories, one claim initially
under review, two terminal decisions and one external manager revocation. Roles
are stable during the modeled operation; no fairness/liveness claim is made. It
checks grant/status consistency, Admin-role enforcement, separation of reviewer
and claimant, and no grant resurrection on replay. Four controls remove each
protection. This abstraction excludes the evidence-review judgment, claim
resubmission, other claims for the same profile, external SQL and dynamic role
revocation. Session/role acceptance still uses the normal request boundary; this
is not a universal in-flight revocation proof.

The actual HTTP runner covers module-only denial, Admin+Artist composition,
self-review denial, a deterministic approve/reject barrier, unchanged replay
receipts/audit count, separately revoked grants and injected grant-write failure.
Execution against the final candidate and independent immutable-SHA review remain
required before declaring implementation conformance.

The complete claim transition graph remains canonical in
`docs/music-directory/formal-model.yaml`. Review now implements its missing
`more_evidence_requested` and draft edges; same-state observations are allowed
only for known states. Direct claim creation atomically submits a request, without
persisting a draft. The review endpoint is restricted to independent Admin
reviewers for **every** edge, including resubmission/withdrawal; this does not
introduce a claimant self-service endpoint. The UI offers only the current state's
outgoing actions. The PostgreSQL HTTP runner independently reads the YAML and
checks all 49 state pairs plus seven unknown-target cases, including unchanged
rows/audit evidence on rejected or observational requests.

The design label `claims.approve` in the directory formal model and permissions
catalog maps to `isDirectoryAdmin` (Admin role AND Admin module) at the current
HTTP boundary, plus distinct-claimant and legal-state guards. It is not a separate
runtime permission lookup. This mapping also applies to all administrative claim
status changes; public professions confer none of this authority.

The review card displays the submitted evidence as escaped text and the claimant
Party identifier. Human review remains an environment assumption: neither a
filled note nor a model-checker result proves that the underlying evidence is
true or that an operator evaluated it correctly.

## Recovery expiry — ID-SESSION-003

A recovery challenge binds one `api_token` to one immutable `user_credential` ID
in additive `auth_recovery_challenge`. Labels remain in the `password-reset:`
namespace and are delivery hints only. Public email changes cannot redirect this
binding. Missing metadata, mismatched ownership/binding, future issuance and an
elapsed deadline fail without credential/session mutations. Existing tokens are
not backfilled: users must request a fresh challenge.

Issuance samples the database clock once and records UTC Unix seconds, with a
900-second lifetime. This is a TDF policy choice, not an OWASP-mandated duration.
The validity interval is `[issued_at_epoch, expires_at_epoch)`. Confirmation locks
Party → credential → token → metadata, rechecks the binding and samples the
current database clock **after** all waits. PostgreSQL uses `clock_timestamp()`,
not the transaction-start `now()`. Validity is required at atomic consumption;
a transaction may finish afterward. The database clock and its synchronization
are trusted. A clock before issuance fails; arbitrary clock rollback within a
window is outside this guarantee. The SQLite test adapter uses UTC epoch seconds
but does not establish PostgreSQL concurrency behavior.

Token deletion cascades its metadata; credential deletion restricts while a
challenge references it. No background expiry deletion is required for denial,
and expiry does not erase audit evidence. Retention duration and coordinated
account deletion remain separate open privacy obligations. Identity consolidation
must not move these bindings; its current retirement guard blocks Parties with
credential/token references. Token/metadata creation is one transaction; failure
leaves no new token and rolls back preceding challenge revocations. Consumption,
password replacement, revocations and replacement session remain one transaction.

Deployment requires the additive migration before the enforcing API. Stop/drain
**every** older recovery handler before admitting traffic to the new version;
a mixed fleet can still accept expired or metadata-free challenges through an old
replica. A rollback must preserve enforcement. Keep the table on recovery; there
is no supported destructive down migration or backfill that renews old links.
The canonical Hetzner routine release must enforce this sequencing and the
recovery floor before this change is production eligible.

`RecoveryExpiry.tla` bounds time to 0..3, an abstract deadline of 2, one request,
one optional metadata row and two credential bindings. It explores a lock-wait
interval and binding change before consumption. Four controls omit expiry, use
a stale sampled clock, admit legacy metadata-free challenges or ignore a changed
binding. It proves no liveness property and assumes eventual lock/transaction
behavior only in the concrete tests. It excludes cryptography, delivery,
credential/session side effects (covered separately), clock rollback and cleanup.
The arithmetic helper is property tested across Int64 inputs, including overflow
shapes and deadline equality; HTTP checks exercise the actual PostgreSQL clock,
controlled token lock wait, atomic metadata failure and bound recovery after a
contact change. Final candidate execution remains required.

Remaining recovery obligations include rate limits, secure hashed token storage,
notification/delivery retry policy and whether to replace automatic post-reset
login with a separate login step. The expiry repair does not claim those OWASP
recommendations are already implemented.

`CredentialLifecycleSpec` calls the actual `completeGoogleLogin` helper on the
fully migrated PostgreSQL fixture using a synthetic already-verified issuer/subject.
Controlled barriers exercise disable-before-login and issuance-before-disable;
the first must deny login and the second must revoke the issued Google session.
This boundary does not exercise provider signature verification or contact Google.
The runner fails on an empty selected suite; its execution remains required.
