# Emergency-administrator rollout gate

Status: pre-migration database gate ready; independent operator exercise pending. This document
records privacy-safe rollout evidence and does not contain credential material.

## Gate definition

Production rollout requires two independent emergency-administrator paths. A database-coherent
path has:

- an active assignment to an active role marked `emergency_administrator`;
- at least one active `user_credential` for a distinct assigned party; and
- active grants for `admin.access`, `security.read`, `security.create`, `security.review`,
  `security.approve`, `security.assign`, and `security.emergency-recover`.

The database check is necessary but not sufficient: both operators must also complete an
independent login/recovery exercise against the candidate revision. Shared credentials, an
unverified password, or two role rows belonging to the same party do not count as two paths.

`tdf-hq/sql/preflight_security_emergency_readiness.sql` implements the reusable read-only check.
It supports the legacy production schema and the canonical security registry, uses short query
timeouts, and emits aggregate counts only. The legacy result can satisfy the pre-migration
credential-count prerequisite, but cannot prove permission coherence because that schema has no
persisted permission graph, so it always reports `databaseReady=false`. The release runner repeats
the check after schema migration and refuses to deploy application Machines unless canonical mode
then reports two coherent paths.

## Production observations

The initial aggregate query was executed on 2026-08-12 inside a read-only transaction against the
current Fly.io PostgreSQL deployment. It returned:

```json
{
  "schemaMode": "legacy",
  "activeEmergencyAssignments": 2,
  "distinctAssignedParties": 2,
  "authenticatableParties": 1,
  "databaseCoherentPaths": null,
  "preMigrationReady": false,
  "databaseReady": false
}
```

On 2026-08-14, after explicit owner authorization of the second operator, production was protected
by Fly volume snapshot `vs_LRqNAqabQP5UoV2lo3JRqy`. A bounded transaction then:

- preserved the single credential deterministically tied to the account's unique primary email;
- reversibly deactivated one duplicate active credential;
- activated the account's unique legacy `Admin` assignment; and
- wrote two audit events with a shared production-only correlation ID and the snapshot ID.

The transaction asserted both the reviewed before-state and the expected after-state, including
exactly two distinct authenticatable administrators, and was first exercised with `ROLLBACK` before
the identical committed run. A recovery request then returned HTTP 200, produced exactly one active
reset token for the designated account, and produced no password-reset failure in the available Fly
log buffer.

The reusable read-only preflight was rerun at `2026-08-14T00:12:43Z` and returned:

```json
{
  "schemaMode": "legacy",
  "activeEmergencyAssignments": 3,
  "distinctAssignedParties": 3,
  "authenticatableParties": 2,
  "databaseCoherentPaths": null,
  "preMigrationReady": true,
  "databaseReady": false,
  "manualIndependentLoginVerificationRequired": true
}
```

The extra legacy assignment is an inactive-credential marker and does not count as an
authenticatable path. No names, usernames, emails, password hashes, tokens, or credential material
are recorded in this evidence. The legacy result satisfies the pre-migration gate only; canonical
permission coherence must still pass after the security-registry migration.

## Exact action required

Before rollout, the designated second operator must personally open the recovery email, choose a
new private password, and complete an independent login/recovery exercise without sharing the
credential. This step cannot be performed or attested by the release operator. Then:

1. record the two operators' pass/fail results, timestamp, exact candidate revision, and approving
   reviewer without credential material;
2. rerun the preflight after the canonical security cutover and require
   `databaseCoherentPaths >= 2` and `databaseReady=true`;
3. deploy application Machines only after the post-migration canonical check passes; and
4. abort or roll back if either operator exercise fails, if the paths are not independently
   controlled, or if canonical permission coherence reports fewer than two paths.

## Recovery and rollback

The preferred rollback before canonical cutover is the reviewed row-level reversal identified by
the production audit correlation: deactivate the added legacy `Admin` assignment, restore the
duplicate credential's prior active flag only if the security reviewer explicitly requires the
exact legacy state, and deactivate any unused recovery token created by this exercise. Run those
updates in one transaction with the same before/after assertions and append compensating audit
events; do not delete the role, credential, token, or audit rows. If row-level reversal cannot be
proven safe, restore snapshot `vs_LRqNAqabQP5UoV2lo3JRqy` into a new Fly volume, verify its counts,
and perform the documented database-volume replacement rather than overwriting the live volume in
place.

## Candidate verification

- The preflight returned canonical mode, `transactionReadOnly=on`, one coherent path, and a closed
  gate against the seeded disposable PostgreSQL database.
- An isolated legacy-schema fixture returned `preMigrationReady=false` for one active credential
  and `preMigrationReady=true` after adding a second distinct active credential, while correctly
  keeping `databaseReady=false` until canonical permissions exist.
- The PostgreSQL security integration passed inside a rolled-back transaction. Negatives rejected
  removal of the final emergency assignment, a critical grant, the last active credential, the
  last active emergency role, and deactivation of a critical permission, action, or module row.
- The focused Haskell security suite passed 17/17 examples; the release-runner suite passed 24/24
  tests, including fail-closed parsing and pre-/post-migration gate behavior.
