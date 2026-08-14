# Emergency-administrator rollout gate

Status: blocked. This is a read-only readiness assessment, not authorization to create, activate,
or modify a production credential or security assignment.

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

## Production observation

The aggregate query was executed on 2026-08-12 inside a read-only transaction against the current
Fly.io PostgreSQL deployment. It returned:

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

No party IDs, names, usernames, emails, hashes, tokens, or credentials were selected or recorded.
The result proves that two active `Admin` rows do not currently provide two independently
authenticatable recovery paths.

## Exact action required

Before rollout, an authorized security administrator must use the existing reviewed account and
role-governance process to establish an active credential for a second distinct emergency
administrator (or approve a different second eligible account), without sharing credentials.
Then:

1. rerun the preflight after the canonical security cutover and require
   `databaseCoherentPaths >= 2` and `databaseReady=true`;
2. have each of the two operators independently authenticate and exercise the documented recovery
   path against the exact candidate revision;
3. record only the verification result, timestamp, candidate revision, and approving reviewer—no
   credential material—in the rollout evidence; and
4. abort rollout if either path fails or if the paths are not independently controlled.

Creating or activating that production credential is a security-sensitive external mutation and
is not performed by this candidate worktree.

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
