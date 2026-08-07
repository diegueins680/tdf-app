# Migration, rollout, and rollback

## Database changes

Two additive migrations create feature access requests/history and feature navigation preferences. They use foreign keys, uniqueness constraints, bounded text columns, timestamps, and indexes. No role, module, credential, preference, or history row is deleted.

Before production migration:

1. Export affected authorization and navigation tables to encrypted storage and record the non-secret backup identifier.
2. Verify at least two coherent emergency administrator credentials. Production currently has only one, so assignment corrections are stopped.
3. Run migration forward/rollback tests on an isolated PostgreSQL database.
4. Deploy the uniquely identified backend revision, run `/health` and `/version`, then apply the additive migrations.
5. Verify representative anonymous role fixtures through API and UI.
6. Monitor error rate, 403 rate, unresolved destinations, access-request transitions, and preference failures.
7. Rotate historical seeded credentials/tokens in a separate bounded operation using the secure credential-delivery channel; never combine password rotation with role/module corrections.

Stop immediately for privilege escalation, administrator lockout, elevated authorization failures, broken primary workflows, migration errors, or abnormal application errors.

## Rollback

1. Disable access-request/navigation UI entry points by reverting the application revision.
2. Roll back backend traffic to the prior Fly revision and web traffic to the prior Cloudflare deployment.
3. Keep additive tables in place during immediate application rollback so request/history/preference data is not lost.
4. If schema rollback is later approved, export the two new table groups, verify the export, then drop only the explicitly named new indexes/tables in reverse dependency order using the reviewed rollback SQL.
5. Re-run health, version, authentication, strict-admin, and representative role smoke checks.

No production deployment, migration, assignment correction, or backfill has been executed from this branch yet. A merge is not required to create a preview, but production rollout must use the reviewed pull-request revision and all gates in `verification.md` must be complete.
