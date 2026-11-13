# Guarded routine Hetzner release (DEPLOY-RELEASE-001)

`ops/hetzner/guarded-release.py` runs on the production host and performs one
backend release as the twelve stages of the [release journal](release-journal.md).
`scripts/guarded-release.mjs` drives it from the operator's machine. This is the
pragmatic executor the owner chose on 13 November 2026. It deliberately does not
finish every obligation of draft PR #495. Its limits are listed below.

## Inputs, derived rather than typed

| Input | Source |
| --- | --- |
| Candidate revision | Operator, a full commit |
| Candidate image | Registry `Docker-Content-Digest` for the image tagged with that commit |
| Migration ids and manifest hash | `scripts/production-migrations.json` blob at the candidate commit |
| Mobile revision | `tdf-mobile` gitlink at the candidate commit |
| Tool code | Git blobs at one committed tool revision, hash-checked on the host |
| Encryption tool | `age` pinned by `ops/hetzner/recovery-tools.json`, archive and binary hashes verified |
| Recovery recipient | Public age recipient from the key custodian; the private identity never reaches the host |
| Off-host target | Storage Box host, port, sub-account and directory; host key pinned in `/opt/tdf/release-control/storage-known-hosts` |

`plan` observes the running API and database identities, configured images,
public revision and Caddyfile digest. It then pulls the candidate by digest and
records the journal plan. The plan's `runtimeHash` binds the observation and
every parameter. Each later phase recomputes it and refuses on any drift, and
refuses tool code from a different revision than the one planned.

## Stages and phases

| Phase | Stage | Effect and acceptance |
| --- | --- | --- |
| backup | maintenance | Original Caddyfile digest must match the plan; overwrite the bind-mounted file in place; edge sees it; reload; public endpoint returns 503 with the maintenance marker |
| | stop-writers | Stop the backup timer and API; no running backup; the database network has only the database; no other client backends; record ledger and system identifier |
| | stop-database | Clean stop; `pg_controldata` in a network-less read-only container reports `shut down` with the same system identifier |
| | capture | Cold file capture of the database volume and `/opt/tdf/production` (configuration, secrets, assets, uploads) with content manifests; bundle archive |
| | encrypt | Pinned age to the custodian's recipient; trusted record binds release, plan, hashes and sizes |
| | retrieve-off-host | Upload bundle and record over SFTP; a fresh connection downloads it; bytes and size must match |
| | restore-isolate | Replay the captured database into the reviewed physical isolate; it must start with the same identity and migration ledger |
| migrate | start-database | Healthy; identity and ledger unchanged; API still stopped |
| | migrate | Candidate image in `TDF_MIGRATION_PRECHECK_ONLY` mode applies its reviewed batch; every manifest id must be in the ledger |
| cutover | start-candidate | Replace exactly the one `TDF_IMAGE` line that holds the recovery image; start the API; container runs the candidate |
| | verify-candidate | `/health` is ok with database ok; `/version` is the candidate commit; every smoke path returns 200; optional root-only login probe |
| | reopen | Restore the original Caddyfile; public `/version` is the candidate commit; restore the backup timer |

Each phase needs the first twelve characters of the candidate commit as
confirmation, which the operator gives only after owner approval. Public traffic
gets the maintenance response from the first stage until `reopen`.

## Rollback

`rollback` needs its own confirmation (`rollback-<12 characters>`). It refuses
once `reopen` has started. From that point users can have written data, and
restoring would discard it.

- **Before any migration stage:** restore `.env` if changed, start the database
  if stopped, check the ledger equals the pre-release capture, start the recovery
  image, verify its `/version`, and restore the edge.
- **From `migrate` onward:** the candidate may have changed the schema. The
  database stays fenced from users until reopen, so the cold capture is still an
  exact copy. Rollback stops the database, replays the capture into a staging
  directory, keeps the post-migration data directory for review, swaps the
  verified copy into the volume, checks identity and ledger, and continues as
  above.

## Not established

- Writer exclusion covers the compose API, the backup timer, database-network
  peers and database client backends. Privileged host processes outside these
  are not fenced.
- The edge certificate volumes are not captured; certificates are reissued.
- The Storage Box account, its overwrite/deletion protection and custodian
  key recovery need the acceptance sequence of the recovery custody setup
  proposed in draft PR #721. A synthetic decrypt by the custodian is still
  required.
- The journal has no `abandon` record. After a rollback the journal stays
  interrupted, and the next release needs a reviewed extension first.
- Tests drive the real journal, release files and bundle archive against an
  in-memory host model. Docker, PostgreSQL, Caddy and SFTP behaviour need the
  staging rehearsal and the first supervised production run.
