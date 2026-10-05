# Scheduled logical archive boundary — DEPLOY-BACKUP-001

The installed daily timer was observed active on2026-10-05, last triggered at
02:34:23UTC. Installed script/service/timer hashes matched their then-current
repository bytes. Service exit0 establishes only that the old script completed;
it does not prove restoration or disaster recovery. That script inherited Docker
and libpq routing, omitted cluster globals and tested only an archive listing.

The repaired `ops/hetzner/backup-postgres.sh` delegates to the adjacent reviewed
`backup-postgres.py` and `inspect-runtime.py`. All three must be installed together
in the root-owned `/opt/tdf/production` directory. Source changes alone do not
update the installed timer. Installation and an actual scheduled run remain
separate current-revision evidence obligations.

## State and admission

`idle -> locked -> source-admitted -> database-archived -> globals-archived ->
archive-listed -> source-rechecked -> receipt-durable` is the only success path.
Any failure exits nonzero. Partial files are retained in a unique private run
directory. `complete.json` is authoritative only when valid and no `failure.json`
exists; incomplete directories are never admitted as complete backups. A process
kill can prevent failure reporting, so file presence alone is insufficient.

The permanent nonblocking lock serializes this scheduled job. The directory and
lock must be private, owned by the effective operator, nonsymlink filesystem
objects. Runs never overwrite an archive, remove prior evidence or unlink the
lock. Before any dump request, a private durable pending marker binds the run and
source container. A killed parent releases flock even if its Docker request still
runs; the marker blocks retries, including when only its partial publication exists.
Only full completion clears it. On failure, an operator must establish that no old
request remains in flight, retain the failed archive evidence, and clear that exact
reservation before retrying. A retry creates a new UUID directory; it does not
convert a failed run into a success. The CLI fixes target/output and requires root.

The local Docker socket, container identity, production project/directory,
immutable image, network and named PostgreSQL volume are checked. Child mounts,
missing/duplicate/noncanonical PGDATA and a different effective data_directory
reject admission. Fixed local-socket SQL returns one boolean using the existing
local postgres role in a read-only session: the restricted inventory role has no
new grants and accepts no additional arbitrary SQL. The same source must pass
again before completion. These are sequential observations, not a fence against
a concurrent privileged operator replacing configuration between observations.

Each database utility has a cleared environment, explicit Unix socket/port/role,
read-only transaction defaults and timeout. Dumps are size bounded, mode0600 and
fsynced. Completion records sizes, SHA256 hashes, tool hashes, timestamps and
source identity, rejects changes to startup-captured tool hashes, then publishes exclusively and syncs the directory. Diagnostics
contain fixed stages only; globals may contain password hashes and remain private.

## Scope and recovery

The online pg_dump archive is database-consistent under PostgreSQL's snapshot
guarantees. Globals are a separate snapshot. `pg_restore --list` checks readable
archive metadata; it is **not a restore test**. The completion receipt explicitly
sets restored=false, offHost=false and coordinatedAssets=false. No provider call,
customer communication, database write, retention deletion or release occurs.

The separately guarded restore rehearsal remains mandatory for recovery evidence.
This scheduled job does not establish matching assets/uploads, secret recovery,
off-host copies, point-in-time recovery, tablespace/WAL filesystem integrity or a
coordinated release backup. Host compromise and concurrent root-level reconfiguration
are outside its boundary. A restore must preserve later accepted financial and
identity evidence; a database rollback is not an application rollback strategy.

## Executable correspondence

`scripts/test-hetzner-backup.py` exercises the actual orchestrator with temporary
private files and controlled process results: dump/global/list failures, empty
archives, overlapping locks, unsafe filesystem objects, storage redirection and
source changes must not create a passing receipt. Collector and access-helper
tests reject alternate mounts/configuration and false effective-storage evidence.
These tests check implementation branches; no whole-system formal proof or
scheduled production success is claimed.

`LogicalBackup.tla` checks two distinct runs with one request per run. Starting
work abstracts successful lock acquisition, durable reservation and child launch;
child completion abstracts the conjunction of both dumps and readable metadata.
Parent death releases the lock while the child may continue. Operator recovery
assumes it can establish that the old child has ended. Publishing abstracts final
source checks and successful durable receipt publication; arbitrary concurrent
privileged source changes within that step are excluded. No fairness is assumed
and no eventual completion or recovery is claimed. Filesystem fsync semantics,
PostgreSQL consistency, archive contents, network transport, off-host copies and
the full process-to-model refinement are not proved by this model.

Safety requires at most one in-flight backup, completed work behind every receipt,
and source agreement at publication. Three controlled variants respectively ignore
the durable pending marker after parent death, admit failed work, and omit source
binding. TLC must fail each with its specific invariant. The actual parent-kill,
process-failure and companion-replacement tests exercise the corresponding code
boundaries. The finite result is bounded verification, not a universal proof.

Primary guidance: PostgreSQL17 [pg_dump](https://www.postgresql.org/docs/17/app-pgdump.html),
[pg_dumpall](https://www.postgresql.org/docs/17/app-pg-dumpall.html) and
[pg_restore](https://www.postgresql.org/docs/17/app-pgrestore.html).
