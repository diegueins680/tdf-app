# Original deployment admission: DEPLOY-JOURNAL-001

`ops/hetzner/original-deployment-admission.py` records actual deployment identities
before any shutdown intent. It composes existing canonical Docker storage and
systemd admission with read-only PostgreSQL identity/history queries and private
filesystem evidence. It performs no service or database mutation.

Under the permanent release lock and caller-held common restore reservation,
`prepare` requires exactly the original journal plan. Its runtime hash must match
the plan. The destination must be a separate already-private directory, durably
retained at the coordinator's configured recovery location. A successful return
binds a private admission document and readiness receipt to that release and plan.
Never publish the document: full original container configuration may contain
credentials. Only its digest and explicitly safe operational facts may leave the
host.

The saved original deployment contains complete canonical container IDs, image
identities, Config, HostConfig and destination-sorted mounts; volume metadata;
no-follow recovery-root identities; root-level configuration file identities and
hashes; original validated unit hashes and enabled/active timer state; actual
PostgreSQL system identifier and ordered migration IDs/checksums/source revisions.
Database queries use an explicit local Unix socket, cleared libpq environment,
read-only transaction/default, statement timeout and fixed SQL. They read no
customer records. The existing `postgres` role is used for control metadata.

Assets and explicitly mounted uploads receive separate directory identities.
The sampler opens the actual API task/root, checks its exact container cgroup,
retains a pidfd, and traverses destinations without symlinks. Actual live mount
identity must equal the host bind-source directory identity; PID exit or changed
Docker PID/start time denies admission. Replacing a host directory at the same
source path therefore cannot silently become the original recovery target.
Mutable files inside those directories are deliberately not frozen by this
pre-shutdown record; coordinated capture establishes their later stopped content.

The helper samples before exclusive publication and again afterward. Drift leaves
the original private evidence file, but no successful preparation receipt. It
never overwrites or automatically retries either artifact. The readiness receipt
is published only after the closing sample; recovery loads it using the configured
private location and exact release/plan identity, then verifies the admission hash.
Matching samples are not continuous exclusion of privileged writers. The
coordinator must retain its reservations, revalidate relevant state before effects
and bind this document into the [abort latch](interrupted-release-recovery.md).
This helper alone does not authorize shutdown or implement runtime restart.

Portable tests exercise real evidence publication, digest/plan checks, late drift,
changed boot, refusal after any shutdown intent, same-directory denial, source
symlinks and permitted mutable content changes. Host/Docker/SQL observations are
synthetic there. The explicitly owned Linux writer-fence fixture additionally
records actual three-container/PG17/systemd admission, checks its system identifier,
rejects an actual live bind-directory substitution, and then exercises shutdown and
legacy-upload replay. No production or complete recovery claim follows. The
ReleaseJournal and AbortRecovery models abstract admission truth; neither formally
refines this sampler or its kernel/Docker interactions.
