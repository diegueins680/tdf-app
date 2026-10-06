# Original service recovery: DEPLOY-JOURNAL-001

Abort recovery preserves current original data after the normal release has been
irreversibly latched. It never restores an older database over possible new writes.
The fixed service sequence is `remove-disposables`, `recover-db`, `recover-api`,
`recover-edge`, `restore-timer`, `complete-abort`. Sequence completion is a journal
fact, not independent evidence of application readiness.

`ops/hetzner/abort-service-journal.py` shares the original permanent release lock.
Its private chained records retain every attempt. An epoch opens only after a
recorded reboot intent and an observed changed boot on the same admitted host.
Every stage records a context-bound intent before calling its effect; observation
must match latch, epoch, boot, stage, targets and operation identity. A failed or
lost response leaves the stage pending. No same-boot retry or later stage is
allowed. A separately recorded reboot and another observed boot permit a new
attempt from the first stage, retaining the earlier uncertain history. This never
re-enables normal release continuation. Partial records deny recovery rather than
being deleted or repaired automatically. At most 512 records are accepted.

The database adapter `ops/hetzner/original-database-recovery.py` loads the separately
prepared original admission and compares it with the abort latch. It holds the
same permanent restore lock used by canonical rehearsal tools; closed, replaced
or foreign-process handles deny effects. After the durable stage intent it checks
actual IDs, immutable images, complete configuration and mounts, volume metadata,
root and assets/upload directory identities, root configuration files and unit
configuration. An active registered backup or unexpected running container denies
admission. Matching samples are not continuous exclusion of privileged writers.

Capture admission remains strict. The separate abort admission accepts a mixture
of original running services and exited services with exit 0, 137 or 143, excluding
OOM, dead, paused or restarting states. This permits in-place crash recovery and
never asserts clean shutdown or capture authorization. Before a database start,
no-follow checks require an existing PG17 cluster: `PG_VERSION` exactly `17\n`,
required directories and a regular 8192-byte control file; recovery markers and
external tablespaces are rejected. These are presence/structure checks, not a
control-file checksum or pre-start system-identity proof. They prevent the image's
missing-cluster initialization path. Privileged concurrent file replacement is
outside this sequential boundary.

The adapter starts only the exact stopped original CID. If already running, it
submits no start. It repeats only fixed read-only SQL readiness probes and requires
the saved database name, system identifier and complete migration ledger to match.
Any start error, mismatch or failed closing admission leaves the journal pending;
there is no stop/start repair cycle. PostgreSQL may modify its existing files during
crash recovery and resumed application writes. No physical byte equality, continuous
maintenance or whole-deployment recovery is claimed.

## Executable evidence and limits

Portable tests use actual journal files and locks with synthetic Docker, SQL and
boot observations. They cover stage ordering, same-boot replay denial, recorded
subsequent epochs, observation binding, publication failure, unchanged SQL identity,
no duplicate start for a running database and shared restore exclusion.

The owned Linux writer-fence fixture enables the additional database component
check only with `TDF_TEST_ORIGINAL_DB_RECOVERY=1`. It force-kills a real PG17 cluster,
checks missing/empty/symlink/wrong-major markers, requires denial before actual start,
then recovers the same cluster and committed sentinel after a second recorded
**synthetic** boot epoch. Its API and edge workloads are inert. The separate
`test-interrupted-release-recovery-linux.py` checks an actual owned-host reboot;
combining these results is not an end-to-end coordinator proof.

`AbortServiceRecovery.tla` bounds one release to boot values 0..2, six stages and a
13-entry effect history (12 legitimate submissions plus one duplicate witness).
There is no fairness assumption or liveness claim. Environment assumptions include
an authenticated host, truthful changed-boot observation, durable immutable journal
publication, original resource admission and validated callback evidence. Receipt
matching and cluster presence are abstract predicates. Five controlled mutations
must fail named invariants: same-boot epoch, absent durable intent, replay, missing
cluster initialization and unbound observation. The model does not refine Python,
fsync, Docker, kernel boot behavior, PostgreSQL recovery/checksums or HTTP readiness.

The original API/edge start and readiness adapters, identity-bound disposable
cleanup, timer restoration, full coordinator, operational key custody and terminal
recovery receipt remain unfinished. Do not use this library alone to stop or reboot
production. The aggregate requirement remains PARTIAL.
