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

## Original API recovery

`ops/hetzner/original-application-recovery.py` continues only after the database
stage. It re-admits the same original runtime, restore reservation and database
identity/history, records the API-stage intent, then starts only a stopped exact
original API container. Already-running originals receive no start. Lost responses
and failed observations remain uncertain and cannot replay in the same epoch.

The fixed probes execute host Python in a held network-namespace descriptor for
the admitted API task, with matching container cgroup, pidfd liveness and closing
PID/start-time/configuration checks. HTTP goes only to loopback port8080, without
proxy use or redirect following. The probe interface permits `/health`, `/version`,
`/rooms/public` and anonymous `/bookings` only, with bounded response reads. Room
names/IDs and booking response bodies are never emitted. Version identity must
match the saved SOURCE_COMMIT/GIT_SHA and compatible metadata aliases. The public
room DTO must be valid and anonymous bookings must return401.

This design accounts for the deployed645f backend: its `/health` implementation
returns constant success; `roomsPublicServer` actually queries the database.
Therefore health alone never qualifies recovery. The public database-backed read
and independent system-ID/migration checks are mandatory. This remains a scoped
availability/security smoke boundary, not complete endpoint conformance or proof
of all application authorization. Edge routing/TLS are a separate unfinished stage.

## Executable evidence and limits

Portable tests use actual journal files and locks with synthetic Docker, SQL and
boot observations. They cover stage ordering, same-boot replay denial, recorded
subsequent epochs, observation binding, publication failure, unchanged SQL identity,
no duplicate start for a running database and shared restore exclusion.

Portable API controls also reject wrong revision, changed ledger, missing public
database readiness, an anonymously successful booking response, conflicting version
metadata, redirect/malformed/oversized bodies and response-data leakage.

The owned Linux writer-fence fixture enables the additional database component
check only with `TDF_TEST_ORIGINAL_DB_RECOVERY=1`. It force-kills a real PG17 cluster,
checks missing/empty/symlink/wrong-major markers, requires denial before actual start,
then recovers the same cluster and committed sentinel after a second recorded
**synthetic** boot epoch. Its default API and edge workloads are inert. Explicit
`TDF_TEST_ORIGINAL_APPLICATION_RECOVERY=1` instead provisions a new synthetic schema,
persistent uploads and real immutable backend image on internal-only Docker
networks, with selected optional workers disabled and synthetic credentials.
Other existing workers can run against that synthetic database; this fixture does
not claim universal worker suspension. It tests the real API
recovery adapter and namespace probes; edge remains inert and boot epochs remain
synthetic. The separate
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

The real immutable backend component experiment also observed Docker's60-second
SIGTERM stop ending in exit137. Capture correctly rejected this state; the fixture
then exercised original database/API abort recovery instead of labelling it clean.
Boot currently does not install an explicit Warp shutdown handler. A candidate
shutdown repair must supervise startup, prevent readiness/worker publication after
stop, preserve startup failure, and distinguish successful HTTP drainage from an
expired shutdown deadline. It cannot retroactively repair the old image's first
stop. This is an open release-design limitation; no forced stop is qualified as a
clean capture by these results.

The original edge start and routing adapter, identity-bound disposable
cleanup, timer restoration, full coordinator, operational key custody and terminal
recovery receipt remain unfinished. Do not use this library alone to stop or reboot
production. The aggregate requirement remains PARTIAL.
