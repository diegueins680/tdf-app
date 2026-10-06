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

## Empty disposable set

`original-disposable-absence.py` implements the first stage only when there is
nothing to remove: the complete Docker inventory, including stopped containers,
must contain exactly the saved three original CIDs, and the pending restore
creation marker must be absent. It checks marker absence before and after two
inventory reads under the fresh-boot epoch and shared restore reservation. Even
a dangling marker symlink, unrelated stopped container or lost observation
blocks the stage. It deletes no containers, markers, archives or directories.

This supports aborts before disposable creation. Existing or ambiguously created
disposables need a separate cleanup adapter with a durably recorded pre-create
identity; the legacy nonce/image marker alone cannot authorize adoption or
deletion. The actual fresh host boot must quiesce earlier daemon requests, and
privileged noncooperating creation remains outside the sampled boundary.

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
of all application authorization. Edge routing/TLS requires the separate adapter below.

## Original TLS edge boundary

`original-edge-recovery.py` requires the completed DB/API journal stages and the
same original reservation/configuration/cluster admission. It starts only the exact
original edge CID, only if stopped; a lost start response leaves pending intent.
Starting edge may immediately restore public requests and writes. It is not a
continuing maintenance boundary.

Four redacted probes use the same PID/pidfd/cgroup/network-namespace binding as the
API probes, with a closed `api|edge` selector. Edge transport connects only to
127.0.0.1:443 inside that held namespace, using api.tdfrecords.net for TLS SNI,
certificate hostname validation and HTTP Host. The default CA chain policy remains
enabled. TLS verification failure is terminal; ordinary initial connection refusal
can retry read-only readiness. Redirects, wrong revision, malformed public DTOs and
anonymous booking success reject completion. Database identity/history and all
three original services are rechecked before recording the observation.

This establishes the namespace-local TLS/routing path only. External DNS, host port
forwarding, firewall and public-network reachability require separate safe probes.
It does not complete timer restoration or the overall abort sequence by itself.

## Original timer boundary

`original-timer-recovery.py` accepts only the originally enabled/active timer
recorded by original admission, after the database, API and edge stages. Unchanged
unit files, no active or failed backup at sampled admission and all original services/database
identity are prerequisites. It issues one `systemctl start` only when the admitted
timer is inactive; an already active timer receives no start. An uncertain command
reply, changed unit, active/failed job at the closing sample or invalid observation leaves
pending intent and cannot replay in the same epoch. It does not enable units,
reload definitions, reset errors, terminate backup work or retry failed effects.

Successful observation restores scheduling only. It neither proves a completed
backup nor prevents the timer from dispatching future work. An immediately due
persistent timer may dispatch a job and make this narrow observation fail; that
failure must remain visible rather than be relabelled complete. A short job can
complete between samples and is permitted; these observations do not establish
dispatch exclusion or certify that job's backup output.

## Terminal observation

`original-recovery-completion.py` records `complete-abort` only after all five
preceding stages. It performs no start, stop, deletion or timer mutation. Before
and after eight fresh namespace-bound API/edge probes, it requires the same
original runtime, database identifier/history and restored timer configuration.
An active/failed backup at either sample, remaining restore marker or labeled
restore/canary container (including stopped containers) blocks completion. Health,
revision, public database access and anonymous booking denial must pass on both
transports; failure leaves durable pending intent and cannot retry in the same
epoch. Terminal journal observation retains the context-bound evidence hash.

Completion seals only this recovery sequence: normal release remains permanently
latched. Public network reachability, backup success, continuous writer exclusion
and whole-system conformance are not certified. Actual disposable removal and
the integrated controller remain separate obligations. The component fixture
exercises the real empty-disposable admission, without creating or deleting
restore/canary containers.

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
recovery adapter and namespace probes. `TDF_TEST_ORIGINAL_EDGE_RECOVERY=1` adds the
immutable Caddy image and actual TLS edge adapter. Its temporary nonce-owned CA and
explicit leaf certificates disable ACME; networks stay internal-only. Untrusted and
wrong-host certificates must reject before the trusted edge is qualified. The exact
trust file is removed even if other resource cleanup fails; an identity mismatch
fails and preserves the changed file. Portable real TLS socket controls additionally
show that a client-hostname-verification mutation defeats the hostname rejection.
`TDF_TEST_ORIGINAL_TIMER_RECOVERY=1` adds the actual registered systemd timer
restoration after edge recovery, without declaring the abort sequence complete.
`TDF_TEST_ORIGINAL_RECOVERY_COMPLETION=1` additionally exercises the terminal
adapter's fresh checks and seals the journal sequence in this component fixture.
Boot epochs remain synthetic. The separate
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
The new [shutdown supervisor](application-shutdown.md) addresses startup and HTTP
drain in newly built images, and its image gate requires exit0. It cannot
retroactively repair the old image's first stop. This is an open release-design limitation; no forced stop is qualified as a
clean capture by these results.

Identity-bound disposable cleanup, full coordinator,
operational key custody and terminal
recovery receipt remain unfinished. Do not use this library alone to stop or reboot
production. The aggregate requirement remains PARTIAL.


## Disposable creation records under construction

`disposable-creation-spec.py` reconstructs the existing physical-copy and canary
admission predicates from typed fields. It stores a hash of the fixed constructed
command; it never executes a serialized command. Source database, all three
original CIDs, nonce-derived name/directory, image identity, exact isolation and
bind-directory identity remain required. A surviving canary can be admitted using
its recorded database CID without starting or inspecting that missing dependency.
Regional configuration is reconstructed in a fixed order after canonical JSON
serialization. Initialization-time policy bytes are bound and later source drift
rejects publication/reading; trusted stable source installation is an assumption.

`durable-disposable-records.py` publishes private immutable descriptors outside the
strict journal record namespace. Each binds the complete prepared original
admission hash, plan hash and release nonce; the specification nonce must match.
Physical admission precedes the canary descriptor, which hashes its predecessor.
Publication failure closes the writer and retains evidence. A new writer cannot
adopt a pre-existing descriptor directory. Optional hooks in both actual creators
publish before marking/submitting creation; failed publication submits no Docker
request. Tests separately retain uncertainty when creation loses its response.

These components do not implement the complete cleanup protocol. A caller guard
must still be wired to the real journal and versioned common reservation. Plan
image binding, exact admitted physical CID cross-binding, complete inventory,
recorded fresh boot, canary-before-database removal, uncertain removal handling,
and release of only the matching marker remain open. Legacy marker adoption is
not authorized. The existing empty-set abort adapter remains the only complete
cleanup-stage adapter; no production eligibility follows from descriptor tests.
