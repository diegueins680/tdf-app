# Cold PostgreSQL copy admission: DEPLOY-PHYSICAL-001

The physical recovery boundary starts a previously restored **complete PG17
cluster** in a disposable container. It does not capture production, stop writers,
establish off-host recovery, run a candidate, or deploy. The caller must supply a
trusted manifest and the expected cluster system identifier from a coordinated
capture. A successful synthetic test is not production recovery evidence.

`ops/hetzner/physical-postgres-recovery.py` admits only a fresh private
`/opt/tdf/backups/rehearsal-<nonce>/physical-data` copy. Before its first mutation,
it checks host mountinfo, rejecting backup-root/ancestor/descendant mounts,
including same-filesystem bind aliases. It then compares every byte and supported
metadata against the recovery-files manifest. The root must belong to the pinned
image's UID/GID999. PG_VERSION must be17. Complete internal WAL and required
cluster directories must exist. Symlinks, external tablespaces, live PID markers,
backup/recovery/standby signals and unsupported metadata reject admission.
The expected deployment layout uses the host root filesystem; an independently
mounted backup filesystem needs a separate reviewed topology contract.

Only after that comparison does the helper clear **the copy's**
`postgresql.auto.conf` and create fixed configuration under a separate read-only
container mount. Original archive and manifest remain unchanged. The receipt
distinguishes original-manifest, modified-copy and configuration hashes. This
matters because ALTER SYSTEM settings survive a physical copy. Production
configuration files are not loaded; the fixed configuration disables their
preload settings, archive/restore commands, SSL and external HBA includes.
Copied per-role/per-database settings remain and may affect sessions, including
session preload settings; this is not universal database sanitization. Network
and mount isolation contain those sessions. The copied role credentials remain private; the
isolated configuration permits only its local socket and loopback connections.
No `POSTGRES_HOST_AUTH_METHOD` initialization shortcut is used.

The same permanent restore lock, durable nonce/image pending marker and labelled
orphan admission used by the logical rehearsal cover this mode. Any uncertain
create or failed cleanup retains the reservation. Only the fully admitted owned
container can be removed; a lost create response is resolved by its nonce name
and full identity/isolation checks. No production-volume removal or broad pruning
is offered. Restarting a process does not clear uncertainty.

A coordinator can enter the one physical reservation before writer fencing and
call prepare/start only after restoring the captured bytes. It must not acquire
the same restore lock again. A dependent canary must use
`with clone.with_application(application): ...` before any creation. The canary
checks that registration before Docker access. Cleanup removes the application
first; a failed/uncertain removal or a still-paused/created application preserves
the database and durable reservation. The guard becomes inactive when its lock
scope exits, even after failure. The release journal separately tracks capture,
encryption and transport uncertainty; Docker cleanup cannot clear that history.

The database uses a disk-backed **copy**, not the logical rehearsal's256MiB tmpfs.
It has no external network or ports, a read-only root, UID/GID999, all capabilities
dropped,384MiB memory/no additional swap, half a CPU,64 processes and16MiB tmpfs
for its private socket. It mounts exactly the copy and fixed configuration.
At least1GiB free memory and2GiB free disk are required before reservation; the
file-copy primitive's2GiB/100,000-entry admission bounds remain unchanged. Those
checks do not reserve disk against unrelated privileged users. A future
coordinator must account for all archive, encrypted, retrieved and clone copies.

The image's initialization entrypoint is bypassed. A bounded600-second sleep
keeps the isolated namespace alive while the controller checks `pg_controldata`:
control version1700, exact system identifier and precisely `shut down` are
required. It then verifies the copy/configuration again before directly starting
`postgres`. **There is no initdb fallback.** Successful startup requires a real
query returning the expected cluster identifier and17.x server version. The
600-second container lifetime is an upper bound, not an extension of any release
deadline; all later migration/application checks must fit an explicitly budgeted
coordinator deadline. This module does not perform them yet.

Failure can leave private partial copies and diagnostics. A cleanup failure or
unknown Docker result must leave the shared reservation and prevent another
rehearsal. An operator must independently establish daemon-request completion and
admit the retained target before recovery. No old production database is restored,
and no possible new production writes may be discarded.

## Executable evidence and assumptions

`python3 scripts/test-physical-postgres-recovery.py` checks cold identity, crashed
and wrong-version state, table-space/live-marker rejection, content/configuration
tampering, source aliases, restrictive umask, network/mount/privilege admission and
required live reservation. Unit fixtures mock only host UID/mount observation
where an ordinary developer account cannot create Linux topology.

The explicit Linux-root integration command is:

```sh
TDF_PHYSICAL_TEST_IMAGE=pgvector/pgvector@sha256:REVIEWED_PRELOADED_DIGEST \
  python3 scripts/test-physical-postgres-docker.py
```

It creates only new synthetic data, initializes its fixture, makes a cold archive,
restores a separate copy and checks real bytes/ownership/configuration. It also
tests an actually killed cluster with its misleading PID marker removed, a lost
create response, and failed cleanup retaining its reservation. Its controlled
cleanup recovery has exact knowledge of its own completed requests; it is not a
general production-recovery command. Private synthetic archives remain for
inspection. No image pull, production source, account or provider is involved.
Each exclusively created empty fixture first demonstrates rejection of a real
POSIX default ACL, then removes only the two recognized POSIX ACL attributes
from that fixture before creating children. This handles hosted-runner ACL
inheritance without changing the production metadata guard or normalizing any
existing recovery data. Unknown attributes remain a failure.

`scripts/test-physical-application-docker.py` composes this boundary with the real
application canary. It initializes only new synthetic historical-schema fixtures,
captures a clean cluster, restores a separate copy, applies the canonical migration
batch twice and compares the full ledger. The application must match its supplied
revision and packaged migration batch, become healthy, fail readiness during a
real disposable-DB pause, and recover. A second real run injects application-cleanup
failure and requires both containers and the durable reservation to remain before
fixture-only, identity-checked cleanup. Build Image runs this after packaging the
tested executable. Publishing an image alone is not a passing release gate.

It also restores actual UID/GID1000 asset/private-upload fixtures, rejects altered
content manifests before application creation and verifies the sentinels through
the running image. Existing copy ownership is verified, never normalized.

This combined test requires at least2GiB available memory and the canonical TDF
application repository. The October5 production host has less than2GiB total RAM;
its online application rehearsal cannot meet that admission. The guard remains
unchanged. Synthetic CI coverage does not establish production-copy application
recovery; a coordinated offline stage needs its own explicit capacity admission.

The existing RestoreIsolation bounded model covers shared reservation/orphan
ordering, with its documented bounds and mutations. It does **not** model cold
PostgreSQL files, mount topology, copied-role semantics or byte equality. Those
properties have implementation controls and empirical integration evidence, not
a formal refinement proof. Docker/kernel/image integrity, honest fsync and no
concurrent privileged mount/file replacement are environment assumptions.

The PostgreSQL17 filesystem-backup and pg_controldata documentation govern this
boundary; Docker stop's possible SIGKILL is why stopped-container status alone
cannot qualify a cold copy. Sources and adoption decisions are in research.json.

The combined synthetic fixture explicitly initializes UTF-8 and checks it again
in the cold copy. The first packaged-image integration failed because no-locale
with libc had selected SQL_ASCII and a Unicode migration was rejected. Separate
actualPG17 synthetic reproductions confirmed that failure and passed all181
migrations plus replay underUTF8. This fixture correction does not rewrite an
applied migration or alter recovered production encoding. SQL diagnostics are
bounded and emitted only by the new-synthetic-source test helper after target
inspection; the shared production recovery helper continues to suppress them.

## Complete recovered bundle to disposable application

`recovered-application-content.py` connects the six-component bundle replay to
`PhysicalClone`. The caller must already hold its live shared reservation and
supply the trusted capture receipt, release binding and verified plaintext from
its retrieved ciphertext. The helper checks backup mount topology before any
replay write, matches nonce and cluster identity, replays all six components and
rechecks the trusted outer tree before using its inner manifests.

Database, assets and private uploads are then selected only from that replay.
The production component supplies assets and persistent uploads; a legacy source
uses the separately captured legacy-upload component. This selection is the
caller's admitted source mode. Directory subtrees retain captured metadata and
content hashes. UID/GID1000 ownership and traversal/write bits must already match;
no content owner or mode is repaired to make recovery pass. Verified directories
move to previously absent disposable targets before the existing physical
preparation records its clone-only configuration changes. The returned manifests
are private. The remaining configuration, edge and unit trees stay retained.

The combined Linux image gate now captures a complete synthetic bundle and uses
this helper to feed its real PG17/application run, including migration replay,
readiness failure/recovery, media sentinels and cleanup-failure retention. It also
runs the filesystem controls as root so both legacy and persistent UID1000 cases
execute; ordinary non-root runs explicitly skip those two cases. Controls reject
wrong nonce, lost reservation, corrupted bundle, changed subtree content, aliases,
existing destinations and rejected topology before replay. In this fixture the
bundle is local plaintext: it does not establish encrypted off-host custody,
production writer fencing, real secret usability or a deployment. Those remain
independent coordinator obligations; no new formal refinement claim is made.
