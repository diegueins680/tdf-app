# Journaled capture: DEPLOY-JOURNAL-001 / MEDIA-RECOVERY-001

`ops/hetzner/coordinated-capture.py` composes the private six-tree bundle with the
existing journal, stopped-source observation, scheduler/process admission and
offline capacity policy. It is a library, with no shutdown or deployment CLI.
The caller must retain the shared physical-recovery reservation, original
application-root descriptors and writer fence throughout capture. OS/kernel/root
trust and exclusion of noncooperating privileged writers remain assumptions.

The release plan's `sourceRevision` and `mobileRevision` identify the audited
candidate that governs the bundle/migration contract. They do not claim the
source database ran that candidate. `runtimeSha256` must equal the independently observed
`runtimeConfigurationSha256` of the canonical source containers (their IDs, image,
Config, HostConfig and destination-sorted complete Mounts). It is not a hash of an
arbitrary inspection report. The digest includes the actual source image identity.

Admission requires exactly the completed maintenance/writer/database-stop prefix,
no pending intent, no possible new writes, the same nonce/database identity and
plan bindings, no disposable container creation/start, and a private reservation
directory. It reobserves canonical stopped sources and the inactive registered
backup service/timer. Actual source-root mount aliases or nested mounts reject;
reviewed host scheduler/process samples and a closing source sample must pass.
Those samples do not prove continuous writer exclusion or PostgreSQL clean
shutdown; the coordinator separately verifies the actual database control state.

Before capture intent, walk all four direct roots and the retained legacy upload
root. Compute an archive-size upper bound using the file primitive's PAX allowance,
the maximum index size, component/outer padding and two bounded unit files.
Reject bounds over2GiB and apply the explicit ten-copy/two-allocation-block
[offline budget](physical-recovery.md#offline-capacity-admission). These are
conservative policy bounds, not resource reservations or a no-ENOSPC proof.

After durable journal intent, copy exactly the admitted two original unit files
with bytes, uid/gid, mode and mtime. Reject unsupported extended metadata instead
of silently stripping it. The host-unit staging directory is deliberately new.
Capture/replay the retained legacy uploads with their actual metadata. If absent,
record explicit absence evidence and stage a new empty UID/GID1000 mode0700 root;
this is newly provisioned storage, not restoration of fabricated historical
metadata. With persistent uploads, the direct production tree already contains
them and the unused legacy component is explicitly empty.

Capture all six trees into one bound archive. Before success, rewalk all four
direct roots and the retained legacy root and recheck original unit identities,
bytes and supported metadata. Reject observed changes, including absent uploads
becoming present after staging. Reapply source/scheduler/process admission.
Exclusively write and fsync `coordinated-capture-receipt.json` and its private
parent before the journal can record its hash. The private receipt includes the
operation context, bundle manifest, legacy presence evidence and capacity sample.
A final admission follows receipt persistence. Never publish its private paths or
contents as public audit evidence.

Any error retains artifacts and pending intent. A saved receipt alone is not a
completed journal observation and never authorizes retry. This helper implements
no encryption, off-host retention, key custody, database recovery, service restart,
rollback or interrupted-intent recovery. It reports those unverified properties
explicitly and cannot authorize deployment.

`test-coordinated-capture.py` uses real private journals, actual archives and
synthetic host observations. Portable controls cover authority, scheduler/capacity
denial before effects, actual-root aliases, complete unit set and PAX size
bounds. Linux-root cases exercise all absent/present/persistent variants with
actual UID1000 metadata, archive replay, receipt failure, late source/legacy/unit
changes, late process rejection and unit xattrs. A mutation removes both source
xattr admissions and must make the named rejection test fail; ordinary archive
checks remain enabled. The hosted image workflow runs these cases as root.
These are executable implementation controls, not a new formal refinement proof
or an actual production capture. The bounded ReleaseJournal model covers ordered
intent only; its documented abstraction exclusions still apply.
