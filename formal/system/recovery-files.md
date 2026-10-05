# Private file recovery: DEPLOY-FILES-001

`ops/hetzner/recovery-files.py` provides file-tree capture and replay primitives for
the coordinated release bundle. They do not stop writers, capture a database,
select storage roots, encrypt data, transfer it off-host or authorize deployment.
The release executor and bundle coordinator remain implementation obligations.

`capture(source, destination)` accepts an absolute directory and creates an
exclusive mode0600 uncompressed tar in a private owned parent. It returns a private
manifest of relative paths, content hashes, sizes, owners, modes and nanosecond
modification times. Empty directories and the source-root metadata are included.
The source must be fenced against writes and nested mounts must be admitted by the
caller. Per-inode checks and a second complete content scan detect sampled changes;
they cannot establish a simultaneous database/files snapshot or fence a privileged
concurrent writer. A same-filesystem bind mount is not detectable by device ID.

Every path component is opened without following symlinks. Only directories and
single-link regular files are supported, bounded to100,000 entries and2GiB of file
content. Special files, links, special permission bits and unsupported extended
attributes are rejected rather than silently losing their semantics. Timestamps
outside the nonnegative signed64-bit nanosecond range reject capture. This is a
restricted recovery format, not a general backup of arbitrary Unix filesystems.

`restore(archive, manifest, destination)` requires a separately trusted manifest,
a private owned archive and a fresh destination under a private owned parent.
It never restores over existing content. Archive members must match the complete
manifest exactly: no path traversal, links, duplicates, absent or additional files,
wrong bytes, owners, modes or times. File creation uses exclusive descriptor-relative
operations, not `extractall`. Metadata is applied explicitly, directories last;
restored content is rehashed and compared before success. A non-root caller cannot
restore another owner's files. A successful return states only file-tree recovery,
with `coordinatedDatabase=false` and `offHost=false`.

Manifest paths, files and error diagnostics can contain private information. Keep
them with the protected bundle; public evidence should contain aggregate counts
and hashes only. A failed archive or partial restore remains private for diagnosis;
there is no automatic deletion, retry over a target or successful completion receipt.
The caller must durably publish and authenticate the bundle manifest and bind the
entire archive digest, including padding, before any later release admission.
An attacker able to replace both inputs is outside this primitive's trust boundary.

## Legacy application writable layer

`ops/hetzner/stopped-application-storage.py` retains an admitted Linux rootful
Docker container's actual root and mount namespace before its caller stops it.
Admission binds the full container ID, immutable image, process cgroup, pidfd,
start timestamp and configured mounts. A child checks the actual held namespace's
mount table; mounts covering or below `/app/uploads` are rejected. The host Python
must support `setns` and `pidfd_open`; namespace entry requires host privileges.
The helper does not stop containers or establish the production writer fence.

After caller-owned shutdown, capture requires that same instance to be exited,
without OOM/restart and with exit code0 or143. It reads `/app/uploads` through the
retained root descriptor, with no-follow component traversal and the same complete
metadata checks as normal file capture. Missing uploads are recorded explicitly.
Actual namespace and Docker identity checks repeat after capture. A restarted
instance cannot reuse the old descriptors. Descriptor ownership ends when the
context closes; the caller must capture before replacing the old container.
Privileged host tampering and concurrent external writers remain outside the
primitive's guarantees. No Docker archive metadata or storage-driver path is
assumed to be complete or stable.

`capture_directory_fd` borrows a caller-admitted directory descriptor and never
closes it. This allows capture after the original pathname/process disappears;
the caller remains responsible for source authorization and writer exclusion.

## Executable evidence

`python3 scripts/test-recovery-files.py` creates actual synthetic archives and files.
Controls cover binary/Unicode content, long PAX names, empty directories, metadata,
links/FIFOs, private permissions, resource limits, mutation during capture, newly
added files, traversal, duplicate/unlisted/missing members, truncation, wrong bytes
and metadata, invalid manifests, extended attributes and numeric PAX headers.
The repository quality gate runs these checks. Linux execution and macOS checks
are distinct evidence; neither establishes an actual production content restore.
No filesystem formal refinement proof is claimed.

`test-stopped-application-storage.py` checks lifecycle/mount/descriptor controls.
The opt-in Linux `test-stopped-application-docker.py` creates only a new64MiB
network-isolated synthetic container. It tests actual namespace retention through
shutdown, full file metadata replay, rejection of a real unsupported xattr, and
running/restarted-source rejection. Its image stop signal is explicitly SIGTERM;
image defaults must not make unrelated failures satisfy a negative control.
The fixture checks identity before every lifecycle mutation and retains the
durable reservation if cleanup cannot be verified. CI runs this fixture; production
container shutdown and production data recovery are separate evidence.

## One bound recovery bundle

`coordinated-recovery-bundle.py` packages exactly six caller-admitted trees:
`database`, `production`, `edge-data`, `edge-config`, `host-units` and
`legacy-uploads`. The production tree includes its assets, persistent uploads and
protected configuration/secrets. Host units are staged by the coordinator with
original metadata and source evidence; missing legacy uploads require an explicit
empty staged tree and separately retained absence evidence. The bundle helper
cannot establish that caller-supplied roots are the actual production roots.

All components share a source revision, Mobile revision, runtime-observation hash,
migration-manifest hash, release nonce and PostgreSQL system identifier. The helper
holds all source directory descriptors while capturing and rechecks every tree
and named root identity before sealing. Overlapping or identical roots, output
inside any source/workspace, missing roles and unexpected roles reject capture.
These checks complement the caller's full-duration writer fence and mount
admission; sampled rechecks do not establish an atomic snapshot.

The exclusive private outer archive contains exact inner archives and one canonical
private index with their full manifests/digests. The complete outer manifest and
digest must be retained as trusted evidence alongside the encryption receipt.
Replay requires that trusted receipt plus an independently expected binding. It
first checks/restores the outer bytes, then validates all six inner manifests and
digests before replaying any component into new destinations. No overwrite or
partial-success receipt is supported. Failed private output remains for diagnosis.
The existing2GiB aggregate archive bound also applies to the assembled bundle;
exceeding it rejects instead of silently dropping a component.

`test-coordinated-recovery-bundle.py` exercises exact six-tree metadata replay,
release-binding mismatch, source changes across component captures, pathname
replacement, missing/extra/overlapping roots, changed bytes and invalid inner
indices. The return value expressly does not establish database recovery, usable
secrets, encryption or off-host recovery. Those checks must consume the same bundle
through the coordinator before release. This is executable file correspondence,
not a filesystem refinement proof or an operational release command.

## Remaining coordinated recovery sequence

The first release executor should use one permanent lock and durable pending
journal, verified maintenance routing, stopped application/background writers and
database-session drainage. Preserve legacy writable-layer uploads before replacing
the old container. Only under that fence capture one identified database/globals,
assets, uploads and protected configuration/secret bundle; reject unaccounted roots.

Encrypt and copy the identified bundle off-host with a recovery key independent
of the production host, retrieve it, and restore **those same bytes** into isolated
targets. Verify schema, file contents/metadata and secret recoverability without
provider access. A fresh database dump is not proof that an older bundle restores.
Apply reviewed migrations explicitly with startup migrations disabled. Candidate
startup itself can write data before traffic reopens. Any subsequent recovery must
preserve those writes through compatible application recovery or forward repair;
never restore an older database over accepted production data.

This sequence is an implementation plan with explicit prerequisites, not a working
operator deployment command. Current command authority remains `ops/hetzner/README.md`.
