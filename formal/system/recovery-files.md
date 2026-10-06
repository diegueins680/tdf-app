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

## Executable evidence

`python3 scripts/test-recovery-files.py` creates actual synthetic archives and files.
Controls cover binary/Unicode content, long PAX names, empty directories, metadata,
links/FIFOs, private permissions, resource limits, mutation during capture, newly
added files, traversal, duplicate/unlisted/missing members, truncation, wrong bytes
and metadata, invalid manifests, extended attributes and numeric PAX headers.
The repository quality gate runs these checks. Linux execution and macOS checks
are distinct evidence; neither establishes an actual production content restore.
No filesystem formal refinement proof is claimed.

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
