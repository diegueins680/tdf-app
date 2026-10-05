# Private upload persistence: MEDIA-PERSIST-001

Feedback attachments (`ServerFeedback.uploadH`) and live-session riders
(`ServerLiveSessions.storeRiderFile`) use relative `uploads/feedback` and
`uploads/live-sessions` paths. In the production image's `/app` working directory,
these must resolve into the private host bind `/opt/tdf/production/uploads` mounted
at `/app/uploads`. This directory is separate from public `/data/assets`; mounting
it must not add an anonymous serving route. Database paths retain their existing
relative values. No historical migration is changed.

The October5 read-only production observation found `/app/uploads` absent and
only the public asset bind present. This establishes exposure to future file loss
on container replacement, not evidence that existing customer files were lost.
The deployed image has not received this repair.

## Executable boundary

The production entrypoint recognizes the backend's production aliases across
`APP_ENV`, `ENVIRONMENT`, `NODE_ENV` and `RUNTIME_ENV`. Before SQL, packaged-asset
copying or application startup, it requires an existing writable `/app/uploads`
with exactly one writable kernel mount entry and rejects known temporary or
container-layer filesystems. Production migration-only precheck may omit the
mount because it does not start an application. Development startup remains
available. The helper does not accept an environment-selected mount table.

Compose binds the pre-existing host directory and sets `create_host_path: false`:
a missing directory must fail instead of silently becoming a root-owned empty
folder. Runtime inspection separately reports whether the canonical source bind is
present and writable. Missing observations remain unknown. A correct mount is
necessary for this contract, but is not proof of disk durability, sufficient
space, backups, correct file permissions, privacy, or successful application I/O.

The startup state machine is `configured -> storage-checked -> application-started`.
A failed check ends in exit78 before application or SQL execution. A migration-only
invocation may instead reach `prechecked -> exited`; it cannot accept an upload.
There is no automatic fallback to an ephemeral path. Startup checks do not prevent
a privileged operator from later unmounting or replacing the host filesystem.

## Release and recovery obligations

Before replacing the old API container, the guarded release must:

1. Acquire its exclusive release lease, identify and drain every writer, and
   confirm the stopped old container's identity. Re-observe `/app/uploads` under
   that fence; the earlier empty observation does not permit discarding later files.
2. Preserve any legacy files in a private staging backup, retaining relative paths
   and checking contents/hashes. Never mount an empty directory over the only copy
   or remove the old container before preservation is verified. Reject conflicting
   existing paths, symlinks or unexpected ownership for operator reconciliation.
3. Provision the private host directory with mode0700 and the reviewed image's
   actual UID/GID. Do not recursively change unrelated paths or overwrite files.
   Verify mounted read/write behavior as that user, including existing files.
4. Back up database, public assets, private uploads and necessary recovery secrets
   as one fenced recovery set. Restore it into an isolated environment and verify
   content/path correspondence before admitting the rollout. The current online
   database-only restore rehearsal does not satisfy this requirement.
5. Verify the exact image's mount, safe private attachment access and synthetic
   file survival across container replacement before routing traffic. Keep
   experiment/provider flags unchanged. After writes resume, preserve accepted
   data and recover forward with a compatible image rather than an old backup.

A host bind survives container replacement while the host filesystem survives.
Host loss, disk failure, file/DB atomicity, upload retry races, retention, deletion,
quotas, public media, DDEX private storage and user-owned providers are separate
obligations. This repair does not claim to solve them. The canonical release
executor and coordinated upload backup/restore remain unfinished; the preparation
command therefore continues to report `executionAllowed: false`.

## Evidence and limits

`persistent-uploads.test.mjs` executes the actual shell helper against controlled
mount tables. `production-entrypoint.test.mjs` verifies rejection before SQL/assets/
application side effects, all runtime aliases, and migration-only behavior.
`test-hetzner-inspection.py` and `hetzner-preparation.test.mjs` check redaction and
unknown/false/true observations without granting deployment.

`test-persistent-uploads-container.py` runs only on an isolated GitHub-hosted Linux
runner. It uses a digest-pinned Debian image, the actual entrypoint, no network,
no credentials, a synthetic file and nonce-owned containers. It requires content
survival in a second container and rejection of absent, read-only and tmpfs mounts.
This is a container/shell integration test, not a backend upload or production
recovery test. No formal filesystem/crash-consistency proof is claimed.

The storage choice follows [Docker's bind-mount contract](https://docs.docker.com/engine/storage/bind-mounts/)
and its [writable-layer lifecycle](https://docs.docker.com/engine/storage/drivers/).
The existing host deployment makes a dedicated private bind a compatible repair;
it does not require a new cloud-storage provider or imply decentralized ownership.
