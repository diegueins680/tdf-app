# Isolated application canary: DEPLOY-CANARY-001

The optional `--canary-image diegueins680/tdf-hq@sha256:…` argument to the canonical
restore rehearsal requires `--with-candidate-migrations`. The image must already
exist locally by digest; this operation never pulls an image or routes traffic.
The selected image must come from the separately reviewed build/source evidence;
matching `/app/COMMIT` and `/version` does not prove arbitrary image provenance.

After restoration and two successful migration applications, a nonce-owned app
joins only the admitted restore database's `network=none` namespace. Both network
identities are checked. It receives fixed synthetic database configuration through
`env -i`, no production env file and no production mounts. It runs as1000:1000 with
read-only rootfs, dropped capabilities and CPU/PID/memory limits. Assets/uploads
are new private host directories; they are not copies of customer files. The real
production entrypoint and private-upload mount guard run. Background startup code
may write the disposable database, but has no route to providers or production.

Host Python sends only `/health` and `/version` requests through an open namespace
file descriptor shared with the app, avoiding a reused PID path. Probes have their
own timeout, no proxy or redirects, bounded bodies and allowlisted metadata keys.
A valid healthy response, exact commit file/version, and binary hash are required.
The tool pauses only the fully admitted disposable DB, requires either a recognized
connection timeout/failure or fixed503 rather than200, then unpauses and requires
fresh healthy recovery. Malformed HTTP, unexpected status/body, or internal probe
errors cannot masquerade as transport unavailability. The receipt records whether
the unavailable probe actually returned503 or timed out; a timeout is not evidence
that the handler produced503 within a strict deadline.

The existing durable restore reservation precedes external creation. App cleanup
must finish before DB cleanup and reservation release. Lost create responses are
recovered only through exact nonce, image, command, network and mount admission.
Uncertain cleanup keeps the reservation and prevents another run. No broad label
cleanup or automatic erasure of failed evidence is permitted. Private synthetic
asset/upload directories and archives are retained for diagnosis.

## Evidence and exclusions

Python controls reject target, mount, network, privilege, command and cleanup
mutations. Synthetic real HTTP servers verify protocol errors, oversized/private
bodies and redirects. Launcher controls reject forged image, source, cleanup,
provider-connectivity and missing pause/binary evidence. These tests do not prove
actual Linux Docker inspect, entrypoint or native runtime compatibility; a real
isolated execution is separately required. Source changes require fresh receipts.

`ApplicationCanary.tla` models two sequentially admitted runs, one DB and at most
one delayed application-create request per run. The environment may finish that
request after parent death. Isolation admission, verified observations and ordered
cleanup are abstract guards. Safety covers disconnected applications, reservation
and DB lifetime, one application at a time and verified completion. Three controlled
variants remove isolation, cleanup ordering or verified-observation admission and
must violate their named invariants. There is no fairness assumption or liveness
claim: crashed/uncertain work may remain blocked pending operator investigation.
Docker, Linux namespaces, JSON, filesystem durability and executable refinement
are excluded. Bounded checking is not a whole-system proof.

This canary is neither a deployment executor nor an asset/secret/off-host recovery
rehearsal. It does not test authenticated workflows, native apps, provider delivery,
or financial side effects. Host checks are sampled, not a privileged-operator
fence. Writable bind directories lack an aggregate quota. Complete coordinated
backup, writer drain, lease, image provenance, rollback/forward recovery and final
production smoke tests remain separate release obligations.
