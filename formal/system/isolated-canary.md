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
read-only rootfs, dropped capabilities and CPU/PID/memory limits. In the online
logical rehearsal, assets/uploads are new private host directories; they are not
copies of customer files. The optional physical-copy mode is specified below. The real
production entrypoint and private-upload mount guard run. Background startup code
may write the disposable database, but has no route to providers or production.
The cloned database may itself contain stored credentials or tokens accessible to
the isolated app. `productionCredentialsProvided=false` means no production
environment or role secrets were supplied separately; it does not mean the clone
is credential-free. Its archives and synthetic application state remain private.

Host Python sends only `/health` and `/version` requests through an open namespace
file descriptor shared with the app, avoiding a reused PID path. Probes have their
own timeout, no proxy or redirects, bounded bodies and allowlisted metadata keys.
A valid healthy response, exact commit file/version, and binary hash are required.
The tool pauses only the fully admitted disposable DB, requires either a recognized
connection timeout/failure or fixed503 rather than200, then unpauses and requires
fresh healthy recovery. Malformed HTTP, unexpected status/body, or internal probe
errors cannot masquerade as transport unavailability. The receipt records whether
the unavailable probe returned503 or encountered a recognized transport failure
(timeout, refusal or reset); a transport failure is not evidence
that the handler produced503 within a strict deadline.

The existing durable restore reservation precedes external creation. App cleanup
must finish before DB cleanup and reservation release. Lost create responses are
recovered only through exact nonce, image, command, network and mount admission.
Uncertain cleanup keeps the reservation and prevents another run. No broad label
cleanup or automatic erasure of failed evidence is permitted. Private synthetic
asset/upload directories and archives are retained for diagnosis.

The physical-copy coordinator may instead supply trusted restored-content
manifests for both assets and private uploads. This mode requires a registered
dependent application in the live physical reservation. Before application
creation, the physical boundary rechecks mount topology, all copied bytes and
supported metadata in the two fixed canary directories. Their roots must retain
UID/GID1000 and owner read/write/traverse permission; existing content is never
silently chowned or replaced to make a check pass. Manifest inputs are copied to
prevent later caller mutation. Only hashes, entry counts and byte counts enter
the canary receipt. The coordinator remains responsible for binding those
manifests to its authenticated retrieved bundle and coherent capture. This mode
does not accept arbitrary mount paths or work with the online logical helper.

The combined synthetic Docker fixture actually archives/restores UID1000-owned
asset and private-upload sentinels, rejects modified expected hashes before
application creation, then reads matching hashes as the real image user after
startup. This establishes fixture access and correspondence, not arbitrary
customer-file workflow, decryption-key or production recovery coverage.

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

This canary is not a deployment executor and does not independently establish
coordinated asset/secret/off-host recovery. It does not test authenticated workflows, native apps, provider delivery,
or financial side effects. Host checks are sampled, not a privileged-operator
fence. Writable bind directories lack an aggregate quota. Complete coordinated
backup, writer drain, lease, image provenance, rollback/forward recovery and final
production smoke tests remain separate release obligations.

The canary reads the enabled locale/currency codes and their single defaults from
the admitted disposable database using a fixed read-only query. These four
validated public reference settings extend the cleared environment; no other
configuration or credential can enter through this projection. Missing/duplicate
defaults, duplicate codes, unexpected fields and malformed values reject startup.
This preserves the restored deployment registry: the synthetic cold fixture
reproduced failure when its persisted Spanish default met the compiled English
default. The repair changes canary configuration, not database authority or the
backend's startup validation. Timezone and wider product behavior are not covered
by this regional startup projection.

Only `test-physical-application-docker.py` may emit bounded startup diagnostics
from its newly initialized synthetic database fixture. A nonzero source identity
rejects before inspection or log retrieval. Shared production recovery helpers
continue to suppress raw application/daemon logs. These diagnostics are not
permission to print logs from a restored production database.

Disposable creation explicitly sets `--restart=no`; every later admission requires
`RestartPolicy={Name:no,MaximumRetryCount:0}` and `AutoRemove=false`. Unknown,
missing or changed policy rejects use and cleanup rather than repairing policy.
These checks prevent automatic disposable restart/removal from being silently
admitted; they do not supply the missing durable post-crash creation descriptor.
