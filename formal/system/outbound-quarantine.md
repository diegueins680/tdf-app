# Outbound restriction during legacy recovery

`DEPLOY-QUARANTINE-001` specifies a scoped recovery boundary. The old production
image can restart outbound workers and ignores candidate-only disable flags.
An uncertain SMTP or provider outcome cannot be repaired by assuming it failed.
Restriction cannot undo bytes already accepted or justify clearing delivery holds.

The library generates one owned nftables `inet` table. Input and forward chains
at priority -210 cover IPv4 and IPv6 Docker bridge traffic. Forwarding within each
explicitly admitted identical bridge pair is permitted. Reply-direction traffic
supports responses to incoming requests. Host input also permits IPv6 Neighbor
Solicitation/Advertisement control packets, only with hop-limit255 and code0;
without these, valid replies fail when the neighbor cache expires. All other traffic originating from
`docker0` or `br-*` is dropped, including already-established outbound connections,
invalid and untracked packets. There is no blanket established/related allowance,
cross-bridge wildcard permit, flush of another table, or automatic removal path.

Exact numeric kernel JSON is compared with generated policy, ignoring only kernel handles
and nft version metadata. Every expression, rule order, family, hook and priority
must agree. Boot can load a missing table atomically; it never overwrites a changed
table. Root-private files bind the program hash, admitted bridges and exact unit
contents. Docker both requires/follows the guard and runs `verify-start` before
every daemon start, including when the oneshot guard is already active. That
second boundary rejects missing or changed live policy instead of repairing it.
Stopping the guard does not flush rules. Startup is ordered after nftables and
UFW loaders; this ordering alone does not qualify their hooks or reload behavior.

The intended recovery state sequence is unqualified → installed → restricted →
recovered-restricted. A reboot must re-establish restriction before Docker restores
containers. Original-image and candidate recovery both retain restriction. Release
of the restriction is a separate transition after delivery reconciliation and
qualified worker behavior. No production installer or release coordinator is
exposed by this component. Production operational acceptance remains required.

## Evidence and exclusions

Portable mutation controls test missing denial, established-outbound permission,
IPv4-only scope, cross-bridge permission, reordered/extra rules and policy drift.
The Linux packet fixture uses actual Docker bridges and synthetic host/routed
receivers. It covers fresh and pre-existing IPv4/IPv6 connections, local peers,
incoming responses after explicitly clearing IPv6 neighbor caches, a removed
Neighbor Discovery allowance and a deliberately removed denial. The three-phase reboot
fixture uses a real changed kernel boot identity, automatic container restart,
an already-listening systemd-notify receiver and failed Docker-start controls.
It also enables live restore in the owned VM and checks that the exact container
process survives daemon stop and a rejected daemon start while the kernel policy
continues to block its repeated connection attempts. A stopped Docker daemon is
therefore not evidence that application processes are stopped. The fixture
restores the original daemon configuration bytes and mode; when the original
omits `live-restore`, a qualified owned-daemon restart is needed because reload
retains the previously enabled value.
Fixture cleanup removes only nonce-labelled containers/networks and their
anonymous volumes; other volume identities must remain unchanged.

The optional `TDF_QUARANTINE_TEST_UFW=1` lane uses UFW0.36.2-6 with the
iptables-nft backend and deliberately permissive synthetic rules. Four continuous
IPv4/IPv6 senders target host and routed receivers during reload and restart.
One immutable provider-acceptance baseline covers both operations and the
intervening local/ingress checks. Removing host and routed restrictions must make
the same destinations reachable, excluding an accidental UFW denial as the reason
for success. The real reboot fixture additionally checks monotonic activation
ordering UFW → quarantine → Docker. CI runs both packet lanes on separate
disposable runners; actual reboot qualification remains a separate release test.
This does not qualify production's UFW configuration. Production admission must
bind the package implementation, selected iptables backend, configuration,
non-executable custom hooks and loaded unit/drop-ins to reviewed evidence.

`ops/hetzner/ufw-recovery-admission.py` observes that identity read-only. It binds
the installed UFW Python/shell implementation, resolved command binaries,
iptables-nft version strings, all UFW configuration files and the loaded service
fragment/drop-ins. It rejects executable custom hooks, unsafe file metadata,
pending daemon reload and drift from caller-supplied reviewed policy. A second
observation must agree. Saved `ENABLED`, IPv6 and built-in-chain management settings
are distinct from systemd's active/enabled unit flags. Observed UFW chains and
unconditional input/output/forward hooks distinguish absent, hooked and partial
kernel filtering; admission rejects a boot-configuration/runtime mismatch. The
active-UFW fixture requires both `ENABLED=yes` and actual IPv4/IPv6 hooks before
it exercises reload, restart or boot ordering, so a skipped loader cannot pass.
Portable controls cover changed hooks, modes, rules,
backend alternatives, package files, unit definitions and evidence references.
The caller must verify the source/packet/reboot receipt provenance: hexadecimal
hashes alone are not verified receipts or approval. No trusted policy is generated
from observation. OS libraries/Python bytecode cache remain trusted, and a matching
UFW identity still reports complete host-bypass admission as false.

The October6 read-only production observation found `ENABLED=no` and no UFW
chains/hooks in either family, despite the service being active/enabled. This is
a disabled UFW configuration, not evidence of active host filtering. The current
production configuration must be re-observed before release; it is not changed by
this component or by synthetic VM qualification.

`ops/hetzner/dormant-container-admission.py` separately observes all containers
through allowlisted read-only Docker HTTP requests. It supports the qualified
29.1.3 daemon version and binds its executable hash, data root, full container IDs,
network identities, restart policies and persisted configuration hashes. An exited
`unless-stopped` container requires literal persisted `HasBeenManuallyStopped=true`
and `HasBeenStartedBefore=true`; `always`, `on-failure`, unknown policies,
transitional states and nonzero dormant PIDs reject. Configuration bytes and
environment values never enter the returned receipt. A caller-supplied reviewed
snapshot must match two observations. For socket activation, PID1 peer credentials
are resolved to the canonical Docker service's inherited listening socket. The
observer checks process start time and polls a held pidfd before/after sampling;
executable reads use `/proc/<pid>/exe`, not the pidfd as a pathname.

This snapshot is exact and stage-specific: running PIDs and private configuration
hashes can change across recovery or reboot. It is not a reusable boot policy or
permission to start a dormant container. The caller must verify independent
daemon-restart/reboot receipts and re-establish admission at each relevant stage.
Privileged noncooperating writers remain outside the sampled guarantee.

The dedicated `scripts/test-dormant-container-reboot-linux.py` fixture runs a second
daemon using caller-staged, hash-bound Docker29.1.3/runtime binaries. Its explicit
private configuration, data/exec roots, socket and managed containerd separate it
from the primary daemon. Abandoned paths reject before any daemon start. Synthetic
containers use network `none`; clearing one persisted manual-stop flag while the
fixture daemon is stopped must make that container restart, while the unmodified
container stays stopped. After both are deliberately stopped again, a real changed
host boot must preserve their dormant state. Metadata replacement, symlink and
writable-file controls exercise actual Linux files. The VFS fixture qualifies this
restart-metadata behavior, not production's storage driver or complete recovery.
Its `prepare`, actual owned-host reboot, `verify`, and `cleanup` phases require
`TDF_DORMANT_FIXTURE_MACHINE` equal to the owned VM's machine identity. Never run
this destructive synthetic fixture on production. Private evidence is retained;
the observer itself performs no Docker mutation.

`ops/hetzner/bpf-recovery-admission.py` qualifies a deliberately narrow BPF subset
on Linux x86_64. Its syscall allowlist only enumerates/reads existing programs,
maps, links and BTF; it cannot load, execute, attach, pin or update them. Program
and map descriptors are held while their relationships and contents are queried.
Two complete observations must agree. Names and tags are diagnostic, not authority.

- Cgroup packet/device programs may use only the accepted reads, register
  operations, forward branches and exit encodings. Stores, helpers, tail calls,
  kfunc calls, backward branches and unsupported opcodes reject this pure subset.
- The separately recognized systemd IPv4 address-verdict template may copy four
  packet bytes to its fixed stack buffer and look up one precisely shaped LPM
  map. Both helper targets are resolved against the running kernel, bound to their
  instruction indices and signed encoded call values, and required to be
  `bpf_skb_load_bytes` and `trie_lookup_elem`. Its complete instruction template
  fixes stack writes, arguments and final verdict logic. Map contents cannot
  redirect packets in this template and are not collected.
- The HID tracing exception requires the exact seven-instruction entrypoint,
  a1024-slot program array with every slot read and empty, and the actual
  `fmod_ret/__hid_bpf_tail_call` BTF attachment. BTF object identity must resolve
  to the running kernel's `vmlinux` bytes. A populated slot, wrong attachment,
  changed return or unsupported link rejects admission.

Unclassified programs, links, maps, incomplete enumeration, changed code hashes
and stale call-resolution records reject. Portable mutations exercise each
boundary. This is source-informed structural checking of already kernel-verified
programs, not a replacement BPF verifier, kernel proof, or continuous monitor.
The running kernel/verifier, syscall and symbol interfaces are trusted; release
ownership excludes concurrent noncooperating privileged changes. The caller must
independently qualify the exact kernel release/BTF identity and evidence provenance.
Dynamic object IDs are bound through observed relationships, not reused as stable
identities across reboot. This component still reports complete host-bypass
admission as false: TC/XDP, flowtables, bridge topology, host proxies and the
integrated recovery coordinator have separate obligations.

Run portable checks with `python3 scripts/test-outbound-quarantine.py`. Linux
fixtures require root on an explicitly acknowledged empty owned VM, an existing
immutable pgvector image and `TDF_SYNTHETIC_QUARANTINE_HOST` equal to its machine
identity. The packet fixture is self-contained. The reboot fixture requires
`prepare`, an actual reboot of that same VM, `verify`, then `cleanup`. Do not run
either on production. Logs and source fingerprints remain separate evidence.

These checks do not prove kernel correctness, total network isolation or exactly
once delivery. Docker's host DNS relay is excluded. Production use still requires
admission of bridge identity, absence of host/macvlan/custom-interface containers,
unexpected bridge ports, flowtables/offload, host proxies and competing firewall
loaders. Privileged noncooperating changes are excluded. Those host checks,
persistent production installation, coordinator integration, first interrupted
legacy stop and original-image recovery admission remain unfinished.

While restricted, provider payments/messages, Caddy certificate renewal and other
container-origin external calls may fail. Legacy workers can record failures or
unknown outcomes; preserve those records. Incoming responses and local database
traffic are intended to remain usable, subject to the complete host admission.

## Namespace and packet-path provenance

`ops/hetzner/network-recovery-admission.py` performs read-only observation of the
canonical Docker socket, all networks and each running container. The caller
supplies independently established full container/network IDs and the uplink.
The collector retains process and namespace descriptors through each sample,
checks process liveness and unchanged Docker identity, and joins endpoint IDs,
MACs, interface indices, bridge membership and peer namespace identity. Interface
indices are namespace-local: reciprocal numbers alone are insufficient. Fixed
`RTM_GETNSID` requests with `NETNSA_FD` look up the held opposite namespace in each
direction; each veth's `link_netnsid` must match. Missing mappings reject. The
collector never issues `RTM_NEWNSID` or interface, route, firewall or Docker
mutation requests. Linux `GETLINK` may materialize namespace-ID bookkeeping
while reporting interfaces; the separate `GETNSID` lookup does not allocate IDs.

Only conventional local Docker bridges, accounted veth ports, the declared
external interface and loopback are accepted. Host/shared namespaces, additional
capabilities, unknown interfaces or ports, STP/VLAN filtering, unsupported routes,
legacy rules, TC filters and XDP reject. nftables syntax is deliberately limited
to observed filtering and NAT classes; duplication, forwarding clones, queues,
flowtables and unknown actions reject. An AF_PACKET socket is accepted only for
LLDP bound to the external interface; its owner still requires separate host
process qualification. The collected receipt omits packet contents and kernel
pointer columns. Two normalized observations must agree; only explicit counters,
handles and lifetimes are ignored.

The portable suite includes wrong/missing namespace mappings, third-namespace
index collisions, changed endpoints/bridges, unsupported packet paths and sampling
drift. `scripts/test-network-namespace-linux.py` requires an acknowledged owned
machine and empty Docker inventory. It creates interfaces only inside three
unshared child namespaces, checks bidirectional descriptor binding, verifies an
unassigned query does not allocate a mapping, and distinguishes two interfaces
with the same index in unrelated namespaces. It cleans up only its child processes
and verifies unchanged host links. CI runs this fixture in both packet-test lanes.

This is sampled structural admission under a trusted kernel/collector and exclusive
release ownership. It does not establish continuous isolation, evaluate arbitrary
BPF semantics, admit privileged host processes/devices/mounts, close the explicitly
excluded Docker DNS relay, or authorize production restriction. The result always
sets `hostBypassAdmissionVerified` to false; the coordinator must combine separately
qualified boundaries and boot/policy evidence before treating the host as admitted.

## Legacy interrupted-stop target contract

The legacy image's observed SIGINT exit255 is an interrupted stop, never evidence
of HTTP drain, completed file writes or known external outcomes. The qualified
ordinary stop/capture path continues to reject it. An exceptional path is not yet
implemented or authorized by these models. It must bind the exact old image,
container, configuration, permitted signal and qualification references durably
before maintenance. A generic unclean-stop switch or arbitrary accepted exit-code
list is forbidden. Missing identity, intent or live restriction must deny the
signal; a lost command response leaves unresolved intent even if the process is
observably dead.

A future exceptional capture must require the acknowledged bound stop, exited255,
PID0, no OOM/restart, a dead retained pidfd, unchanged configuration and retained
root/namespace checks. It must preserve all existing file/metadata/mount checks and
report potentially partial application files explicitly. PostgreSQL still needs
its ordinary clean shutdown and control-state checks. Original/candidate recovery
must re-establish and recheck restriction, including after reboot and before starts
on an already-running daemon. No recovery completion may remove restriction or
invent a known provider outcome. Removal requires separately evidenced reconciliation.
Operational acceptance of these degraded conditions remains pending.

`LegacyInterruptedStop.tla` is a **target safety model**, not a refinement of an
implemented exceptional path. It bounds one attempt, one API identity, one boolean
restriction boundary, one database-cleanliness observation and one ambiguous
external outcome. The only phase values are initial, intent, submitted, stopped,
uncertain, captured, recovered and released. Reboots may repeat but erase only
sampled restriction state in this abstraction. There is no fairness assumption or
liveness claim. Exact source identity, durable writes, effective/persistent packet
restriction, trustworthy acknowledgements, PG control checks and reconciliation
are abstract environment predicates. Existing `AbortServiceRecovery` separately
models fresh-boot recovery epochs; this model does not compose or prove those
adapters, kernel/network behavior, partial-file repair, SMTP, provider state or
operational approval. `Reconcile` is a trusted abstract action, not provider
evidence. There is only one submission and no retry action; this does not
independently prove that an implementation rejects retries.

The intended invariants require exact identity and durable intent before stop,
restriction at stop and recovery, an acknowledged stop and clean database before
capture, no fabricated graceful/external completion, and reconciliation before
release. Eight isolated controlled mutations remove one of these guards; each
must violate its named invariant. Passing bounded analysis establishes only this
transition contract under its assumptions. Integration, actual legacy capture and
restricted recovery remain open implementation obligations.
