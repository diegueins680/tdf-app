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
