# Interrupted-release abort boundary: DEPLOY-JOURNAL-001

`ops/hetzner/interrupted-release-recovery.py` preserves a failed pre-write release,
records an irreversible abort and admits observation of a different boot of the
same trusted Linux host. It does not restart production, restore a database,
authenticate an SSH endpoint or certify deployment recovery.

## Authority and failure policy

The [original-deployment sampler](original-deployment-admission.md) implements the
private preparation boundary. The coordinator must durably retain the **full original deployment admission before
its first shutdown effect**: original container IDs/images/configuration, storage
and volume identities, installed configuration, original enabled/active timer state,
migration history, release/plan identity and host identity. This library binds a
caller-supplied admission document and its independently retained hash; it does not
establish the truth, completeness or prior durability of that document.

Before normal sequence15 (`start-database` intent), an abort can preserve the
original current cluster and API writable layer. Never restore a historical
snapshot over them. Sequence15 or any later numbered file, including an empty or
malformed pending file, rejects this pre-write abort path. After production write
intent, recovery requires a separate forward/compatibility procedure.

The abort reader acquires the **same permanent `release.lock`** as normal release.
It recognizes a contiguous canonical original journal with at most one final
publication remnant: a complete lone pending record, or pending/canonical names
linked to the same private inode. It checks the complete chain and records hashes,
names and inode relationships without completing, deleting or repairing records.
Truncated/malformed remnants remain preserved and denied; separately reviewed
recovery from independently durable admission is still required for those cases.

Creation of `abort/`, including an interrupted or malformed latch, permanently
denies normal initialization and effects. A valid durable latch binds original
admission, release, plan and frozen journal. Every subsequent operation rechecks
that original snapshot. Unknown files, unsafe permissions, substituted records,
competing locks and inherited/closed handles reject admission.

The fixed `abort-reboot` intent is published with file/directory fsync **before**
invoking a caller's reboot effect. It marks new writes possible immediately. A lost
response, returned callback or disappeared SSH connection is not reboot evidence.
There is no automatic reboot retry. Observation requires a changed kernel boot ID
on the same machine ID, under authenticated-host and trusted-kernel assumptions.
The observation explicitly leaves runtime readmission, clean database shutdown,
continuous maintenance and successful recovery unverified.

Original services and an enabled timer may resume during abort. **This abort policy
permits original-deployment availability**, and does not claim a continuous ingress
fence. Before any explicit service recovery the coordinator must reacquire release
and restore reservations, re-admit original configuration/storage and clean only
identity-bound disposable resources. It must recover the current cluster in place,
verify readiness/version and migration history, preserve original uploads, restore
the admitted timer state and record a terminal abort outcome. Those production
restart/terminal adapters remain unimplemented; this library cannot authorize a
production shutdown by itself.

## Why a fresh host boot

Pinned Moby29.1.3 stop handling survives client cancellation. Its kill path can
launch a detached cleanup goroutine; an ActionStop event or returned request does
not establish that all cleanup has joined. A delayed-cleanup/restarted-container
schedule is a **source-derived risk, not a reproduced Docker failure**. Reject an
ActionStop-only barrier. A full trusted host reboot provides a stronger process
lifecycle boundary without depending on that schedule's reproducibility. It does
not prove clean PostgreSQL shutdown. See `RES-ABORT-QUIESCENCE-*` in `research.json`.

## Executable evidence and exclusions

`python3 scripts/test-interrupted-release-recovery.py` exercises real private files,
shared locks, publication remnants, changed snapshots, process death after reboot
intent, all three publication fsync failures and denial after uncertain intent.
Its host observations are synthetic. Two source mutations remove the boot-change
or host-identity guard; each must make its named control fail. Existing journal
controls continue to run. This is implementation testing, not a refinement proof.

The separate two-step `test-interrupted-release-recovery-linux.py prepare|verify`
requires explicit acknowledgement of an exclusively owned empty rebootable Linux
VM. It creates nonce-labelled PG17/inert API resources, persists synthetic history
and legacy uploads, loses a real stop-client response after signal delivery, then
requests an actual host reboot. Verification checks the barrier, original complete
container configurations, current data/uploads and automatic timer resumption;
it cleans only its admitted synthetic resources. Never run it on production or a
shared CI runner. Its source-qualified execution receipts must distinguish fixture
restart code from the unimplemented production restart adapter.

Root/kernel/filesystem trust, independently authenticated host identity and
noncooperating privileged writers remain outside the boundary. Boot IDs do not
replace authentication; journal hashes do not authenticate a root attacker.

## Bounded model

`AbortRecovery.tla` explores one release, two boot epochs and Boolean latch,
production-write, intent and observation state. Successful durable publication and
unchanged private admission are abstract atomic facts; host identity is fixed.
It checks denial of normal continuation after latch, pre-write-only abort, durable
intent before reboot request and fresh-boot admission. Four controlled mutations
remove those guards and must violate their named invariants. Crashes are abstract
stuttering after persisted state; reboot can occur only after request. No fairness
or liveness is asserted. JSON/filesystem formats, ambiguous partial publication,
host authentication, multiple reboots, live Docker behavior, service restart and
full deployment are excluded. This model does not refine Python or prove the
production coordinator correct; real filesystem and VM controls are separate.
