# Isolated restore safety boundary

`DEPLOY-RESTORE-001` owns this boundary. The implementation must hold the private
host lock while checking **all** containers carrying `net.tdf.restore-rehearsal`,
including stopped containers. A durable root-private pending-attempt marker is
created and fsynced before Docker creation; any existing marker (including a
malformed file or symlink) rejects before container inventory. The marker is
removed only after the fully admitted target has been removed successfully. A
client death before a create response may leave an outstanding daemon operation,
so absence of a container is insufficient to clear a marker. Any result or Docker failure rejects before source
inspection, a new archive or a new isolate. Labels identify a blocking residue;
they do not authorize deletion. An operator must inspect full nonce/image/target
identity before resolving an orphan. Process death releases flock but need not
remove the Docker container.

## Bounded model

`formal/event-operations/RestoreIsolation.tla` models two distinct one-shot runs,
with idle, locked, requested, created, restored, verified, cleaned, passed and failed phases.
A crash can occur in any active phase, releasing the process lock while retaining
any isolate and durable pending marker. Create request and completion are separate
actions; completion may occur after the request owner crashes. A retry is a different run. Verified operator orphan removal is an
abstract environment action allowed only after no create operation is in flight. All actions may stutter; no fairness or liveness
claim is made. In particular, an orphan may block future attempts indefinitely.

Safety properties: `ExclusiveOwners`, `NoSourceMutation`, `AtMostOneIsolate`, `ReceiptSound` (a receipt
requires verification and removal), and `TypeOK`. `HonorPending` represents both
the preflight check and exclusive reservation at creation (`O_EXCL`); its negative
control intentionally disables that whole boundary. The five controls are:

- NoLock removes live owner exclusion and must violate `ExclusiveOwners`; exclusive
  marker creation still protects the isolate boundary.
- OrphanRetry removes both visible-orphan and durable-reservation admission, and
  must violate `AtMostOneIsolate`.
- LateCreate removes durable-reservation admission while retaining the visible
  orphan check, and must violate `AtMostOneIsolate` after delayed completion.
- SourceTarget permits a source target and must violate `NoSourceMutation`.
- EarlyReceipt removes verification/cleanup sequencing and must violate `ReceiptSound`.

These are controlled specification mutations, not claims that deleting a single
Python line defeats all independent guards. CI requires each named violation, not
merely a tool error.

The abstraction trusts the local Docker inventory, exclusive lock inode, durable marker ordering and
fully checked container identity. Only cooperating helpers create these labelled
isolates; an unrelated root operator changing containers concurrently is excluded.
Fsync durability and authorized operator proof that a create operation has quiesced
are assumptions, not modeled guarantees. A crash before sending a create request
may leave a conservative marker and is represented as blocked recovery; availability
is not proved. The verified phase abstracts successful archive replay, relation-count/ledger
comparison and optional migration checks. It does not prove these SQL operations,
Docker isolation, Python refinement, SSH identity, resource sizing, asset recovery,
provider behavior or release rollback. Source reads and role handling are outside
this small scheduling model. It is not a universal recovery proof.

## Executable correspondence

| Model action/property | Implementation | Executable evidence |
| --- | --- | --- |
| Acquire / exclusive admission | `rehearsal_lock`, then `rehearse_locked` orphan check | Concurrent lock and orphan-before-source tests in `scripts/test-hetzner-restore.py` |
| Create / NoSourceMutation | `IsolatedRestore.admit`, `write_command`, pinned local daemon | Unsafe-target and read-only source command controls |
| Restore / Verify | Exported snapshot archive, counts and migration ledger checks | Stage fault injection and exact-source remote restoration receipt |
| Cleanup / ReceiptSound | Identity-checked cleanup before receipt | Lost-create-response, cleanup-failure, interruption and source-drift controls |

Run the Python test above and `bash scripts/verify-event-operations-formal.sh`
with the repository's pinned TLC/Alloy jars. Full receipts bind the exact commit
and source digest; earlier passing receipts remain historical evidence.

The design follows [Linux flock semantics](https://www.man7.org/linux/man-pages/man2/flock.2.html)
and [Docker's all-container label filtering](https://docs.docker.com/reference/cli/docker/container/ls/).
The need for an orphan check is a TDF-specific inference from those two lifecycles.
