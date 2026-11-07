# Ordered release intents: DEPLOY-JOURNAL-001

`ops/hetzner/release-journal.py` is a local ordering and durability boundary for the
pending Hetzner coordinator. It invokes only caller-provided effects. It does not
implement maintenance, writer drainage, storage admission, backup, transfer,
restore, migration, deployment or validation of an observed external effect.

All release nonces must use **one configured global control directory**, created
privately and durably by the coordinator. `open_journal` holds a nonblocking lock
on its permanent mode0600 inode. Its records are descriptor-relative, private,
single-link regular files under a directory opened without symlink ancestors.
A new nonce cannot use `initialize` over any existing journal. There is no
rotation, deletion, reset, resume-pending or rollback API. The separate
`initialize_next(plan, release_nonce, expected_tip_hash)` admission appends a new
plan only after a complete terminal prefix under the same permanent lock. A different
control directory would bypass this local exclusion and must never be admitted by
the coordinator. Privileged noncooperating writers are outside this boundary.

## Separate admission of a subsequent release

The observed production journal already contains a complete release. Deleting,
renaming or resetting it, changing the global directory, or replacing its lock
would discard exclusion and evidence. The separate admission closes this local
library gap; it does not install or authorize the production coordinator.

Every release occupies one plan plus twelve intent/observation pairs. Existing
schema-version1 records, names and bytes remain unchanged. A successor plan starts
at the next global sequence and hashes the previous release's terminal record;
its new nonce and plan hash bind its own effects. Reading validates the entire
chain, including historical plans and every original observation. All nonces
must remain unique across retained history. Admission requires the exact expected
terminal hash, so two operators using the same observed tip cannot both append
successors. Every plan, intent and observation rechecks its caller's expected tip
at publication, including callers sharing one handle between threads; exclusive
pending-file creation rejects competing publication of the same sequence. A
per-handle mutex serializes the complete append, including its fresh prefix
check and durable publication, so a losing thread cannot leave a new temporary
after the winner has completed. PID admission occurs before waiting on this
mutex. Reads encountering an in-progress temporary may reject; no liveness or
fully linearizable shared-handle API is claimed.
`initialize` still rejects any nonempty journal. The legacy reader rejects an
extended multi-release history; control-tool rollback must retain the upgraded
reader and the full chain rather than truncate evidence to fit the old reader.

No incomplete prefix, pending intent, temporary file, hole, unsafe link/permission,
corrupt history or stale tip can admit another release. The same exclusive
publication and fsync protocol applies to the successor plan. IO uncertainty
poisons the current handle. A leftover temporary blocks fresh admission; a fully
published canonical plan can become durable through a successful fresh open,
without republishing it or replaying any effect. Forked handles stay invalid.

`status` and `perform` address the current release. `priorReleases` counts its
complete predecessors and `latestRecordHash` supplies the compare-and-set token.
`newWritesPossible` remains conservative over the entire history; admitting a new
plan never clears evidence that writes may have occurred. It does not authorize
restoring any prior database. A maximum of128 releases bounds validation work;
exhaustion rejects before mutation and requires a separately reviewed extension,
not deletion, rotation or another directory.

The coordinator must freshly validate the configured global directory and lock,
current target, exact plan/image, authorization, maintenance, fences, recovery and
effect evidence before use. A historical terminal record or matching tip cannot
certify those current facts. Receipt storage, exact-artifact assurance/risk
disposition, recovery and release authorization remain independent blockers.

The plan binds source/Mobile revisions, runtime and migration evidence, candidate
and recovery image digests, and the recovery recipient hash. It contains no raw
credentials, SQL, filenames or provider responses. Each canonical record carries
schema version, release nonce, plan hash, sequence and predecessor digest.
Intent additionally binds the phase and exact-target document hash; its operation
ID is derived from that context. Completion references the same operation and an
observation hash supplied by the trusted coordinator. Hashes detect accidental
substitution; they do not authenticate an attacker with journal write access.

The executable phase order is `release-journal.py::STAGES`: maintenance, stopping
writers and database, capture, encryption, off-host retrieval, isolated recovery,
database startup, migrations, candidate startup, candidate verification and traffic
reopening. Off-host evidence must independently distinguish upload from retrieval
and identify the same encrypted bytes and destination. Database startup intent
already sets `newWritesPossible`; startup configuration, migration and background
jobs may write before public traffic returns. No old-database rollback is offered.

`perform(stage, targets_hash, effect)` publishes and fsyncs intent before calling
`effect(context)`. Only that invocation can append its matching observation.
The callback must independently validate the exact effect and return the context
plus its protected evidence hash. A Boolean, mismatched context, failed callback,
or interrupted intent cannot authorize subsequent operations or another nonce.
`sequenceComplete` means only that the recorded sequence completed; it is not
proof of release readiness or factual validity of the caller's evidence.

Records use exclusive temporary creation, file fsync, exclusive hard-link
publication, directory fsync, temporary unlink and another directory fsync.
Any leftover temporary, unexplained file, missing sequence, unsafe permission,
link or malformed/hash-inconsistent record rejects admission. An IO error poisons
the current handle. A fresh admission fsyncs the directory before inspecting the
prefix: an already complete canonical observation can become durable through that
sync, but an unresolved intent is never replayed. Storage that lies about fsync,
hardware loss beyond its durability contract, and privileged journal replacement
are excluded. Never infer completion from a disappeared process or released flock;
an external child or daemon request may still be running.

## Executable controls and limitations

`python3 scripts/test-release-journal.py` exercises actual temporary files, locks,
subprocess death and a surviving child. Controls reject out-of-order/replayed
phases, ordinary initialization over pending or terminal state, incorrect observation context,
file/directory fsync failures, partial publication, sequence holes, links, public
permissions, changed records, inherited fork handles and replaced lock inodes.
Succession controls complete three releases, preserve the legacy25-record prefix
and lock inode, reject reused nonces/stale tips, exercise two real competing
processes and process death during plan publication, and inject every successor
plan fsync failure. They retain the original controls and run with Python
optimization so admission does not depend on `assert`.
These are implementation/property controls, not a distributed consensus proof or
formal proof of external Docker/provider effects. Production execution remains
unavailable until effect validators, global directory admission, writer fencing,
coordinated recovery and explicit operator recovery are implemented and reviewed.

## Bounded model

`formal/event-operations/ReleaseJournal.tla` checks two release nonces and two
ordered phases. Intent persistence abstracts successful file/directory fsync and
canonical record admission; partial or failed publication cannot reach effect
execution. The model retains a reservation after process death and allows an
external effect to finish afterward. Completion requires context-matching
observation, abstracting the Python reference checks, not factual effect validity.
Safety properties cover durable intent, one reserved release, ordered effects and
bound completion. Three controlled mutations disable intent persistence, permit a
second nonce, and accept a mismatched observation; each must violate its named
invariant. No fairness or liveness claim is made: unresolved intent intentionally
blocks, and the environment can fail permanently. Filesystem formats, permission
checks, cryptography, evidence truth, exact twelve-phase effects, terminal succession
and recovery are excluded from that original model. Real filesystem/process controls supplement this model;
it is not a verified refinement proof from Python or a whole-system proof.

The bounded model treats crash as terminal and excludes reopening a completed
prefix. Python permits continuation after fresh durable admission when every prior
intent has a complete observation. The coordinator must revalidate current target,
maintenance and writer-fence prerequisites before each effect, including after
reopening: a historical observation does not prove a fence remains effective.
Completion-publication fsync controls exercise all three completion sync points;
only the fully published canonical observation can continue after successful fresh
admission, without repeating its effect. Partial publication remains blocked.

The additional `ReleaseJournalSuccession` model qualifies only its stated finite
succession abstraction, separately from the original model. Its configurations
and named mutation controls are mapped in the requirement register. Neither model
proves Python refinement, physical fsync durability, factual effect evidence,
privileged-host safety, liveness or complete implementation correctness.
