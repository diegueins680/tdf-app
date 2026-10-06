# Ordered release intents: DEPLOY-JOURNAL-001

`ops/hetzner/release-journal.py` is a local ordering and durability boundary for the
pending Hetzner coordinator. It invokes only caller-provided effects. It does not
implement maintenance, writer drainage, storage admission, backup, transfer,
restore, migration, deployment or validation of an observed external effect.

All release nonces must use **one configured global control directory**, created
privately and durably by the coordinator. `open_journal` holds a nonblocking lock
on its permanent mode0600 inode. Its records are descriptor-relative, private,
single-link regular files under a directory opened without symlink ancestors.
A new nonce cannot initialize over any existing journal. There is deliberately no
rotation, deletion, reset, resume-pending or rollback API, even after completion;
reuse for a later release is a remaining coordinator obligation. A different
control directory would bypass this local exclusion and must never be admitted by
the coordinator. Privileged noncooperating writers are outside this boundary.

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
phases, new nonces over pending or terminal state, incorrect observation context,
file/directory fsync failures, partial publication, sequence holes, links, public
permissions, changed records, inherited fork handles and replaced lock inodes.
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
checks, cryptography, evidence truth, exact twelve-phase effects, terminal rotation
and recovery are excluded. Real filesystem/process controls supplement this model;
it is not a verified refinement proof from Python or a whole-system proof.

The bounded model treats crash as terminal and excludes reopening a completed
prefix. Python permits continuation after fresh durable admission when every prior
intent has a complete observation. The coordinator must revalidate current target,
maintenance and writer-fence prerequisites before each effect, including after
reopening: a historical observation does not prove a fence remains effective.
Completion-publication fsync controls exercise all three completion sync points;
only the fully published canonical observation can continue after successful fresh
admission, without repeating its effect. Partial publication remains blocked.

## Canonical shutdown adapter

`production-writer-fence.py` supplies three actual stop effects to this journal.
It is a library with no command-line execution path. Maintenance stops the
registered backup timer, rejects an already dispatched backup service, then stops
the admitted edge container. The next phase stops the exact API after checking
its caller-retained legacy root, and the following phase stops the exact database.
Each stop is followed by source/configuration and registered-unit resampling.
The service set, image identities, mount topology, configuration digest and
installed unit hashes must remain admitted throughout this sequence.

`production-recovery-sources.py` admits the exact declared subset of stopped
canonical services during these intermediate phases. Undeclared stopped services,
unknown running containers and unacceptable exit states still reject admission.
Unit checks reject unregistered TDF units, overrides, pending daemon reloads,
transient units, dispatched jobs, changed unit bytes and changed enablement.
Only the already-enabled backup timer is admitted; stopping it preserves that
enablement and its restoration remains an explicit coordinator responsibility.

The coordinator must first hold the shared restore reservation and complete host
worker inventory, pin legacy root descriptors before maintenance, and later
verify actual clean PostgreSQL control state. The adapter's receipt explicitly
leaves host-worker inventory and clean shutdown unverified. It does not prove
continuous exclusion of privileged/noncooperating writers. It offers no automatic
restart, rollback or retry after an interrupted journal intent.

`test-production-writer-fence.py` uses actual private journal files with synthetic
daemon observations and effects. It checks stop order, partial state admission,
configuration changes, a backup dispatch race and lost stop responses. No actual
Docker/systemd stop or production recovery is claimed by those tests. The bounded
release-journal model covers intent ordering only; it does not refine systemd,
Docker, kernel descriptors or these exact effect implementations.
