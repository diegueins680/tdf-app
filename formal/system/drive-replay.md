# Drive upload replay — MEDIA-DRIVE-001

`POST /drive/upload` is an authenticated adapter that attempts public-reader
sharing. It is not a private-attachment store. Existing operations/artist access
checks and effective Google credential/folder selection remain in force.

A supplied 64-hex key is normalized and searched within the effective folder and
credential-visible provider scope. New uploads retain that key and store a
versioned SHA256 fingerprint in `appProperties.tdfRequestFingerprint`. The
fingerprint covers the server-authenticated Party, normalized effective filename,
MIME, destination folder and SHA256 of exact file bytes. It stores no OAuth token.
An identical sequential retry returns the original file and retries best-effort
sharing. A matching key with different or absent fingerprint returns409 before
upload, sharing or metadata effects. Duplicate lookup matches fail closed502.

The previous helper accepted any key/folder match without checking request
content or actor. AUTHORITY-031 classifies that behavior as an integrity bug.
Cross-actor misuse additionally requires authorized access to the same effective
provider account/folder and knowledge of the key; arbitrary file discovery or a
production compromise is not established. Existing unbound files are preserved,
not silently assigned to the next caller. Their intended provenance must be
reconciled explicitly. The artist worker retains its deterministic key format.

`DriveReplaySpec` replaces both http-client connection constructors with synthetic
in-memory HTTP wires. It uses no sockets, DNS, OAuth refresh or real provider
credentials. The fake provider returns the properties captured from the actual
first multipart request rather than recalculating the implementation fingerprint.
Tests compare request traces, require same-request success without a second
create, and reject changed content/name/MIME/actor and legacy unbound replays
before permission requests. Separate harness controls reject redirects and
unexpected destinations. Those transport controls constrain the fixture; they do
not claim production redirects are disabled. Compilation/execution receipts are
required before claiming these tests pass.

This is not a distributed idempotency proof. Lookup and create are separate
provider requests. Concurrent misses, provider visibility lag, process death after
create, out-of-band content/property changes and credential visibility changes
remain outside this repair. Google app properties are mutable metadata, not
cryptographic attestations. Authorization changes after initial handler admission
are not rechecked within the helper. Private-storage policy, public-permission
revocation and historical provider-object reconciliation remain separate work.

The primary-source decisions and their limits are recorded in `research.json`.
No real Google operation is required by this verification.

## Bounded model

`formal/event-operations/DriveReplay.tla` uses one idempotency key, two explicit
folders in one fixed credential-visible scope, two requests, two actors and two
content/name/MIME shapes. Provider entries are indexed by folder; each starts
absent, legacy (no fingerprint) or bound to an actor and shape. A request looks
up its own folder before creating, reusing or conflicting. Folder identity is
represented by the provider namespace, separately from the remaining fingerprint
shape. A conflict performs the lookup GET, then no create, permission or metadata
mutation; a reuse creates no object but may still run best-effort sharing.

The primary configuration checks `ReuseBinds`, `NoForeignReuse`,
`NoLegacyAdoption` and `ConflictNoMutation`: 33,168 generated states and 21,648
distinct states. Three guard-removal controls fail their respective reuse,
actor and legacy invariants. `DriveReplayRace.cfg` restricts both requests to the
same folder and still finds two creations after concurrent misses; it does not
claim distributed exactly-once creation.

`DriveReplayCrossFolder.cfg` starts both folders empty and runs the second
request only after the first finishes, with the same actor, content and key in
different folders. All 20 reachable states satisfy `CrossFolderCreates`: when
both requests finish, both created an object. The separate
`DriveReplayCrossFolderObjects.cfg` deliberately checks the false global
single-object claim and reaches two creations, demonstrating that the positive
terminal-state assertion is not vacuous. A local mutation that reads the first
folder for every lookup fails `CrossFolderCreates`.

These are safety checks on finite executions, without fairness or eventual
completion claims. Bounds exclude a missing effective folder (the implementation
then searches without a parent filter), fingerprint-version changes, credential
visibility changes, provider visibility lag, process death after create,
out-of-band property changes, duplicate-match lookup after both creates, and
best-effort sharing/metadata calls after the decision. Fingerprint equality is an
abstraction, not a cryptographic proof. Runtime correspondence is by source
inspection of `findDriveUploadByIdempotencyKey`, not refinement.

`DriveReplaySpec` remains the execution evidence for the existing handler guards.
Its historical nine-example result from 2026-10-12 is not a new execution receipt
for this model revision. No real Google operations are part of these checks.
