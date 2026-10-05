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
