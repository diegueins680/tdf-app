# Snapshot and database-error privacy contract

EO-045/051/055: an event snapshot is one authorized projection, not separately authorized reads
assembled across grant changes or different instants. Acquire a shared feature-flag lock and then
the event-state/authorization-epoch lock. Only afterwards sample one wall-clock instant, check
current access and build all fields from one SQL statement. Grant changes and lifecycle commands
wait for the shared state lock; stale RR/Serializable readers abort. Unknown, disabled or unreadable
snapshots return no data. A missing schema retains the existing feature-disabled API behavior.

Capabilities and available transitions use the same sampled instant. An approval must not be
advertised to its own requester or without a recorded requester, even if the actor has approval
scope. This does not promise that a displayed transition can later execute without revalidation,
required reason, or a current expected version. Snapshot authorization linearizes at projection;
later revocation/expiry cannot retract information already authorized at that instant.

Errors crossing the database boundary emit only an allowlisted event identifier and category.
Never emit exception messages, SQL, provider responses, credentials, documents, request values,
raw SQLSTATE bytes or decoder errors. Transaction serialization/deadlock failures may be categorized
separately without exposing payloads. Preserve asynchronous cancellation rather than swallowing it
as an ordinary database outage. Public failure responses retain the existing 503/code contract.

`SnapshotRead` models one read split around lock acquisition and projection, a revocable grant,
clock 0–3 with expiry 2, and two representative secret payloads. Negative controls must find early
authorization, mixed-clock projection and raw-log-field violations. It abstracts database locking
and cannot certify arbitrary SQL isolation or all possible secret strings; real PostgreSQL races
and Haskell property tests must refine those assumptions. `ReceiptReplay`, `EventLifecycle` and
Alloy scopes remain complementary. Feature-flag locking, SQL decoding and asynchronous exceptions
also require executable tests; they are not independently proven by this abstraction.

Add the SQL read function to the existing unmerged, non-production API migration. Rollback disables
the feature and removes read/write functions while retaining audit/receipts/authorization epoch.
The application must map NULL to not-found and validate JSON into the existing snapshot DTO without
returning a partial object. No public DTO or generated client changes are intended. Global legacy
handlers, guest/organization administration, complete offline queues, HTTP authentication journeys,
observability infrastructure and rollout remain separate work; do not activate production here.
