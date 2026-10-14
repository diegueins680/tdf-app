# Public knowledge retrieval — PRIV-RAG-001

Customer-facing retrieval ranks course identities from the index and renders the
current course fields. An index entry does not authorize use of its stored text.
Historical private chunks can remain in the index; retrieval must exclude them,
including a private-source entry whose identifier matches a public course slug.
Future refreshes follow PRIV-RAG-002 below. Historical provider retention remains
a separate open obligation; reindexing does not establish provider-side erasure.

## Implementation and executable checks

`tdf-hq/src/TDF/RagStore.hs:selectRagChunks` joins course identity, requires
`chunk.source = 'course'`, then loads each selected course with `getEntity`.
`courseToDoc` renders the public fields; `catMaybes` omits a missing row.
`retrieveRagContext` runs those reads in one `runSqlPool` transaction. The staff
assistant and Facebook, Instagram and WhatsApp workers call that boundary.
The assistant additionally requires authenticated Social Inbox authority; the
historical anonymous disclosure reproduction is not the current access policy.

The PostgreSQL/HTTP fixture in `scripts/test-booking-conformance.py` checks
anonymous and unprivileged denial, private and unsupported source exclusion,
poisoned cached text, updated public metadata and deleted-course exclusion.
Its colliding-slug case leaves only non-course entries for an existing course,
requires no document, then restores an eligible course entry and requires the
current public result. A join alone cannot satisfy that case. Calls use the
synthetic staff account, local embeddings and a disposable database; no provider
delivery is tested. CI runs the fixture as part of backend quality.

## Bounded state machine

`formal/event-operations/PublicKnowledgeRead.tla` separates `Rank`,
`ReadMetadata` and `Deliver`. Rejected ranking finishes with no document.
Environment edits and deletion can commit before, between or after those steps.
One request, one course identity, two public metadata values and two source
classes form the finite state space. All non-course kinds are abstracted as
`private`; indexed raw text is a distinct private sentinel even for course hits.
Course presence and source kind are nondeterministic initially. Deletion is
terminal for that identity; recreation, multiple rows and ranking order are not
modeled. The implementation stores only a selected ID; `selectedMetadata` is
ghost state used to demonstrate the unsafe earlier-snapshot mutation.

The safety invariants are:

- `AllowedSource`: a delivered document originated from an eligible course hit.
- `NoPrivateText`: raw indexed content is never delivered.
- `ExistingAtRead`: a returned course existed when its metadata was read.
- `CurrentMetadata`: the returned fields equal that metadata-read snapshot.
- `TypeOK`: every stage and payload stays within the declared abstraction.

Each SQL statement is atomic in the abstraction. Successive reads may observe
different committed states, matching PostgreSQL Read Committed; a transaction
alone does not promise one snapshot. We do not infer the production session's
isolation setting from the PostgreSQL default. Alternate isolation settings,
provider failures, field-level sanitization, embedding computation and arbitrary
Haskell/SQL refinement are excluded. Public-course field classification is an
existing contract assumption, not a theorem established by this model.

There is no fairness assumption and no liveness claim. A request may stall.
Deletion or editing after `ReadMetadata` may precede delivery of the admitted
snapshot: deletion-linearized delivery and cross-channel retraction are explicitly
not guaranteed. The model allows that behavior rather than proving a stronger
property the implementation does not provide.

## Negative controls and evidence

The shared pinned TLC runner executes the intended configuration and four broken
configurations, requiring the named invariant failure rather than any nonzero
exit. `Unfiltered` admits a non-course hit (`AllowedSource`); `RawText` returns
index text (`NoPrivateText`); `CachedMetadata` uses the ranking snapshot after an
edit (`CurrentMetadata`); `MissingFallback` returns an earlier snapshot after
deletion (`ExistingAtRead`). Guards change; the checked properties do not.

Run `scripts/verify-event-operations-formal.sh` with the pinned Java/TLC/Alloy
toolchain. Inventory and traceability bind model, implementation and fixture
fingerprints. A passing model is bounded specification evidence; the HTTP fixture
is implementation evidence. Neither establishes production delivery or a complete
refinement proof. Requirement status remains PARTIAL until the separate delivery,
provider-sharing and retention obligations have evidence.

## Public-only refresh — PRIV-RAG-002

The user explicitly approved “Public courses only; stop indexing private material”
on 2026-10-13 (AUTHORITY-RAG-PUBLIC-INDEX-20261013). That decision supersedes the
old multi-source index builder. `buildPublicCourseDocuments` selects up to 50
courses ordered by latest update. `courseToDoc` uses the existing public course
fields; its dedicated document type and chunk constructor cannot select arbitrary
private source kinds. Private booking titles, availability, campaigns, ads,
services, rooms, resources and studio notes are no longer collected for embedding.
The retained `RAG_AVAILABILITY_*` configuration fields are compatibility inputs
with no effect on this index; they do not authorize private-source indexing.

A successful nonempty refresh replaces the entire derived index transactionally.
An empty corpus clears it without a provider request. Provider refusal or response
count mismatch leaves the previous index intact; insertion failure rolls back the
delete and propagates an error. `runSqlPool` owns commit and rollback; redundant
manual transaction control is removed. The original transaction pattern also
preserves rows: an initial temporary-table fixture falsely suggested otherwise
because pool reconnection discarded its tables. Schema/model validation still
precedes refresh.
Historical private chunks can therefore remain until a successful refresh, but
PRIV-RAG-001 excludes them from retrieval throughout. Source records, backups and
provider-held historical inputs are outside this replacement operation. Query
embedding and other assistant payloads are separate privacy boundaries.

`refreshRagIndexWith` exposes an embedding-adapter seam used by PostgreSQL tests;
production `refreshRagIndex` always supplies the existing `embedTexts` adapter.
`RagIndexPrivacySpec` captures the actual payload and checks course-only persisted
rows, empty-corpus no-call cleanup, provider refusal, count mismatch and rollback
after a database CHECK rejects insertion. Per-test nonce schemas shadow a fully migrated disposable fixture and retain
rows across connection replacement; every new connection restores its search path.
No provider call or production write is needed. Run
`scripts/test-rag-index-privacy.sh` with `TDF_IDENTITY_HTTP_DATABASE_URL` pointing
to that fixture. Backend CI executes the runner after migration verification.

### Refresh model and limits

`PublicKnowledgeIndex.tla` models one refresh, zero or one captured public course,
two private document classes and an initially mixed historical index. `Collect`,
`Empty`, `Embed`, `Publish` and `Reject` represent collection, external disclosure,
atomic publication and failure. Provider failure, malformed response and database
rollback are abstracted by `Reject`. The model assumes atomic database replacement
and an immutable captured corpus; it does not prove arbitrary Haskell/SQL refinement,
field sanitization, provider retention, multi-refresh serialization, query privacy,
crash recovery or delivery freshness. Public field classification is an explicit
contract assumption. There is no fairness or liveness claim; stalled refreshes are
allowed. Concurrent refreshes may publish older captured public data; retrieval
continues to render current metadata under PRIV-RAG-001.

The pinned runner checks `TypeOK`, `NoPrivateEmbedding`, `SuccessfulReplacement`,
`FailedRefreshPreservesIndex` and `EmptySkipsProvider`. Three controlled mutations
must fail by named invariant: private source collection (`NoPrivateEmbedding`),
empty-corpus retention (`SuccessfulReplacement`) and destructive failure handling
(`FailedRefreshPreservesIndex`). These are bounded model checks, supported by the
separate PostgreSQL implementation tests, not a universal correctness proof.
