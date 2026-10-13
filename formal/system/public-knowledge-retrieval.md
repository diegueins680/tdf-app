# Public knowledge retrieval — PRIV-RAG-001

Customer-facing retrieval ranks course identities from the index and renders the
current course fields. An index entry does not authorize use of its stored text.
Historical private chunks can remain in the index; retrieval must exclude them,
including a private-source entry whose identifier matches a public course slug.
Index retention and disclosure to an embedding provider remain separate open
obligations. This contract does not approve either practice.

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
