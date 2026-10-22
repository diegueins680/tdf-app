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
`courseToDoc` admits only a canonical stored identity and renders fields through
the shared public URL policy; missing or ineligible rows are omitted.
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
route-eligible courses ordered by latest update. SQL filters ASCII canonical
slugs before the 50-row limit; `courseToDoc` independently rechecks the stored
identity and applies the public course URL projection. It uses the existing public course
fields; its dedicated document type and chunk constructor cannot select arbitrary
private source kinds. Private booking titles, availability, campaigns, ads,
services, rooms, resources and studio notes are no longer collected for embedding.
The retained `RAG_AVAILABILITY_*` configuration fields are compatibility inputs
with no effect on this index; they do not authorize private-source indexing.

Before checking index age or validating embedding configuration, worker checks and
administrator refreshes commit deletion of non-course derived chunks. This cleanup
requires no provider call and cannot be skipped merely because the public index is
fresh. Cleanup failure aborts before embedding. A missing table skips cleanup and
retains the existing schema-validation failure. No source business record is removed.

A successful nonempty refresh replaces the remaining public index transactionally.
An empty corpus clears it without a provider request. Provider refusal or response
count mismatch leaves the previous public index intact; insertion failure rolls back
public replacement and propagates an error. The separately committed legacy cleanup
is never restored by a failed refresh. `runSqlPool` owns commit and rollback; redundant
manual transaction control is removed. The original transaction pattern also
preserves rows: an initial temporary-table fixture falsely suggested otherwise
because pool reconnection discarded its tables. Schema/model validation still
precedes refresh.
Historical non-course chunks are removed at the next admitted worker check or manual
refresh even if embedding cannot succeed; PRIV-RAG-001 excludes them from retrieval
before that check. Deployment must drain old mixed-source writers, and recovery
must retain the public-only builder and cleanup. Privileged external SQL writes,
old-backend rollback, source records, backups and provider-held historical inputs
are outside this local cleanup guarantee. Query
embedding and other assistant payloads are separate privacy boundaries.

`refreshRagIndexWith` exposes an embedding-adapter seam used by PostgreSQL tests;
production `refreshRagIndex` always supplies the existing `embedTexts` adapter.
`RagIndexPrivacySpec` captures the actual payload and checks course-only persisted
rows, fresh-index cleanup, cleanup before invalid configuration, empty-corpus
no-call cleanup, cleanup failure preventing provider admission, provider refusal,
count mismatch and rollback
after a database CHECK rejects insertion. Per-test nonce schemas shadow a fully migrated disposable fixture and retain
rows across connection replacement; every new connection restores its search path.
No provider call or production write is needed. Run
`scripts/test-rag-index-privacy.sh` with `TDF_IDENTITY_HTTP_DATABASE_URL` pointing
to that fixture. Backend CI executes the runner after migration verification.

A read-only [production aggregate](evidence/2026-10-15-rag-legacy-index/production-aggregate.json)
observed backend `7ab344400d0878838b9b12742ca4cc5cd7f38f94` retaining 19 course
and 37 non-course chunks, last refreshed on September 29. No content was read and
no provider call was made. This demonstrates the need for cleanup independent of a
successful embedding refresh; it does not identify the content of those chunks or
prove provider-side retention. That observation precedes delivery of this repair.

### Refresh model and limits

`PublicKnowledgeIndex.tla` models one refresh, zero or one captured public course,
two private document classes and an initially mixed historical index. `Cleanup`, `Collect`,
`Empty`, `Embed`, `Publish` and `Reject` represent committed legacy removal,
collection, external disclosure, atomic public publication and failure. Provider failure, malformed response and public-replacement
rollback are abstracted by `Reject`; rejection before collection additionally
represents invalid configuration after cleanup. Failed refresh preserves the original
public subset. Freshness scheduling and cleanup SQL failure are tested in PostgreSQL, not
modeled. `Cleanup` models a successful committed purge; the model does not claim
legacy removal when that database operation fails. The model assumes atomic database replacement
and an immutable captured corpus; it does not prove arbitrary Haskell/SQL refinement,
field sanitization, provider retention, multi-refresh serialization, query privacy,
crash recovery or delivery freshness. Public field classification is an explicit
contract assumption. There is no fairness or liveness claim; stalled refreshes are
allowed. Concurrent refreshes may publish older captured public data; retrieval
continues to render current metadata under PRIV-RAG-001.

The pinned runner checks `TypeOK`, `NoLegacyAfterCleanup`, `NoPrivateEmbedding`, `SuccessfulReplacement`,
`FailedRefreshPreservesIndex` and `EmptySkipsProvider`. Four controlled mutations
must fail by named invariant: skipped legacy cleanup (`NoLegacyAfterCleanup`),
private source collection (`NoPrivateEmbedding`),
empty-corpus retention (`SuccessfulReplacement`) and destructive failure handling
(`FailedRefreshPreservesIndex`). These are bounded model checks, supported by the
separate PostgreSQL implementation tests, not a universal correctness proof.


## Stored public projection — 22 October 2026

Public route requests normalize their slug before a case-sensitive lookup. A
stored course must therefore already have a canonical identity: lower-case ASCII
letters/digits/hyphens, at least one letter or digit, and at most 96 characters.
Normalizing a stored row during indexing could bind it to a different course.
The refresh query applies the canonical predicate with C collation before the
50-row limit; the document constructor checks it again. Ranked current-course
reads enforce the same boundary, including legacy course-kind index rows.

`TDF.Courses.PublicMetadata` shares slug eligibility and stored URL validation
with the public HTTP metadata path. Landing, map, WhatsApp and instructor-avatar
URLs use that sanitizer before embedding or retrieval output. Rejected stored
values are omitted; accepted values retain the existing trimming, HTTPS, public
host, path, fragment, encoded-control and WhatsApp-specific constraints.
`TDF.PublicUrl` holds the existing HTTP URL/path parser; Trials and course HTTP
wrappers delegate to it. Existing HTTP400 bodies and URL rules are preserved.

Actual Stack-selected GHC 9.10.3 compilation of Store, models, configuration and
shared policy runs against a fresh complete 211-migration synthetic PostgreSQL
16 schema. With the old Store, the five added regressions fail: unsafe structured
URLs in embedding and current reads, unreachable identities in embedding and
current reads, and an unreachable-only corpus reaching the adapter. With the
repair, all 14 database cases pass. The limit case puts 57 ineligible rows ahead
of the valid course. Six pure-policy examples include 200 generated idempotence
and canonical-identity checks. The embedding adapter is injected and retrieval
uses local embeddings: no provider request is part of this evidence. Owned
fixtures are removed and the database cluster stopped after each run.

These implementation checks refine the models' abstract public-course predicate;
the unchanged finite models still do not prove a URL parser or arbitrary SQL/
Haskell refinement. Prior bounded-model receipts retain their original source
identity. Complete hosted backend/HTTP qualification, independent review, provider
retention and production rollout remain separate obligations. No provider-side
historical erasure or live activation is established here.


### Self-resolving production cohorts

A canonical stored slug alone does not establish that the row is public. The
production metadata route resolves aliases and retired cohorts to the earliest
eligible cohort, creating one when necessary. RAG does not invoke that mutating
resolver or attach another cohort's metadata to the old identity. It admits a
production row only when the public route would return that same row: a canonical
`produccion-musical-` identity with an earliest session at least the existing
28-day lead threshold. Bare `produccion-musical` and configured non-cohort aliases
are excluded even if their stored rows have future sessions.

The public HTTP resolver and RAG share `isProductionCoursePublicSlug` and
`hasRequiredProductionLead` in `TDF.Courses.PublicMetadata`, using the existing
`minimumProductionStartDate`. Both RAG SQL queries mirror this predicate before
their limits. Rendering reads the current minimum session date and checks the
shared identity predicate again. This adds no course creation, registration or
session mutation to index work. Read snapshots and provider-admission timing keep
the concurrency limits above; no delivery-time or arbitrary SQL-refinement proof
is claimed.

Four new PostgreSQL cases fail against the previous Store and pass after repair:
51 retired cohorts cannot crowd eligible ordinary/current courses out of the
50-row corpus; retired/missing-session/alias-only rows make no adapter call; a
cohort ceases to be returned after its first session falls below the threshold;
and cached bare/configured aliases do not expose their stored fields. The first
case includes a later future session that must not override the earlier one and
admits the exact threshold day. All 18 database examples pass on a fresh complete
211-migration synthetic schema. Eight pure-policy examples include the existing
200 generated checks and explicit cohort/alias/lead-boundary cases. Earlier
14-case receipts retain their original source identities; they are not relabeled
as executions of this new policy. Provider requests and production writes remain
outside these rehearsals.

### Normalized production identity collisions

A legacy production row can share the public resolver's normalized identity
with a canonical row. The resolver considers both in descending update order;
discarding only the legacy row from RAG can therefore expose a canonical row
whose metadata that route does not return. RAG conservatively excludes every
ambiguous production identity, regardless of which row is newer or has valid
sessions. It does not change the HTTP resolver or select the competing row.

`ambiguousProductionCourseSlugs` uses the same `Text.toLower . Text.strip`
operations as the resolver. It counts normalized production identities from
stored slugs, including legacy whitespace and case variants. Reading the slug
column avoids relying on PostgreSQL collation or its narrower default trimming
semantics. Both selection statements bind the ambiguous identities as a text
array and exclude them before their limits. Production document rendering
rechecks the collision classification along with the current session boundary.
Ordinary courses retain their case-sensitive identity authority. Collision
removal allows an otherwise eligible production row to be indexed again.

The additional reads have the existing statement-snapshot limits: the corpus is
captured before provider execution, and collision changes after capture or after
metadata read do not imply retraction or delivery-linearized freshness. The
extra slug scan also has a cost proportional to the stored course count; each
production rendering recheck repeats that scan. No schema mutation or provider
request is part of classification.

The index model now abstracts one collision Boolean for the captured corpus;
`NoAmbiguousEmbedding` excludes that corpus from disclosure. The read model
allows collision changes between ranking, metadata read and delivery;
`NoAmbiguousRead` refers to the classification at metadata read, allowing later
changes under the existing snapshot contract. The two new guard-removal
configurations must violate those named invariants. These bounded abstractions
assume the collision predicate, rather than proving Unicode normalization or
arbitrary SQL/Haskell refinement. Actual Store regressions and pure generated
normalization checks cover the separate implementation boundary.
