# Directory semantic search (DIR-SEMANTIC-SEARCH-001)

`GET /directory/search` (public, `security: []`) ranks documents with `searchSql` in
`tdf-hq/src/TDF/Server/Directory.hs`. Its `semantic_score` was 1 when the query matched
a catalog alias of the document's taxonomy and 0 otherwise. Behind
`DIRECTORY_SEMANTIC_SEARCH_ENABLED` (off by default) the term becomes
`max(alias, embedding similarity)` for directory profiles and classifieds. Route,
parameters and response shape do not change.

The owner approved the rule and its constants on 2026-11-21, including sending public
document text and search queries to the configured embedding provider.

## Rule

For a non-empty query `q` and a document `d`:

```
alias(q, d)    = the 0/1 alias term, unchanged
sim(q, d)      = round4(clamp(cosine(E(q), E(d)), 0, 1))   with a usable embedding pair
               = 0                                          otherwise
semantic_score = max(alias(q, d), sim(q, d))
```

Every other term, weight, rounding step, the image boost, the filter, the facets and the
ordering are untouched:

```
organic = round6((.40*text + .15*semantic + .15*proximity + .10*quality
                  + .08*activity + .05*availability + .07*reputation) / 5)
rank    = organic + .10*image
order   = rank DESC, source_updated_at DESC, entity_kind, entity_id
```

An embedding pair is *usable* only when all of these hold:

- the flag is on, the configured model has 1536 dimensions, and `q` has at least 3
  characters after normalisation;
- `d` is a `profile` or `classified` row of `directory_search_document` (events, venues
  and the legacy artist rows keep the alias term);
- `d` has a stored embedding carrying the SHA-256 of `d`'s current revision (title,
  subtitle, summary, `search_text` and `source_version` of its search document), and
  the provider and model of the query embedding;
- a query embedding is available for this request (see Failure).

Similarity ranks; it does not admit. A document that fails the text filter is not
returned, whatever its similarity, so facets and totals are unchanged.

## Inputs and outputs

- Inputs: the existing search parameters, the flag, stored document embeddings and the
  query embedding.
- Output: the existing response. `scoreBreakdown.taxonomy` carries `semantic_score`,
  now a number in [0, 1] instead of 0 or 1. The OpenAPI type was already `number`; the
  contract file and the generated clients do not change.
- `score` stays in [0, 1]; its maximum is (.40*10 + .60)/5 = .92.

## What is sent to the provider

Only text the public directory already shows, after removing what looks like an e-mail
address or a telephone number, capped at 6000 characters:

| Kind | Text |
|---|---|
| profile | `public_name`, profession and instrument names, `bio`, `experience_summary`, `credits_summary`, `equipment_summary`, genre names, service names |
| classified | title, category name, description |
| query | the search text, lower-cased |

Never sent: anything from `party`, `directory_private_location`, coordinates, rates,
links, claim and verification data, moderation notes, payment data, messages,
applications, and any identifier of the caller. The redaction is a pattern match (a
word containing `@`; a run of digits and telephone punctuation with seven or more
digits), not a guarantee that free text holds no contact detail.

## Exclusions and privacy

- A document is eligible only while it satisfies the predicate search itself uses:
  `source_status='published' AND visibility='public' AND moderation_status='allowed'`,
  `effective_at` not in the future and `expires_at` not passed, and for profiles also
  the published, public, allowed and unmerged profile row.
- Embeddings are joined by primary key onto rows the `documents` projection already
  returned. They cannot add a row.
- Search is not personalised: no per-user signal is read, so every caller gets the
  non-personalised result and there is nothing to opt out of.
- The query cache stores `sha256(provider, model, normalised query)` and the vector,
  not the text. Rows older than 30 days, and rows beyond the newest 5000, are deleted.

## Storage

Migration `tdf-hq/sql/2026-11-21_directory_semantic_search.sql`:

- `directory_search_embedding(entity_kind, entity_id, content_hash, provider, model,
  embedding vector(1536), embedded_at)`, cascading from `directory_search_document`;
- `directory_query_embedding(query_hash, provider, model, embedding vector(1536),
  created_at)`;
- `directory_query_embedding_budget(window_started_at, calls)`, provider calls made by
  search requests per minute;
- trigger `directory_search_embedding_guard_trigger`: an update that hides a search
  document, leaves it expired or not yet effective, or changes its revision deletes
  its embedding in the same transaction.

The revision is hashed, not `search_text` alone: `search_text` is a lossy
normalisation (it drops non-Latin text, for one), so an edit it does not show would
otherwise keep a stale embedding. `source_version` changes with every edit of the
profile or classified, which covers the profile fields that are sent but are not
columns of the search document.

The migration requires the `vector` extension and refuses without it. No approximate
index is created: the join is by primary key, so the cosine is exact.

## Concurrency

A background sweep every 60 seconds, never in a request:

1. deletes embeddings whose document is not eligible or has other text (backstop);
2. prunes the query cache and the budget windows;
3. claims eligible documents with no embedding of their current text in the current
   space, reading text and hash in one statement;
4. calls the provider outside any transaction;
5. writes each vector with one statement that locks the document row and inserts only
   if the document is still eligible and still has the claimed hash.

Invariants:

- **I1 (never served).** A hidden document is never in a search response.
- **I2 (never indexed).** After every committed transaction each stored embedding
  belongs to a document whose own row is public and to its current text.
- **I3 (never stale).** An embedding of other text never contributes to a score.
- **I4 (sent only while public).** Text reaches the provider only if the document was
  eligible when claimed. A document hidden after the claim has already been sent; the
  guarded write discards the result.

I2 is transactional for the columns of the search document. Two changes reach no
trigger: a profile hidden without its search document being refreshed, and a document
whose `expires_at` passes with the clock. Both are caught by the read predicate at
once (I1, I3: search reads `directory_public_search_document`) and by the sweep within
a minute.

Several API processes may sweep at once; they may embed the same document twice, and
the guarded write makes the second a replacement by the same vector.

## Failure

- Flag off, or no query embedding: the handler executes today's `searchSql` text.
- No query embedding means: the query is too short, the tables are missing, the model
  does not fit, the cache misses and the minute's budget of 60 calls is used up, or
  the provider fails, answers after 400 ms, or returns a vector of the wrong
  dimension, a non-finite value or a zero vector.
- With a query embedding, a document without a usable embedding has `sim = 0`, so its
  `semantic_score` is the alias term and its score is today's.
- If the semantic statement itself fails, the handler runs today's statement.
- The RAG path substitutes a local bag-of-words vector when the provider fails.
  Directory search does not: a failed call yields no embedding.
- Pagination: the order is total. If the query embedding is available for one page
  and not the next, the pages are ordered by different scores and an item can repeat
  or be skipped, as already happens when data changes between pages. The cache makes
  this rare; it is not excluded.

## Constants

| Constant | Value |
|---|---|
| flag | `DIRECTORY_SEMANTIC_SEARCH_ENABLED`, default `false` |
| provider, model | OpenAI, `OPENAI_EMBED_MODEL` (the RAG setting); 1536 dimensions |
| semantic weight | `.15` (unchanged) |
| minimum query length | 3 characters |
| request wait for a query embedding | 400 ms |
| provider calls by search requests | 60 per minute, all processes |
| sweep interval, batch | 60 s, `RAG_EMBED_BATCH_SIZE` |
| text cap per document | 6000 characters |
| query cache | 30 days, 5000 rows |

## Implementation and evidence

| Part | Where |
|---|---|
| Rule, plan, text and SQL (pure) | `tdf-hq/src/TDF/Directory/SemanticSearch.hs` |
| Sweep and query lookup | `tdf-hq/src/TDF/Directory/SemanticIndex.hs` |
| Handler | `searchDirectory` in `tdf-hq/src/TDF/Server/Directory.hs` |
| Schema | `tdf-hq/sql/2026-11-21_directory_semantic_search.sql` and its rollback |
| Properties of the pure core | `tdf-hq/test/TDF/Directory/SemanticSearchSpec.hs` |
| PostgreSQL behaviour | `tdf-hq/test/TDF/Directory/SemanticSearchPostgresSpec.hs`, `scripts/test-directory-semantic-search.sh` |
| Ranking proof | `formal/directory/semantic_rank.py` (Z3) |
| Interleaving model and proof | `formal/directory/DirectoryEmbeddingIndex.tla` (TLC, Apalache) |

| Property | Evidence | Strength |
|---|---|---|
| P1 **critical**: for a non-empty query, an exact normalised title match ranks above every other match for all component values in range and any `semantic_score` in [0, 1] | Z3: exact ≥ .80 against ≤ .78 for the rest, with rounding; the weights are parsed from the pinned SQL text; three necessity controls. QuickCheck on the reference. One PostgreSQL case | Proved for the real-number model of the formula. That PostgreSQL evaluates the SQL as the model says is tested, not proved |
| P2 **critical**: I1 to I4 under publish, hide, edit, claim, write, abandon, purge and search | Apalache inductive invariant for up to three documents and three text versions; TLC with four negative controls; PostgreSQL cases for the trigger, the guarded write and the purge | Proved for the model. The model treats each statement as atomic; that the row lock gives this in PostgreSQL is assumed. Correspondence of model and SQL is by review and test |
| P3 **critical**: flag off or provider unavailable gives today's ranking exactly | The plan is lexical in both cases and a lexical plan executes the unmodified `searchSql` (QuickCheck and examples). Z3: with `sim = 0` the weighted sum equals today's term for term. PostgreSQL: rows of the semantic statement equal today's rows when no pair is usable | By construction for the statement text; proved for the formula; tested on PostgreSQL |
| P4: `score` in [0, 1] | QuickCheck; Z3 | Tested; proved for the model |
| P5: the page order is total | QuickCheck on the comparator; one PostgreSQL paging case | Tested |

Assumptions the proofs rest on: quality, availability and reputation are in [0, 1]
(CHECK constraints of `directory_search_document`); `source_updated_at` is not in the
future (otherwise the activity term leaves [0, 1], as it does today); distances are not
negative.

## Owner decisions

Taken on 2026-11-22, each the recommended default:

- Similarity only reorders documents that pass the text filter; it does not admit.
  Its weight is .03 of the score against .24 between title tiers, so the visible
  effect is small by design. Admission is not planned in this requirement.
- The constants stand as built, including the query cache retention of 30 days and
  5000 rows. All can change without touching the proofs except the semantic weight and
  the clamp.
- The pattern-match redaction of contact details stands.
- A page ordered with a query embedding followed by one ordered without it may repeat
  or skip an item; accepted.
- The flag stays off by default. On 2026-11-23 the owner decided to turn it on in
  production: `DIRECTORY_SEMANTIC_SEARCH_ENABLED=true` is set in the production API
  environment file. It has no effect until production runs a backend build that
  contains this change, and the feature stays inert until the migration is applied
  there.

## Still open

- A changed embedding model with another dimension needs a migration; until then the
  feature stays inert.
- No HTTP-level test drives the route with the flag on, no test calls the real
  provider, and the PostgreSQL fixtures are classified documents, not profiles.
