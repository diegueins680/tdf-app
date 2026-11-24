# Directory search proofs (DIR-SEMANTIC-SEARCH-001)

The contract is [`formal/system/directory-semantic-search.md`](../system/directory-semantic-search.md).
Two artefacts live here. Neither says anything about the quality of an embedding; they
bound what an embedding can do.

## Ranking envelope: `semantic_rank.py` (Z3)

Reads `searchSql` (`tdf-hq/src/TDF/Server/Directory.hs`) and `similaritySql`
(`tdf-hq/src/TDF/Directory/SemanticSearch.hs`) as pinned regions, parses the title
tiers, weights, divisor and image boost from the SQL text, and states the formula over
real numbers with six-decimal rounding.

| Obligation | Result |
|---|---|
| An exact title match ranks above every other match, for any two semantic terms in [0, 1] | **proved** |
| The same for the prefix tier over contains, and contains over the rest | **proved** |
| With similarity 0 the weighted sum equals today's, term for term | **proved** |
| The public score is in [0, 1] | **proved** |
| A larger similarity never lowers the weighted sum | **proved** |
| Similarity can reverse an order inside a tier; the exact-match margin is below .03 | witnesses found |
| Similarity not clamped, semantic weight doubled, image boost doubled | each breaks the exact-match proof, as expected |

Run: `python formal/directory/semantic_rank.py` with `z3-solver` 4.13.3.0. A change to
either pinned region fails the run until the pin is reviewed and updated.

Not proved: that PostgreSQL's `float8` and `numeric` evaluation agrees with the
real-number model (tested on PostgreSQL); that the stored components are in range
(CHECK constraints, and `source_updated_at` not in the future); the title and alias
predicates.

## Index interleavings: `DirectoryEmbeddingIndex.tla` (TLC, Apalache)

Documents are published, hidden and edited at any time while a sweep claims public
documents, sends their text, and later writes or abandons the vector; a purge and
searches interleave freely. Four switches model the four defences; each `FALSE` is a
negative control.

| Switch | Code |
|---|---|
| `GuardedWrite` | `writeDocumentEmbeddingSql`: insert only if still eligible with the claimed hash, under a row lock |
| `DeleteOnChange` | trigger `directory_search_embedding_guard_trigger` |
| `JoinOnly` | `semanticSearchSql`: embeddings are joined onto the public projection |
| `HashChecked` | `similaritySql`: the embedding must carry the hash of the current revision (title, subtitle, summary, search text, source version) |

`Safe` = `NeverServed` (a hidden document is never returned), `NeverIndexed` (every
stored embedding belongs to a public document and its current text), `NeverStale` (an
embedding of other text never ranks) and `SentOnlyPublic` (only text that was public
reached the provider).

| Obligation | Tool | Result |
|---|---|---|
| Bounded exploration, 2 documents, 3 text versions, all defences | TLC 1.7.2 | no violation |
| Unguarded write, 1 document | TLC | `NeverIndexed` violated, as expected: claim, hide, write |
| No trigger, 1 document | TLC | `NeverIndexed` violated, as expected: write, hide |
| Search driven from the embedding table, no trigger | TLC | `NeverServed` violated, as expected |
| No hash check, no trigger | TLC | `NeverStale` violated, as expected |
| `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′`, `IndInv ⇒ Safe`, all defences | Apalache 0.62.4 | **proved** for any nonempty set of up to three documents and up to three edits each |
| Unguarded write: reachable violation | Apalache | counterexample found, as expected |

Run: `bash scripts/verify-inductive-proofs.sh` (needs `APALACHE_TGZ`, `TLA2TOOLS_JAR`
and Java 17+).

The inductive proof covers executions of any length within those bounds on documents
and versions. Each action is one atomic step: that a PostgreSQL statement holding
`FOR SHARE` on the document row is atomic with respect to a concurrent update of that
row is assumed. The model's `public` is the search document's own publication state;
a profile hidden without its search document being refreshed, and a document that
expires with the clock, are outside the model and are covered by the read predicate
and the purge. That the SQL implements the actions
is established by review and by `tdf-hq/test/TDF/Directory/SemanticSearchPostgresSpec.hs`.
