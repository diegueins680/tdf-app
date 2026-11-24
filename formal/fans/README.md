# Fan recommendation proofs

COMMUNITY-ARTIST-SIMILARITY-001. Contract: [../system/artist-similarity.md](../system/artist-similarity.md).

## Job, table and requests (`ArtistSimilarity.tla`)

The nightly job reads the signals one artist at a time while follows and unfollows
commit, and publishes one complete generation or fails. A request uses the newest
generation only while its snapshot is recent. Signals are abstracted to follows and
the similarity to "at least two distinct fans follow both"; the model does not depend
on the weights.

| Variant | Meaning |
|---|---|
| `Snapshot = TRUE`, `AgeGuard = TRUE` | the implementation (`runSimilarityGeneration` under `REPEATABLE READ`, `generationSimilaritySql`) |
| `Snapshot = FALSE` | every read sees the rows committed at that moment (negative control) |
| `AgeGuard = FALSE` | a request uses the newest generation whatever its age (negative control) |

`Safe` = the stored relation is the reference on the rows committed at its snapshot;
a request that used the table ranked with the reference of a snapshot at most `MaxAge`
old; without failing runs the newest generation is at most `Period + Duration` old.

### Results (`npm run verify:inductive-proofs`)

| Obligation | Tool | Result |
|---|---|---|
| Two fans (one changing its follows), two artists, no failing runs, time 0..5 | TLC 1.7.2 | no violation (7,876 states) |
| Same with failing runs, time 0..7 | TLC | no violation (55,892 states) |
| Reads without one snapshot | TLC | `TableRefinesReference` violated, as expected |
| Failing runs and no age guard | TLC | `ServedRefinesReference` violated, as expected |
| `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′`, `IndInv ⇒ Safe` | Apalache 0.62.4 | **proved** |
| Both negative controls: reachable violation | Apalache | counterexample found, as expected |

**Scope of the proof.** Executions of any length and every interleaving, for any
subsets of three fans and three artists, any set of fans that never change, a period
of 1..6 ticks, a run duration below the period, an age limit of 0..12 ticks, with and
without failing runs. Assumptions in the model: a due run starts before time passes,
a run ends within `Duration`, and a `REPEATABLE READ` transaction reads the rows
committed when it started. The step from the model to the Haskell and SQL is by
review and by `scripts/test-artist-similarity-postgres.sh`; it is not mechanically
refined.

## Exclusions and order (`artist_similarity_ranking.py`)

Z3 proofs over a hand translation of `rankBySimilarity`, the popularity fallback and
`loadRecommendationFrame`, pinned by the SHA-256 of each source region: every result
is listed, not followed, not the caller and followable under the policy; every named
seed is listed and the caller's; the sort keys are strict total orders on distinct
ids over IEEE doubles; no score is NaN. Each guard is removed in turn and must break
a proof. The script's docstring lists what it does not cover.
