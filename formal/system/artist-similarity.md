# Multi-signal artist similarity (COMMUNITY-ARTIST-SIMILARITY-001)

Status: the rule and its constants were approved by the owner on 2026-11-21 as
proposed. Implemented behind `FAN_MULTI_SIGNAL_RECOMMENDATIONS_ENABLED`, off by default.
Extends COMMUNITY-FAN-RECOMMENDATION-001 ([fan-recommendations.md](fan-recommendations.md),
pull request 805), which stays the behaviour while the flag is off.

## Inputs

For each fan `f` and artist party `a`, an integer weight `w(f, a)` is the sum of four
components read from one database snapshot:

| Signal | Source | Counts when | Points |
|---|---|---|---|
| Follow | `fan_follow` | row exists | 8 |
| Favourite | `music_favorite` → `music_release_track` → `music_release_version` → `music_release.artist_party_id` | at least one favourited recording of the artist | 8 |
| RSVP | `event_rsvp` → `event_artist` → `social_artist_profile.party_id` | at least one `accepted` RSVP to an event in `directory_public_rsvp_event` with the artist in the lineup | 4 |
| Play | `music_playback_event` → `music_release_version` → `music_release.artist_party_id` | `eligible_play`, `party_id IS NOT NULL`, `occurred_at` later than 180 days before the snapshot | 1 for 1–2 plays, 2 for 3–9, 3 for 10 or more |

So favourite = follow (8) > RSVP (4) > play (at most 3), and `0 <= w(f, a) <= 23`.
Several favourites or RSVPs for one artist count once. `maybe` and `declined` RSVPs,
anonymous plays, plays the platform did not mark eligible, and a party's signals on its
own artist profile are not evidence. An artist that is not publicly listed
(ART-PUBLIC-VISIBILITY-001) has no vector at all.

## Similarity

For listed artists `a # b`:

- `dot(a, b) = SUM_f w(f, a) * w(f, b)`, `norm2(a) = SUM_f w(f, a)^2`,
  `support(a, b) = |{f : w(f, a) > 0 /\ w(f, b) > 0}|`.
- `sim(a, b) = dot(a, b) / sqrt(norm2(a) * norm2(b))` when `support(a, b) >= 2`,
  otherwise 0. Symmetric, and `0 <= sim <= 1` (Cauchy–Schwarz).

Unlike pull request 805, the caller's own rows are part of the vectors: the similarity
is one global relation, which is what lets a table hold it.

## Output and ordering

For a fan with seeds `S = {s listed : w(fan, s) > 0}` and seed strength
`u(s) = min(w(fan, s), 8) / 8`:

- `score(c) = SUM_{s in S, s # c} u(s) * sim(s, c)`; hence `0 <= score(c) <= |S|`.
- Candidates with `score > 0` are returned as `similar_fans`, ordered by score
  descending, then follower count descending, then artist id ascending. Artist ids are
  unique, so the order is total. Each names up to three seeds, by contribution
  descending then id.
- The list is completed with the popularity fallback of pull request 805, unchanged
  (most followed, at least one follower, `popular`, score 0).
- `limit` default 6, clamped to 1..24, unchanged. The response shape is unchanged, so
  the OpenAPI contract and the Mobile pin do not change.

## Exclusions

Evaluated at request time on live data, never from the table:

- not publicly listed (as a result **or** as a named or contributing seed);
- already followed by the caller, or the caller's own party;
- not followable under `social_v2_profile_eligible` where it is installed.

A seed (named or contributing) must be listed, must not be the caller's own party, and
must be followed by the caller or followable under that policy (`frameSeedEligible`).
A block in either direction, a closed profile or an inactive account therefore removes
an artist the fan favourited, played or RSVPed to from the evidence as well as from the
results; an artist the fan still follows remains a seed.

An artist hidden or blocked after the job ran is therefore dropped at once, as result
and as evidence. An artist the fan played or favourited but does not follow remains a
candidate.

## Precomputed table and reference

- **Reference.** `similarityFromWeights` (pure, total) maps the weight matrix to
  `(dot, support)` per pair and `norm2` per artist. `loadSignalSimilarity` feeds it the
  signal rows the transaction sees. `loadSimilarityRecommendations LiveSignals` ranks
  with it per request; it stays in the code as the reference and no route calls it.
- **Table.** A job (`startArtistSimilarityJob` in `TDF.Cron`: at start, then daily at
  03:00 local; session advisory lock 8401320261121, as the other daily jobs) runs
  `runSimilarityGeneration` in one `REPEATABLE READ` transaction. It calls the same
  `loadSignalSimilarity` and inserts one row in `artist_similarity_generation(id,
  snapshot_at, completed_at, artist_count, pair_count)`, the norms in
  `artist_similarity_norm(generation, artist_party_id, norm2)` and the pairs with
  `support >= 2` in `artist_similarity_pair(generation, artist_a < artist_b, dot,
  support)`. Integers only; the division and the square root happen in
  `similarityValue`, the same Haskell function on both paths.
- **Publication.** The generation and all its rows commit together, so a visible
  generation is complete. A request reads the table with one statement
  (`generationSimilaritySql`), hence from one snapshot: the pairs of the newest
  generation that touch the caller's seeds, with both norms.
- **Refinement.** For every generation `g`:
  `table(g) = similarityFromWeights(weights at snapshot_at(g))`. The request result is
  `rankBySimilarity(live fan inputs, table(g))`, which equals the reference ranking on
  that snapshot's similarity.
- **Staleness.** With period `P = 24 h`, the newest generation is served only while
  `now - snapshot_at <= 48 h` (`maxGenerationAgeHours`, two periods). While runs
  succeed its snapshot is at most `P + run duration` old. Follows and unfollows
  committed after `snapshot_at` change the caller's seeds and exclusions at once and
  the similarity at the next run.
- **Failure.** No generation, or a newest one older than 48 h: the statement returns
  no rows and the request returns the popularity fallback only. A failed run rolls
  back and leaves the previous generation in place. A successful run deletes all but
  the two newest generations.
- **Migration.** `tdf-hq/sql/2026-11-21_artist_similarity.sql` (additive): the three
  tables, and two partial indexes for the per-fan reads of a request, on
  `music_playback_event(party_id, occurred_at)` for eligible signed-in plays and on
  `event_rsvp(party_id)` for accepted RSVPs. Neither table had an index starting with
  the party.

## Privacy and opt-out

- Results name artists only. No other fan's identity is read at request time; the job
  reads signal rows and stores only per-pair and per-artist aggregates.
- Favourites, plays and RSVPs are private data (RSVPs are hidden from profiles by
  default). `support >= 2` means no similarity rests on one fan alone.
- **Opt-out invariant.** For a fan whose `social_v2_preference.personalized` is false:
  `result = popularity fallback(limit, live exclusions)` exactly — the fan's favourites,
  RSVPs and plays are not read and there is no `similar_fans` entry. The same fan's
  favourites, plays and RSVPs are left out of everyone's vectors from the next run;
  their follows, which are public, remain. No row means the default, personalised.
- **Reaching the switch.** The switch is written by `PUT /social/v2/preferences`, which
  answers only where social v2 is enabled, and its web control is behind
  `VITE_SOCIAL_V2_ENABLED`. Where neither is on, a fan cannot opt out. Enabling the
  flag below therefore has a precondition: fans can reach the switch.
- **Flag.** `FAN_MULTI_SIGNAL_RECOMMENDATIONS_ENABLED` (`TDF.Config`, default false,
  invalid values refuse startup) gates the job and the read path. Off: pull request 805
  behaviour. The feature registry (`tdf-hq/assets/feature-registry.json`) describes
  navigable routes; this change adds no route, so it has no registry entry.

## Properties and evidence

Three strengths of evidence are used and kept apart: **proved** (machine-checked for
all cases of a stated model), **model-checked** (exhaustive within stated bounds) and
**tested** (examples and random cases). A proof is about the model or the hand
translation named; the step from there to the running code is by review unless a test
is listed.

| Property | Evidence | Level |
|---|---|---|
| A result is listed, not followed, not the caller and followable under the policy; a named seed is listed, is the caller's, and is followed or followable under the policy (critical) | `formal/fans/artist_similarity_ranking.py`: Z3, hand translation of `rankBySimilarity`, `frameSeedEligible`, the seed restriction in `loadSimilarityRecommendations`, the fallback and `loadRecommendationFrame`, pinned by source hash, with witnesses and guard-necessity controls | proved for the translation |
| | QuickCheck properties in `ArtistSimilaritySpec`; hidden artist, blocked favourited artist (stored and live), later follow and later hiding on PostgreSQL (`ArtistSimilarityProbe`) | tested |
| The order is total and deterministic (critical) | Same script: the sort keys are strict total orders on distinct ids over IEEE doubles that are numbers; a contribution and a sum of contributions are numbers; allowing NaN breaks it | proved for the translation |
| | QuickCheck: row-order independence of the weights, sorted and duplicate-free keys | tested |
| The table equals the reference on the job's snapshot; a request uses it only while fresh; staleness bound (critical) | `formal/fans/ArtistSimilarity.tla`: Apalache inductive invariant (initiation, consecution, implication) for any subsets of three fans and three artists, any period, run duration below the period and age limit in the stated ranges, runs of any length, with and without failing runs | proved for the model |
| | TLC, two fans and two artists, with and without failures; two negative controls (reads without one snapshot; no age guard), each also reproduced by Apalache | model-checked |
| | QuickCheck: the rows a request reads rank exactly like the whole relation. PostgreSQL: the stored read equals the reference for every fixture fan after each run | tested |
| `0 <= sim <= 1`, `0 <= score <= number of seeds`, weights in `1..23` | QuickCheck | tested |
| Opt-out yields exactly the fallback | QuickCheck (no seeds, or no similarity, equals `rankArtistRecommendations` without evidence); PostgreSQL fixture fan | tested |
| Monotonicity, as restated below | QuickCheck, plus the counterexample as a fixed case | tested |

Not established: that the model's single abstract signal and "two shared fans"
relation carry over to the weighted relation (the model does not depend on the
weights, by construction, but this is an argument, not a check); that PostgreSQL's
`REPEATABLE READ` gives the snapshot the model assumes (documented behaviour); a
concurrent write during a real job run on PostgreSQL (not tested); the handler and
scheduler wiring (compilation and review); an HTTP-level run on a fully migrated
database; the migrated `directory_public_rsvp_event` view and `social_v2_preference`
table themselves (the fixture uses stand-ins with the migrated column types).

**Monotonicity as requested does not hold for weighted cosine.** With two fans,
`a = (1, 1)` and `b = (1, 1)` give `sim = 1`; raising the first fan's weight on `a`
to 2 gives `3 / sqrt(10) = 0.949`. Any normalised measure has such cases. What does
hold and is tested (approved restatement): (1) a new signal never lowers `dot(a, b)` or
`support(a, b)`; (2) when all weights are equal (the follows-only case), a new shared
fan never lowers `sim(a, b)`. The counterexample is kept as a test so the limit stays
visible.

## Open for the owner

1. Opt-out reachability: reuse of `social_v2_preference.personalized` was taken as the
   default. Before the flag is enabled anywhere, either social v2 is on there or a
   dedicated Fan Hub control is added (that changes the OpenAPI contract and the Mobile
   pin).
2. Radio sequencing and event ordering (the two follow-up changes) need new targets:
   `TDF.ServerRadio` is a station catalogue without sequencing (the play queue is in
   `tdf-hq-ui/src/player/queue.ts`), and `TDF.Services.EventDiscovery` is ingestion
   (user-facing order is `listEvents` in `TDF.Server.SocialEventsHandlers`).
3. The job hour (03:00 local) is a constant; make it configurable if operations need it.
