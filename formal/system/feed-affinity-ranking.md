# Affinity ranking of the fan-club feed (COMMUNITY-FEED-AFFINITY-001)

Rule and constants approved by the owner on 2026-11-21. Off by default: the runtime
gate `FAN_FEED_AFFINITY_ENABLED` must be set before any viewer sees a personal order.

## Scope

`GET /fan-clubs/{artistId}/feed?sort=hot`. The `top` and chronological sorts, pinned
items, the set of returned items and the response shape are unchanged. The discovery
feed (`GET /fans/discovery`) is **not** changed: it orders boosted posts by `boosted_at`
and never used the hot score, so it is left for a later decision.

## Before this change

`sortFeedItems` put pinned items first in input order and sorted the rest by
`hotScore = reactions / max(1, ageHours)^0.8`, descending, with a stable sort. Ties kept
the handler's input order: posts before memories, each newest first. The order of two
items of one kind with the same `created_at` was whatever the database returned.

## Rule

**Mode.** A request is personalised only when all three hold; otherwise it is fallback:

1. `FAN_FEED_AFFINITY_ENABLED` is on (default off; `TDF.Config`).
2. `social_v2_preference` is installed and the viewer's `personalized` is not `false`.
   A viewer without a row has the column default, on. Where the table is absent the
   viewer cannot opt out, so the request is fallback.
3. The viewer has evidence for at least one author other than themself.

**Evidence.** For viewer `v` and author `a # v`, over the last 90 days:

- `r` = items by `a` that `v` reacts to now (by reaction `created_at`);
- `p` = replies by `v` to items by `a`;
- counted only on items of the feed being served that `v` may read and that are not
  hidden. Interactions with any other item are discarded.

Where canonical interactions are activated the rows come from `interaction_reaction` and
`interaction_comment` (visible comments); otherwise from `fan_club_post_reaction`,
`fan_club_memory_reaction` and non-hidden reply rows of `fan_club_post`.

**Score.**

- `units = r + 2p`
- `affinity = 1 + min(units, 20) / 10`, so `1 <= affinity <= 3`; 1 without evidence
- `typeWeight = 1` for posts and memories (a named constant, not yet differentiated)
- personalised: `score = affinity * typeWeight * (reactions + 1) / max(1, ageHours)^0.8`
- fallback: `score = reactions / max(1, ageHours)^0.8`, the previous expression

`reactions + 1` lets affinity lift an item nobody has reacted to yet.

**Order of unpinned items.** Score descending, then posts before memories, then
`created_at` descending, then id descending. `(kind, id)` is unique, so the order is
total and independent of input order. In fallback mode it equals the previous order
wherever that was determined, and fixes it where it was not.

Nothing about the score, the affinity or the mode is returned.

## Properties and evidence

Evidence levels: **proved** (solver, all inputs of the stated abstraction), **bounded**
(model checker, the listed finite scope), **tested** (QuickCheck or examples on the
Haskell code).

| # | Property | Proved | Bounded | Tested |
|---|---|---|---|---|
| P1 | Permutation: ranking returns exactly the items it was given | Apalache, on the model | TLC | QuickCheck on `sortFeedItems` |
| P2 | Bounded displacement: an item is preceded only by pinned items and items whose `(reactions + 1) / decay` is at least a third of its own | Z3 over the reals for any positive decay; Apalache on the model | TLC | QuickCheck |
| P3 | Refinement: gate off, opted out or no evidence gives the previous order; an opted-out viewer's behaviour is not read | Apalache (mode and order, model); Z3 (tie-break keeps the stable order) | TLC | QuickCheck against a copy of the previous sort; SQLite loader tests |
| P4 | The order is total and deterministic | Z3 (strict total order of the key); Apalache (order fixed by the key) | TLC | QuickCheck (shuffled input) |
| P5 | Nothing outside the visible set is returned; hidden or unreadable items are never evidence | Apalache, on the model | TLC | QuickCheck on `tallyEvidence` and `feedEvidenceTargets` |
| P6 | Score rises with reactions and affinity, does not rise with age | Z3, exact arithmetic only | | QuickCheck on the `Double` code |

Negative controls (each must fail): affinity floor 0 (P2), no tie-break (P4), evidence
read before the visibility filter (P5, P3), opt-out ignored (P3). In Z3: zero floor,
unequal type weights (the bound 3 no longer holds), key without the id.

**Starvation.** The feed is unpaginated, so P1 means every item the viewer may read is
in every response. P2 bounds how far it can be moved down: only items within a factor
of 3 of its base score can pass it.

Not proved:

- The models abstract the sort as "emit the item whose key precedes all waiting items".
  That the Haskell `sortBy` realises this is tested, not proved.
- The Apalache proof covers feeds of up to four items by three authors with abstract
  scores 0..6 and 1..12 and three affinity levels. The Z3 lemmas cover all reals.
- `Double` arithmetic, including `** 0.8`, is tested only.
- The canonical-interaction SQL runs against a fixture with the columns it reads, not
  against the migrated schema. Handler wiring is compiled and reviewed; no HTTP-level
  test serves a personalised feed.
- Which items are visible is decided by `Interactions.visible`, outside this change.

## Boundaries

- **Privacy.** Reads only the viewer's own reactions and replies, and nothing at all
  with the gate off or for a viewer who opted out. Only the order depends on them.
- **Hidden items.** Returned and flagged exactly as before; never evidence.
- **Authorization.** Unchanged.
- **Concurrency.** Items and evidence are read in the handler's transaction at the
  pool's default isolation. P1 to P5 hold for any evidence values, so a concurrent
  reaction can change one response's order and nothing else.
- **Failure.** A missing preference table gives fallback. Any other database error
  fails the request as it does for the rest of the handler.
- **Schema, contract, clients.** No migration, no OpenAPI change, no web or mobile
  change.

## Implementation and evidence

| Part | Where |
|---|---|
| Ranking (pure), evidence tally, SQL, loader | `tdf-hq/src/TDF/Fans/FeedAffinity.hs` |
| Handler | `listClubFeed`, `sortFeedItems` in `tdf-hq/src/TDF/ServerFanClub.hs` |
| Gate | `fanFeedAffinityEnabled` in `tdf-hq/src/TDF/Config.hs` |
| Properties and loader tests | `tdf-hq/test/TDF/Fans/FeedAffinitySpec.hs` |
| Canonical SQL on PostgreSQL | `scripts/test-feed-affinity-sql.py`, `scripts/test-feed-affinity-postgres.sh` |
| Model, bounded checks, inductive proof | `formal/social/FeedAffinity.tla`, `scripts/verify-inductive-proofs.sh` |
| Arithmetic lemmas | `formal/social/feed_affinity_arithmetic.py` |

## Open for the owner

- The constants (90 days, reply weight 2, cap 20, range 1 to 3) have no data behind
  them. Review after the gate has been on for a while.
- `typeWeight` is 1 for both kinds. Setting them apart widens the displacement bound to
  `3 * max / min` and P2 must be restated.
- The discovery feed is not personalised.
- Hidden items are still returned flagged to every viewer, as before.
- Evidence is limited to the club being viewed; interactions with the same author in
  another club do not count.
