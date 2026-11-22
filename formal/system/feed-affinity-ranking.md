# Affinity ranking of the fan-club and discovery feeds (COMMUNITY-FEED-AFFINITY-001)

Status: **proposed, awaiting owner approval. No implementation exists.** Every constant
below is a proposal. Section "Open for the owner" lists what must be decided first.

## Today (main at 985117b3d)

- `GET /fan-clubs/{artistId}/feed?sort=hot|top|new` (`listClubFeed`,
  `tdf-hq/src/TDF/ServerFanClub.hs`) returns **every** top-level post and memory of the
  club that `Interactions.visible` admits, unpaginated. `sortFeedItems` puts pinned items
  first in input order, then sorts the rest. For `hot` the key is
  `hotScore = reactions / max(1, ageHours)^0.8`, descending, with a stable sort, so ties
  keep input order: posts before memories, each newest first.
- `GET /fans/discovery?limit=N` (`discoveryFeed`, `tdf-hq/src/TDF/Server.hs`) returns the
  `min(50, N or 20)` most recently boosted posts that `InteractionLegacy.visible` admits,
  ordered by `boosted_at` descending. **It does not use `hotScore`.**
- Items a moderator hid (`is_hidden`) are returned by the club feed, flagged
  `fcfIsHidden = true`, unless the canonical interaction resolver removes them. The
  discovery feed does not read `is_hidden` at all.
- Authors of feed items are fans (club members). `fan_follow` records fan-to-artist
  follows, so "an author the viewer follows" has no direct meaning in these feeds.

## Rule

Mode. A request is **personalised** only when all hold; otherwise it is **fallback**:

1. runtime gate `FAN_FEED_AFFINITY_ENABLED` is on (environment, default off, read in
   `TDF.Config` like `EVENT_DISCOVERY_ENABLED`);
2. the viewer has not opted out: `social_v2_preference.personalized` is not `false`
   (an absent row means `true`, as `social_v2` already reads it; an absent table means
   fallback);
3. the viewer has at least one unit of evidence (below) for at least one author.

Evidence. For viewer `v` and author `a` (`a # v`), over the last 90 days:

- `r` = items by `a` that `v` currently reacts to, counted by reaction `created_at`;
- `p` = replies by `v` (`fan_club_post.parent_id`) to posts by `a`;
- only items that are visible to `v` now and not hidden count. Blocked, deleted, hidden
  or unreadable items are not evidence.

Score.

- `n = r + 2p`
- `affinity(v, a) = 1 + min(n, 20) / 10`, so `1 <= affinity <= 3`; `affinity(v, v) = 1`
- `typeWeight(post) = 1.0`, `typeWeight(memory) = 1.2`
- `score = affinity * typeWeight * reactions / max(1, ageHours)^0.8`

Order of unpinned items, personalised `hot`: score descending, then kind (`post` before
`memory`), then `created_at` descending, then id descending. The key is unique per item,
so the order is total and does not depend on input order.

Fallback `hot` is the existing function, unchanged, followed by the same tie-break. The
tie-break agrees with today's stable order wherever today's order is determined and fixes
it where it is not (equal `created_at` within one kind).

Unchanged in both modes: pinned items first; `top` and `new`; which items are returned;
the response shape. No explanation, score or affinity is returned.

Discovery. The boosted window is selected exactly as today, before and independently of
ranking. Fallback keeps `boosted_at` order. Personalised mode orders the same window by
the score above, with `boosted_at` descending as the first tie-break.

## Properties and intended evidence

| # | Property | Critical | Evidence planned |
|---|---|---|---|
| P1 | **Permutation.** In either mode the returned multiset equals the fallback's. Ranking never adds or drops an item. | yes | Apalache inductive invariant; QuickCheck |
| P2 | **Bounded displacement.** With `R = 3 * 1.2 = 3.6`, an unpinned item `x` is preceded only by pinned items and unpinned `y` with `hot(y) * R >= hot(x)`. Its position is at most `pinned + #{y : hot(y) >= hot(x) / R}`, whatever the affinities. | yes | Z3 (reals; decay as an arbitrary positive value); TLC bounded model; QuickCheck |
| P3 | **Refinement.** Fallback order equals today's order on every input where today's is determined. With every affinity and weight equal to 1 the personalised key equals `hotScore` bit for bit. | yes | Z3 for the key; Apalache for mode selection; QuickCheck against the retained legacy function |
| P4 | **Total, deterministic.** The comparison key is injective on items; output is independent of input order. | yes | Z3 (strict total order); QuickCheck (shuffle) |
| P5 | **Exclusion.** No item outside the visible set is returned or used as evidence. | yes | Apalache invariant over the model; SQL extracted from source run on PostgreSQL |
| P6 | Score monotone in reactions and affinity, anti-monotone in age. | no | QuickCheck only |

Negative controls for the bounded model: floor 0 (P2 fails), no tie-break (P4 fails),
evidence read before the visibility filter (P5 fails), opt-out ignored (P3 fails).

What the proofs will not cover: the Haskell `Double` arithmetic of `** 0.8` (tested, not
proved), the SQL (tested on a fixture), and the handler wiring (compiled and reviewed).

Starvation as worded in the request ("appears within a bound of positions or time") is
P1 + P2. The club feed is unpaginated and the discovery window is chosen before ranking,
so no visible item can be ranked out of a response; P2 bounds how far down it can move.

## Boundaries

- **Privacy.** Reads only the viewer's own reactions and replies. Nothing derived from
  them is returned except the order. The order is a function of (viewer, visible items).
- **Authorization.** Unchanged: the routes' existing guards.
- **Concurrency.** Evidence and items are read in the request's transaction at the
  pool's default isolation. A reaction landing between the reads changes at most that
  request's order; P1 to P5 hold for any evidence values.
- **Failure.** If evidence cannot be read (missing table, query error) the request is
  served in fallback mode; it never fails because of personalisation.
- **Schema.** None proposed. Evidence is computed per request from existing rows.
- **Contract.** No route, parameter or field changes, so no OpenAPI or mobile change.
- **Web.** No visible change.

## Open for the owner

1. **Constants.** Window 90 days; reply weight 2; cap 20 interactions; affinity range
   1 to 3; memory weight 1.2. None has data behind it.
2. **Discovery.** It has no `hotScore` order today. Proposed: fallback = today's
   `boosted_at` order, personalised = score order over the same window. Alternative:
   leave discovery untouched.
3. **Hidden items.** The request says hidden posts never appear; today the club feed
   returns them flagged. Excluding them changes `top`, `new` and fallback `hot`, which
   the request says must stay exactly as today. Proposed: exclude hidden items from
   evidence only and leave returned items as today (P5 then means "not visible to the
   viewer"). Alternative: drop hidden items for non-officers in every sort, as a
   separate, declared behaviour change.
4. **Zero reactions.** `score` is 0 for an item without reactions whatever the affinity,
   so affinity never lifts a close author's fresh post. Kept as requested. Alternative:
   `reactions + 1` in personalised mode only.
5. **Type weight and no-history viewers.** Refinement forces weight 1 for viewers
   without history, so memories gain 1.2 only once a viewer has any evidence.
6. **"Follows".** Read here as "every visible item", which is stronger.
7. **Evidence store.** Where canonical interactions are activated, reactions are read
   through `interaction_*` functions; the evidence query must use the same source.
