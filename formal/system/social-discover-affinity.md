# Affinity order of the people in Discover

Requirement `SOCIAL-DISCOVER-AFFINITY-001`. Rule and constants approved by the owner
on 2026-11-27. Off by default (`SOCIAL_DISCOVER_AFFINITY_ENABLED`).

This is the discovery half of the affinity task that `COMMUNITY-FEED-AFFINITY-001`
deferred. The owner named the surface on 2026-11-27: `GET /social/v2/discover`, not
`GET /fans/discovery` (which has no data source and no caller).

## Scope

`GET /social/v2/discover?limit=N`. Only the order of the candidates changes. The
candidate set, the eligibility rules, the daily sample, the limit, the response shape
and the `reason` values do not change. No OpenAPI change and no Mobile change. On the
web only the label of the personalisation switch changes.

## Before this change

The surface lists **people**, not posts. `social_v2_discover(actor, page_size)`, for a
signed-in, live actor:

1. takes a sample of at most 200 parties that opted in (`discoverable`, not `closed`),
   rotated daily by `md5(party_id || current_date)`;
2. keeps those that are not the actor, not an organisation, have an active credential,
   and have no block, mutual consent, mute, dismissal or follow by the actor;
3. orders them by `shared_interest` descending (1 when the actor has not switched
   personalisation off and shares a genre with the candidate), then by
   `md5(id || current_date || actor)` (without the actor when personalisation is off),
   then by id;
4. returns the first `page_size` (1 to 50), each with `reason` = `shared_interests` or
   `public_profile`.

The web page then moves candidates that also appear in the PageRank suggestions
(`COMMUNITY-FRIEND-SUGGESTION-001`) to the front. That overlay is kept as it is.

## Rule

**Which function runs.** The request runs the three-argument function
`social_v2_discover(actor, page_size, true)` only when both hold; otherwise it runs
exactly the statement it ran before, `SELECT social_v2_discover(?,?)::text`:

1. `SOCIAL_DISCOVER_AFFINITY_ENABLED` is on (default off);
2. the three-argument function is installed.

Inside the three-argument function no behaviour is read when the actor's
`social_v2_preference.personalized` is `false`.

**Evidence.** For actor `v` and candidate `c`, over the last 90 days (date of the
interaction):

- `r` = top-level posts by `c` that `v` reacts to now;
- `p` = visible replies by `v` to top-level posts by `c`;
- counted only on posts that are not hidden and that are in a club `v` may read now,
  by the rule of `social_v2_feed`: `v` is the club's artist, follows the artist or is an
  officer of the club, and the artist is a live account that `v` has not muted and
  that has no block with `v`;
- `units(v, c) = min(r + 2p, 20)`.

Where canonical interactions are the authority (`interaction_runtime.activated_once`,
as in `TDF.Interactions.Legacy.activated`), reactions and replies are read from
`interaction_reaction` and `interaction_comment` on `club_post` targets; otherwise
from `fan_club_post_reaction` and reply rows of `fan_club_post`.

These are the constants of `COMMUNITY-FEED-AFFINITY-001` (window 90 days, reply weight
2, cap 20). A candidate has no reactions or age of its own, so there is no decay term:
the only clock is the 90-day evidence window.

**Order.** Among the candidates of step 2 above:

1. the `K` candidates with the largest `units > 0` lead, by `units` descending, then
   id ascending; `K = min(page_size div 2, number of candidates with units > 0)`;
2. every other candidate follows in exactly the previous order (step 3 above).

So at most half of a page is chosen by affinity and the rest keeps the daily rotation.
A page of one is never changed.

**Explanation.** None. `reason` keeps its two values and its meaning; no units, rank or
mode is returned.

## Properties and evidence

Evidence levels: **proved** (solver, all cases within the stated model), **bounded**
(model checker, the stated sizes), **tested** (the implementation on examples).

| | Property | Evidence |
|---|---|---|
| P1 | Refinement: with the flag off, for an opted-out actor, or for an actor with no usable evidence, the response is the previous one | Flag off: the handler issues the previous statement and nothing else (by construction of `discoverRows`; tested on `runsAffinityStatement` and the statement text). Opt-out and no evidence: **proved** on the model (Apalache inductive invariant); **tested** on PostgreSQL by text equality with the two-argument function |
| P2 | Same set: every candidate of the previous function is returned exactly once before truncation and nothing else is | **Proved** on the model for arbitrary candidate sets. **Tested** on PostgreSQL: the candidate CTEs of both functions are compared as text, and the items of a full page are equal |
| P3 | Evidence is the actor's own interaction with a top-level, not hidden, readable post; other parties' behaviour never changes the order | **Proved** on the model; two negative controls (unfiltered evidence, third-party evidence). **Tested** on PostgreSQL with generated worlds and 19 mutations of the migration, each rejected by the test |
| P4 | At most `page_size div 2` candidates lead, each with units; every other candidate stands at most that many places below its previous position and never above it | **Proved** on the model (`Half` in 0..2); negative control without the cap on leaders. **Proved** with Z3 that `2K <= page` and at least one place keeps the previous order. **Tested** with QuickCheck on the reference for pages of 1 to 50 |
| P5 | The order is a strict total order for a given database state and date | **Proved** with Z3 over all integers (irreflexive, asymmetric, transitive, total for distinct ids); control without the id. **Tested**: QuickCheck |
| P6 | `units` is in 0..20, is 0 exactly without interaction, and is monotone in `r` and in `p` | **Proved** with Z3 over all non-negative integers; controls without the cap and with reply weight 1. **Tested**: QuickCheck |

What the levels do not cover:

- The model abstracts units to 0, 1 or 2, the previous order to an arbitrary key and the
  sort to "emit the candidate whose key precedes all waiting ones". It is proved for at
  most four candidates and three posts. That the SQL realises the model is **tested**,
  not proved.
- The pure Haskell function `TDF.Social.DiscoverAffinity.discoverOrder` is the
  reference of the order. It and the test oracle reproduce the same vectors; the SQL
  is compared with the oracle on PostgreSQL. The reference is not on the request path.
- The PostgreSQL test uses the real social migrations and this migration; the other
  tables are reduced to the columns read, not the migrated production schema.
- No HTTP-level test serves a personalised page.

## Privacy

The comment in the function says the pool rotates "without using popularity, private
connections or purchases". That still holds for the pool. This rule adds the actor's
own reactions and replies as a signal for the order of the actor's own page only.
Nothing about them is shown to the candidate or to anyone else, and the switch that
already governs genre matching turns it off. The switch is now labelled "Personalizar
con mis intereses y mi actividad".

A blocked, muted, dismissed, followed, closed, non-discoverable, organisation or
credential-less party cannot appear, because the candidate set is unchanged. A hidden
post, or a post in a club the actor may not read, is never evidence.

## Concurrency and failure

One STABLE function reads one snapshot; there are no writes. A reaction or block
committed after the snapshot affects the next request. Where the three-argument
function is not installed the handler runs the previous statement.

## Schema

`tdf-hq/sql/2026-11-27_social_discover_affinity.sql` adds the three-argument function
and two indexes for the legacy evidence reads
(`fan_club_post_reaction(reactor_party_id, created_at)` and replies by author). The
two-argument function is not touched. No new table. The rollback drops the function
and the indexes.

## Rollout

1. Apply the migration (through `scripts/production-migrations.json`).
2. Set `SOCIAL_DISCOVER_AFFINITY_ENABLED=true` and restart.

To switch off: unset the flag and restart.

## Known limits

- Affinity only reorders the daily sample of 200. A person the actor reacted to who is
  not in today's sample does not appear (owner decision of 2026-11-27).
- Discover excludes people the actor already follows, so the rule promotes people the
  actor interacted with without following.
- The constants were approved without usage data.
