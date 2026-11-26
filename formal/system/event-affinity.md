# Event order by artist affinity (EVT-AFFINITY-ORDER-001)

Status: third of the three changes that reuse the artist similarity
(COMMUNITY-ARTIST-SIMILARITY-001, [artist-similarity.md](artist-similarity.md)). The
owner asked to continue with the recommended defaults on 2026-11-22; the rule and the
constants below are those defaults and were not approved item by item. Off by default
behind `EVENT_AFFINITY_ORDER_ENABLED`.

## Target

The task names `TDF.Services.EventDiscovery` and the social events handlers.
`EventDiscovery` imports external events and orders nothing a person sees; the order of
the events a person sees is decided by `listEvents` in
`TDF.Server.SocialEventsHandlers` (`GET /social-events/events`). The rule is specified
there.

## Rule

`GET /social-events/events?scope=for_you` is `scope=all` with one more step:

1. The page is chosen exactly as for `all`: same filters, same visibility rules, same
   date order, same `limit` and `offset`.
2. For the caller, each usable artist `a` gets an affinity:
   - `1 + min(w(caller, a), 8) / 8`, in `(1, 2]`, when the caller has a signal for `a`
     (follow, favourite, accepted RSVP or play, the weights of
     COMMUNITY-ARTIST-SIMILARITY-001);
   - otherwise the mean over the caller's usable seeds `s` of
     `min(w(caller, s), 8) / 8 * sim(s, a)`, capped at 1, from the stored similarity;
   - no affinity when that is 0.
3. The affinity of an event is the largest affinity among the artists of its lineup
   (`event_artist` through `social_artist_profile.party_id`), 0 without one.
4. The page is returned ordered by event affinity, largest first; events with equal
   affinity keep their date order. So events without affinity stay in date order at
   the end, and a caller without any affinity receives the page of `all` unchanged.

Usable artists are publicly listed (ART-PUBLIC-VISIBILITY-001), are not the caller's
own party, and are either followed by the caller or followable under
`social_v2_profile_eligible` where it is installed. Listing, follows, the policy, the
caller's signals and the lineups are read at request time; only the similarity is
stored.

`scope` values other than `all`, `subscribed` and `for_you` are refused with 400 as
before. With the flag off `for_you` is accepted and answers exactly as `all`.

## Properties

- **Page preserved.** The response is a permutation of the page of `all` for the same
  parameters: no event is added, none is lost, and event visibility is unchanged.
  Pagination therefore stays that of `all`; the permutation is inside one page.
- **No hidden evidence.** An artist that is not usable has no affinity and contributes
  to none, as of the request's own read of the listing.
- **Order total and deterministic.** The key (affinity descending, position in the
  date order ascending) is a strict total order on a page.
- **Bounds.** Affinities lie in `(0, 2]`; own-signal artists are above 1 and inferred
  ones at most 1, so an event with an artist the caller chose never falls behind one
  that is only similar.
- **Fallback and opt-out.** A caller whose `social_v2_preference.personalized` is
  false has no affinities and receives exactly the page of `all`; none of their
  favourites, RSVPs or plays is read. The same holds with the flag off, and without a
  usable stored generation only own signals count.

## Privacy

The response has the shape and the events of `all`; only their order inside the page
depends on the caller's own signals. No other person's identity or behaviour is read
at request time (the stored similarity holds aggregates over at least two fans).

## Failure and staleness

- No generation, or one older than 48 hours: the similarity is empty; own signals
  still order the page.
- A lineup entry whose profile has no canonical party id is not evidence.
- The page query and the affinity reads are separate statements; an event or artist
  that changes between them is seen as of the later read. The page set is fixed by
  the first statement.

## Evidence

| Property | Evidence | Level |
|---|---|---|
| No hidden evidence (critical) | `formal/fans/event_affinity_order.py`: Z3, hand translation of `artistAffinities` and `loadFanArtistAffinities` pinned by source hash, witnesses and guard-necessity controls | proved for the translation |
| | `formal/fans/EventAffinity.tla`: Apalache inductive invariant with artists hidden and shown, signals changing and the job running at any time, three artists and two events | proved for the model |
| | TLC on two artists and two events; negative control (listing taken from the stored relation's snapshot), also reproduced by Apalache | model-checked |
| | QuickCheck: removing every non-usable seed and pair changes nothing | tested |
| Order total and deterministic (critical) | Same Z3 script: the key is a strict total order on distinct positions over doubles that are numbers; affinities are numbers; allowing NaN breaks it | proved for the translation |
| | QuickCheck: sorted by key, stable, idempotent, independent of lineup order | tested |
| Page preserved | TLA+ model (set of returned events); QuickCheck: the result is a permutation, and the identity without affinity | proved for the model; tested |
| Bounds | Z3 for the three arithmetic steps; QuickCheck | proved for the translation; tested |
| Opt-out and fallback | QuickCheck (no seeds, no usable artists); PostgreSQL probe on the fixture fan | tested |
| Lineup statement | PostgreSQL probe through `loadEventLineupParties` | tested |

Not established: the handler wiring (compilation and review; no HTTP-level test of
`scope=for_you`); that `sortOn` is the key's sort (library); the SQL sets against a
fully migrated database (the probe uses the fixture's stand-ins); the web control
beyond typecheck, lint and its unit test.

## Open for the owner

1. The rule reorders inside one page only. A global "for you" order across pages
   would need its own pagination contract.
2. An artist the caller follows that later becomes unfollowable under the policy still
   counts (followed artists are kept); say if a block must remove it.
3. The inferred affinity is a mean over the caller's seeds, so many unrelated seeds
   dilute it; it only orders events among those without an own-signal artist.
