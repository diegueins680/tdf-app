# Friend suggestions by personalized PageRank (COMMUNITY-FRIEND-SUGGESTION-001)

`GET /social/suggestions` keeps its route, authentication and array shape. Behind
`FRIEND_SUGGESTIONS_PAGERANK_ENABLED`, off by default, its order comes from a
personalized PageRank over actor-confirmed `social_v2_pair` follows computed by a nightly run, instead of the
second-degree count. Directory ranking is not touched.

The owner approved the rule, the constants and the nine decisions of the proposal on
2026-11-21. Two points differ from the approved text; see "Departures from the
approved proposal".

## Rule

For a viewer `v`:

- **Eligible accounts `E`.** A party is in the graph only if it is a person
  (`NOT is_org`), has an active credential and a verified email (a sign-in provider
  bound the credential after verifying it, `auth_provider_identity`, or the party
  completed a password reset sent to it, `party_email_verification`), has not closed its social profile
  (`social_v2_preference.closed`), and was created at least `MIN_AGE = 7 days` before
  the run started (`party.created_at`).
- **Viewer graph `G_v`.** Nodes: `E` minus every party with a block towards or from `v`
  (`social_v2_pair.block_a OR block_b`). Edges: each true `follow_a` contributes A → B and each true `follow_b` contributes B → A, when both ends are
  both nodes, without self-links. An excluded party has no node, so it has no score and
  no edge through which score could pass.
- **Walk.** `p_0 = e_v` (all mass on the viewer). With damping `d = 0.85`:

  `p_{t+1}(j) = d * sum over i->j of p_t(i)/out(i) + [j = v] * ((1 - d) + d * D_t)`

  where `out(i)` is the out-degree of `i` in `G_v` and `D_t` is the mass on nodes with
  no outgoing edge. A walker at a dead end restarts at the viewer.
- **Iterations.** `T = 100`. The stored score is `p_T`, computed in `Double`.
- **Candidates.** Nodes other than `v` that `v` does not follow and that do not follow
  `v` (today's exclusion), with `p_T > 0`.
- **Order.** Score descending, then party id ascending. The run stores the first
  `K = 50` per viewer; the route returns at most 20, as before.

## Fallback and opt-out

`fallback(v)` is exactly what the route returned before this change: the second-degree
count ordered by count descending and party id ascending, limit 20, through the
`social_v2_legacy_suggestions` reader where social-v2 is installed (which returns `[]`
once social-v2 has been activated) and the Haskell handler otherwise.

The route returns `fallback(v)`, with the same JSON as before, when any of these holds:

1. the flag is off;
2. the viewer opted out of personalization (`social_v2_preference.personalized = false`);
3. the score tables, `auth_provider_identity`, `party_email_verification` or the
   social-v2 policy are not installed,
   or the database is not PostgreSQL;
4. no stored result exists for the viewer, or its run completed more than
   `MAX_AGE = 48 h` ago;
5. any party that carried score for the viewer in that run (its stored *support*, the
   nodes with `p_T > 0`, the viewer included) is no longer a live verified person with
   an open profile, or now has a block with the viewer;
6. the session is not current, or reading the stored result fails.

Invariant **OPT-OUT**: for every viewer with `personalized = false`, and for every viewer
while the flag is off, response = `fallback(v)`.

## Request-time boundaries

- **Follow policy.** Stored candidates pass `social_v2_profile_eligible(v, ids)` and the
  "not already connected" test again at request time, before the limit, so a block,
  closure, deactivation or new follow made after the run takes effect on the next
  request. Rule 5 covers the same events for parties that are evidence only.
- **Privacy.** A personalized entry carries the party id, `sfReason = "network"` and
  `sfMutualCount = 0`. No score and no count is returned: the walk uses follows that
  nobody consented to publish, so no number derived from them is disclosed. Scores are
  stored per viewer and read only for that viewer. No third party is named as the
  reason. A fallback entry has no `sfReason` field, so its JSON is unchanged.
- **Authorization.** Unchanged: bearer session, revalidated in the read transaction
  (`withCurrentSession ReadSession`).

## Nightly run

- Daily at 08:00 UTC (03:00 America/Guayaquil) under a PostgreSQL advisory lock, so one
  API replica runs it. Not started unless the flag is on when the API starts.
- The run reads accounts, opt-outs, blocks and follows in one `REPEATABLE READ`
  transaction, computes every viewer in memory, then records the run, deletes every
  earlier run (which cascades to its viewers and scores) and inserts the new results in
  one transaction. Readers see the previous run or the new one, never a mixture. A
  failed run leaves the previous run in place; rule 4 retires it after 48 h.
- Opted-out viewers are not computed, so the replacement removes their rows.
- Tables (`tdf-hq/sql/2026-11-21_social_friend_suggestions.sql`, registered in
  `scripts/production-migrations.json`):
  `social_suggestion_run`, `social_suggestion_viewer` (run and support per viewer) and
  `social_suggestion_score` (candidate, position, score). All three are derived data
  and are removed with the party they refer to.

## Convergence

Write `F` for one step. For two distributions `p`, `q` on the nodes of `G_v`,
`F(p) - F(q) = d * M (p - q)`, where column `i` of `M` spreads `1/out(i)` over the
accounts `i` follows, or is `e_v` when `i` follows nobody. Every column of `M` is
non-negative and sums to 1, so `norm1(M x) <= norm1(x)` and

`norm1(F(p) - F(q)) <= d * norm1(p - q)`.

`F` maps distributions to distributions, so it has a unique fixed point `p*` among them
(Banach), and `norm1(p_0 - p*) <= 2` because both are distributions. By induction

`norm1(p_T - p*) <= 2 * d^T = 2 * 0.85^100 < 1.75e-7 < 2e-7`.

## Properties and evidence

Three strengths of evidence are used and are not interchangeable: **proved** (Z3, exact
reals, bounded graphs, one step), **bounded model** (Alloy, graph structure only) and
**tested** (QuickCheck/Hspec on the Haskell functions, PostgreSQL on the SQL text).

| Property | Evidence | What it does not cover |
|---|---|---|
| Excluded parties never carry score: a non-node holds no score, and follow rows touching a non-node do not change a step | Proved by Z3 for every viewer, node set and follow relation up to 4 accounts (`scripts/verify-friend-suggestion-pagerank.py`), with a control that admits follows into excluded accounts. Bounded model in Alloy up to 5 accounts (`formal/event-operations/FriendSuggestionGraph.als`) for the graph structure, blocks included. Tested by QuickCheck on `TDF.Social.SuggestionRank` | Larger graphs; the Haskell and SQL text (the Z3 step is a hand translation of `walkStep`) |
| Excluded parties never appear: which accounts are eligible, and which stored candidates are served | Tested on PostgreSQL with the statements extracted from the Haskell source (`scripts/test-friend-suggestions-sql.py`): 13 accounts, 16 read scenarios and the password-reset write | Not proved. In that test `social_v2_profile_eligible` is a stand-in with the migrated signature. The same statements were also executed once, by hand, on a database built by the complete migration rehearsal (real function, real tables): they run and return no rows there, because that fixture has no eligible account. No scenario was checked against the real policy function |
| `p_t` is a probability distribution for every `t`, dead ends included | Proved by Z3 as a one-step inductive invariant up to 4 accounts, with a control that drops the dead-end return. Tested by QuickCheck: exact over `Rational` after every step; over `Double` within `1e-9` after 100 steps | The `Double` tolerance is tested, not proved. Induction over `t` is the hand argument above |
| Convergence: `norm1(p_T - p*) <= 2e-7` | Hand proof above. Proved by Z3: the one-step contraction (symbolic up to 3 accounts; for 4 accounts every one of the 4096 follow relations with the viewer fixed and every account a node), and `2 * 0.85^100 < 2e-7`; a control with factor 0.84 is refuted. Tested by QuickCheck over `Rational` | General size and the induction rest on the hand proof. `Double` rounding is outside the proof |
| Fake follows: accounts that are too new or unverified cannot change any viewer's graph, whatever they follow and whoever is made to follow them | Bounded model in Alloy up to 5 accounts; controls that remove the age filter or the email filter each produce a counterexample. Tested by QuickCheck | Bounded. Says nothing about accounts that pass both filters. "Same graph gives same order" is the function signature, not a model result |
| Flag off, or viewer opted out, gives exactly the fallback | Tested: Hspec on the adapter with the flag off; PostgreSQL scenario for the opt-out | Tested only. No HTTP-level test on a fully migrated database |

## Departures from the approved proposal

1. **No feature-registry entry.** The gate is the environment variable only, listed
   with the other backend boolean controls in `ops/hetzner/inspect-runtime.py`. A registry
   entry would change the generated Mobile registry, and the behaviour has no route or
   screen of its own.
2. **The response field is optional and absent on fallback entries**, so the fallback
   JSON is byte-for-byte what it was. `/social/suggestions` was never described in
   `tdf-hq/docs/openapi/api.yaml` and still is not, so the OpenAPI contract, the
   generated clients and the Mobile pin are unchanged.

## Verified email

`party_email_verification` holds one row per party that completed a password reset.
`TDF.ServerAuth.runPasswordResetConfirm` writes it through
`TDF.Auth.EmailVerification.recordVerifiedEmail`, in the reset's own transaction, after
the token is consumed. The write is skipped outside PostgreSQL, before the table exists
and when the role may not insert, so it cannot fail a password reset. It happens whether
or not the flag is on. Google and Apple sign-ins already leave an
`auth_provider_identity` row and need no write. Neither fact is withdrawn when the party
later changes its address.

## Open for the owner

- This reverses the retirement recorded in `docs/social/relationship-read-boundary.md`
  (2026-09-16) for viewers who are served a personalized result; that document still
  describes the fallback.
- Clients that render the network reason request `?networkReason=true`. All other clients receive the existing fallback, including older Mobile releases.
- Account age is `party.created_at`; credentials have no creation time. A CRM contact
  created long ago that registers today counts as old.
- Rule 5 is coarse: one account closing its profile sends every viewer it carried score
  for to the fallback until the next night.
- Opted-out accounts stay in other viewers' graphs; opting out governs what they
  receive.
- The filters stop only accounts that fail them. An account that waits seven days and
  signs in with Google, or resets its password, is a normal node; it gains score only if a node follows it.
- Each actor sets their own follow direction through the canonical social mutation. Legacy
  friend, vCard and fan-club rows have no actor provenance and contribute no edges.
- Whether social-v2 is activated in production, and the count of actor-confirmed follows, were
  not checked (the production read was denied). If activated, the fallback is `[]`.
  One run costs about `100 * follows` map updates per viewer, in memory.
- The run starts only if the flag is on when the API starts; turning the flag on needs
  a restart, and the first results appear after the next 08:00 UTC.
