# Social refactor handoff — 2026-09-15

**Overall status: incomplete; inactive review foundation delivered. Do not activate.**
This packet contains implementation and executable verification, but it does not
satisfy the requested full-platform privacy cutover. Outstanding implementation is
listed explicitly below. No PR was merged by this task. All new social gates remain
off. An automatic provider reported a successful deployment despite the requested
pending-deployment boundary; see the exception below.

## Delivered review sequence

| Order | Actual PR | Dependency | Implemented or specified result |
|---|---|---|---|
| 1 | [tdf-app #355](https://github.com/diegueins680/tdf-app/pull/355) | main | Audit, research decisions, canonical graph/policy, executable models; independent reviewer strengthened ownership, reads, delivery and stale-command properties |
| 2 | [tdf-app #356](https://github.com/diegueins680/tdf-app/pull/356) | #355 | Additive PostgreSQL authority, independent consent, block/mute/dismiss, retry/version fences, disabled runtime, read models, generated SQL refinement and races |
| 3 | [tdf-app #360](https://github.com/diegueins680/tdf-app/pull/360) | #356 | Bounded 200-candidate Discover sample and batched policy evaluation |
| 4 | [tdf-app #365](https://github.com/diegueins680/tdf-app/pull/365) | #360 | Explicit reaction retry model and runner; final diff preserves upstream reaction code |
| 5 | [tdf-app #366](https://github.com/diegueins680/tdf-app/pull/366) | #365 | Membership-first Following queries, supporting indexes, statement snapshots and measured query budget |
| 6 | [TDF-mobile #80](https://github.com/diegueins680/TDF-mobile/pull/80) | current mobile gitlink `790e03e1` | Generated additive contract; commit `c1832c5eb150299c12b36ba156ff38c981a6b046`; no mobile UI/runtime change |
| 7 | [tdf-app #367](https://github.com/diegueins680/tdf-app/pull/367) | #366 and mobile #80 | Authenticated gated API, Following/Discover/Connections web preview, local vCard QR, account cache isolation, real HTTP and browser fixtures, synchronized contracts and reconciliation |
| 8 | [tdf-app #377](https://github.com/diegueins680/tdf-app/pull/377) | #367 | Complete schema fixture + 102 registered migrations, publication/legacy source writes, preserved-data pause, final verification handoff |

PR #355 was made ready by concurrent work; #356/#360/#365/#366/#367 and mobile #80
and #377 are drafts at the recorded inspection. The code-bearing stack remains dependent and
unmerged. Current head/CI snapshots are recorded separately so historical results
cannot be mistaken for checks of a rewritten commit.

Original audit baseline: `17a33eca11d585d84435af85340beece9b51d14e`.
The independently reviewed audit branch incorporated main
`73edd77a36c8dcc73e5217303c62376ae684853b`; this task rebased its own dependent commits
onto reviewed audit `6d5c25c20927f45c7ce80e90d0d5d58b33b206a6`, preserving concurrent
formal work and upstream onboarding/reaction repairs. No unrelated branch was
merged as an implementation shortcut. Original dirty app/mobile checkouts remain
preserved. The refreshed mobile contract is byte-identical to the generated web
combined contract.

## Evidence actually executed

- **Executable models:** pinned TLC distribution 1.7.2 (reports TLC 2.17), SHA256
  `fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22`, Java 17.0.12.
  Four positive configurations and all twelve expected negative controls passed.
  Relationships: 101,855 distinct states; Feed: 1,674; RequestReplay: 272; Reaction: 15.
  Logs: `evidence/independent-final-models/`. Commands/configurations and bounded
  assumptions are in [verification](verification.md).
  The current reviewed model independently records delivery/read authority; modeled
  worker progress is not a claim about the existing production notification worker.
- **Model-to-SQL counterexample:** old disconnect erased peer consent. The generated
  case failed at state 23, then passed after the SQL repair. The final reviewed
  ConsentTraces run explored **32 states and produced 30 passing transition cases**.
  Logs: `evidence/reviewed-sql-before.txt`, `evidence/consent-independent-review.txt`,
  `evidence/sql-independent-review.txt`.
- **Real PostgreSQL:** private native PostgreSQL 16.10 fixtures passed additive
  reapplication, disabled defaults, retries/payload conflicts, ordered block/accept
  race, rate limits, closure, feed eligibility/order and pause preserving new writes.
  Reconciliation ran with zero fixture violations. Those initial tests use a minimal schema. The subsequent
  [complete schema fixture](schema-compatibility.md) restored the schema-only
  baseline, applied 102 registered migrations and passed additive reapply/backfill/
  preserved-write pause on PostgreSQL 16.10. Live data and old/new server coexistence
  remain unqualified. Earlier Docker fixtures passed;
  a later shared Docker API 500 blocked that path, so no Docker restart was attempted.
- **Real HTTP:** **8 examples, 0 failures**, actual Servant bearer authentication and
  private PostgreSQL. Includes gate states, injected actor rejection, consent
  ownership, block denials, malformed cursor/limit, membership revocation and
  inactive/organization identities. `evidence/http-refreshed-final.txt`. GHC's
  bytecode interpreter failed; Stack GHC object compilation succeeded.
- **Web:** **15 focused tests passed**, full TypeScript/Vite build and bundle budget
  passed. Initial JS 357,190 bytes gzip / five preloads; existing large-chunk warning
  remains. `evidence/ui-refreshed-final.txt`, `evidence/ui-build-refreshed.txt`.
  Source bytes are unchanged by the final model-only dependency refresh.
- **Browser:** local component with synthetic API/session fixtures passed default
  Following, keyboard tabs, explicit consent, 390px overflow and selected axe
  WCAG 2/2.1 AA checks (zero reported violations). Desktop/mobile screenshots were
  captured and visually inspected in `evidence/browser-refreshed/`. This is not
  authenticated full-app or native mobile E2E.
- **Refreshed backend tests:** the newly linked Stack/GHC 9.10.3 test binary ran
  **2,542 examples, zero failures**, exit 0. `evidence/backend-refreshed-result.txt`
  records the command, source/binary/log hashes and final output. The full Stack
  build wrapper is tracked separately while the application executable compiles.
- **Historical backend:** the earlier Stack-built binary ran **2,540 examples, zero
  failures**. Its Stack wrapper later failed copying an unbuilt executable. Earlier
  CI independently passed backend tests before failing a merch-migration prerequisite.
  These outcomes are historical, not proof of the refreshed final backend tree.
- **Synthetic performance:** 100k edges, 10k opted-in profiles, high-degree actor,
  50k posts and sparse membership. The initial feed exceeded 30s; final warm p95
  Following 102.321ms (sparse) / 185.770ms (empty), Discover 194.169ms (hub) /
  128.345ms (sparse), under the declared 200ms warm threshold. Cold empty EXPLAIN
  was 241.535ms. See [performance](performance.md); no production-scale or outcome
  uplift claim, and native correctness timing is not compared to Docker benchmarks.

## Acceptance register

Statuses apply to the stated scope; “failed” includes required work still incomplete.
A passing bounded model is evidence about that model and bounds, not the whole app.

| Criterion | Status | Evidence, limits, and remaining action |
|---|---|---|
| Inspect capabilities, instructions, dirty checkout and open work | satisfied | [Audit](audit.md); isolated worktrees, real baseline/tool failures recorded |
| Preserve unrelated work and existing domain authorities | satisfied | Existing dirty checkouts preserved; payments/booking/media/reputation authorities reused |
| Evidence-backed inventory of existing social domains/integrations | satisfied | Source-symbol gap matrix, reuse/repair/add/defer classifications and acceptance gates; not an exhaustive route/field coverage proof |
| Current primary-source research and decision matrix | satisfied | [Research](research.md), dated URLs, workload assumptions and rejected scale-inappropriate infrastructure |
| Canonical graph independent of storage | satisfied | [Policy](policy.md) and [database](database.md); directed ownership, independent consent, scoped domain membership, tombstones |
| Keep stack; justify infrastructure by measurement | satisfied | PostgreSQL joins/indexes; no graph engine, broker or new remote service provisioned |
| Default chronological Following and separate explainable Discover | satisfied for inactive web/API preview | Immutable publication order, reauthorized cursor, declared-interest/public-profile explanations and opt-out; sparse recall limitation documented |
| Full feed traversal and interaction product coverage | failed | Relationship list stops at 50; newly eligible old content needs refresh; private follow requests, reshares and complete recommendation-repeat evaluation absent |
| Accessible responsive focused flows and safe selection | satisfied for preview | Existing profile/interest flows, named candidates, keyboard/ARIA and synthetic browser evidence; full-app/mobile journeys not qualified |
| Shared server privacy/block policy across all existing surfaces | failed | DM read/write/API integration is delivered in #386/#390 and web isolation in #391; notifications, boosted feed, selectors, mentions, shared spaces and media still need integration; inactive code does not repair production behavior |
| Principal/entity/token and organization authority | failed | [Session boundary](session-boundary.md) retains token identity and verifies revocation races for the account-only API; delegated entities, credential provenance and global-role revocation remain incomplete |
| Account suspension/deletion/export and stale external work | failed | Social closure tombstone is implemented; full account lifecycle, retention, export and queued worker refinement are incomplete |
| Abuse/reporting/appeals and proportionate controls | failed | Mutation rate limit implemented; legacy reports reused in design, but general harassment report integration, mention controls and auditable moderation are incomplete |
| Executable permissions/privacy/relationships/concurrency models | satisfied within documented model bounds | TLA+/TLC safety/liveness, explicit negative controls, independent observed read/delivery evidence and command revisions |
| Requirement → model → code → generated test traceability | satisfied for consent/read-model subset | 30 generated SQL cases plus race/HTTP fixtures; full legacy/worker refinement remains failed |
| Tests of both flag states, real persistence/HTTP and denied outcomes | satisfied for preview | Local gates/race/retry/denial fixtures; no fabricated model/runtime results |
| Full application/backend/mobile/E2E and green current CI | blocked pending current CI; mobile/full-app coverage failed | Final CI must be read at exact refreshed heads. Legacy/mobile runtime tests and complete backend qualification cannot be inferred from focused fixtures |
| Performance thresholds and reproducible before/after | satisfied for documented synthetic read workload | Warm threshold met; mutation p95, multiple high-degree clubs, background lag, production histogram/storage/cost remain unqualified |
| Useful-outcome metrics and post-release plan | failed for instrumentation; satisfied for plan | [Operations](operations.md); aggregate reconciliation/connected-pair stock is only a proxy. Consent-aware lead/conversation/booking/sale attribution is not instrumented |
| Additive migration, idempotent publication backfill and preserved-write pause | satisfied on minimal non-production fixture | No manufactured legacy consent; pause retains blocks/preferences/positions. No destructive down migration |
| Complete-schema additive migration and preserved-write pause | satisfied on repository fixture | [Schema compatibility](schema-compatibility.md): complete schema-only baseline + 102 registered migrations; no live-data or lock-duration claim |
| Old/new server coexistence and rollback after activation | failed | Every legacy deny guard remains required; rolling back to code that ignores new blocks is unsafe |
| Small dependent review PRs, flags inactive, no merge | satisfied | Linked stack; dependencies, tests, limitations, flags and rollback documented; no merge performed |
| Leave all deployment pending | failed | Vercel and later Cloudflare checks reported success; Vercel inspection returned HTTP 403 and Cloudflare target remains uninspected |
| New graph engine/ML/broker/contact import/destructive cleanup | intentionally deferred | No measured requirement; no private relationship inference or unsupported infrastructure |
| Production activation and product experiment | intentionally deferred | User explicitly reserved these actions; no new production social flag activated |
| Overall requested completion | failed / incomplete | The required legacy, lifecycle, moderation, mobile, migration and instrumentation work remains |

## Remaining implementation order

1. Extend the implemented [account session boundary](session-boundary.md) to explicit
   credential provenance and delegated entity authority. Token revocation is modeled
   and tested; add cross-organization role-revocation cases before enabling delegates.
2. Build an endpoint/field coverage map, then integrate shared authoritative denials
   into legacy social, fan-club, DM, notifications, discovery/search and media reads.
   Fence DM insert and notification delivery with revocation; remove manufactured
   mutual consent through explicit compatibility adapters. Preserve old contracts.
3. Connect existing account lifecycle/reporting/appeal authorities, mentions and
   shared-space rules; add terminal stale-event handling and generated worker tests.
4. Finish relationship pagination, private follow decisions and mobile flows, then
   run real authenticated web/mobile journeys and all meaningful legacy tests.
5. Extend the passing complete-schema fixture to old/new application coexistence
   and approved representative data; measure index-lock duration and multi-club/
   high-degree query growth. Do not activate while any legacy bypass exists.
6. Add consent-aware useful-outcome and guardrail instrumentation using existing
   conversion identifiers, run current CI, and prepare a separately approved rollout.

## Deployment exception and owner action

An automatic audit-PR integration reported Vercel SUCCESS at
[this provider deployment](https://vercel.com/diego-saas-projects/tdf-app-tdf-hq-ui/8Pin2SvYAQ6UHGkpfZG2XbVPZSLP).
No deployment command was issued here. An authenticated API lookup of that exact
deployment returned **403**, so its target and removal could not be verified.
The provider owner must inspect the target and remove any unintended review preview.
Do not blindly delete a production target. Branch-scoped Vercel suppression and
Cloudflare-specific skip prefixes were added to this stack without skipping CI;
subsequent independently authored audit commits can still trigger provider checks.
This exception prevents a claim that all deployments remained pending.


## Published source references and CI snapshot

Recorded application stack heads on 2026-09-15:

| PR | Published commit | State at inspection |
|---|---|---|
| #355 | `6d5c25c20927f45c7ce80e90d0d5d58b33b206a6` | open, ready (concurrent audit work) |
| #356 | `1078a07c5d6a7c5a54231e1640ea36d03f30b977` | open draft |
| #360 | `fa6b99e7a4e718f7e6db7226ed01301ff762e4f9` | open draft |
| #365 | `4962f005bc2413e60b0762b9b43572c80b9fd0cd` | open draft |
| #366 | `aa56289c3b01d0d74d45c5e590882eca98fd5b2c` | open draft |
| #367 | `9a68db7448909741b6084bfa41bfcfc55fba2597` | open draft |
| mobile #80 | `c1832c5eb150299c12b36ba156ff38c981a6b046` | open draft |
| #377 | `b689f88e2967d09af990a678749b7c42627f1ba1` (schema/test implementation) | open draft; later evidence-only commits may follow |

At #367's exact head, [social CI](https://github.com/diegueins680/tdf-app/actions/runs/34988405056)
passed both models/PostgreSQL and client/browser jobs. The [complete CI run](https://github.com/diegueins680/tdf-app/actions/runs/34988405048)
passed repo, UI, mobile, persona, API tests/contracts and production-migration checks
at inspection; backend was still running and migration-tests was skipped by scope.
The saved `evidence/current-pr-*.json` snapshots include every result, including
failed aggregate checks on canceled duplicate runs for some earlier PRs; those must
not be described as all-green rollups. #377 adds its own schema CI run; the parent
results do not establish that new job succeeded.


At schema implementation `b689f88e2`, [schema/social CI](https://github.com/diegueins680/tdf-app/actions/runs/34991068129)
completed successfully, including the actual **Complete schema compatibility and
preserved-write pause** step on PostgreSQL 17, all models/PostgreSQL fixtures and
the client/browser job. Exact job/step metadata is in `evidence/ci-schema-b689f88.json`.
This independently verifies the Docker path; the native PostgreSQL 16.10 result
remains separately recorded.

A later Cloudflare Pages check on the independently updated audit branch also
reported [SUCCESS](https://dash.cloudflare.com/?to=/c07256e78d05ad9a508d0aee82ac577a/pages/view/tdf-app/77e2d52f-3869-47fb-bdd5-893667baada2).
Its target is uninspected; owner review is needed alongside the Vercel exception.
`evidence/current-pr-355.json` records this distinct result. Do not misattribute the
Vercel HTTP 403 to Cloudflare or describe provider deployments as all pending.

## Continuation evidence — session enforcement

The parent full Stack build completed successfully after the earlier snapshot:
2,542 examples, zero failures, application executable built, `Completed 2 action(s)`.
The parent [full CI run](https://github.com/diegueins680/tdf-app/actions/runs/34988405048)
also completed successfully, including backend-quality and its runtime/migration
checks. Those are results for the recorded parent source, not the new session patch.
The [session-boundary packet](session-boundary.md) adds current-token validation,
executable safety/progress models, a reproduced handler regression and 82 passing
PostgreSQL/HTTP cases. Overall full-platform delivery remains incomplete.

The account session patch is now [draft PR #382](https://github.com/diegueins680/tdf-app/pull/382),
dependent on #377, source `a68f235ca8a7d1987fd94defdba078228f77cd1c`. Its full local Stack build completed:
2,542 examples, zero failures and application executable built. Focused fixture
results and limits are in [session boundary](session-boundary.md). It remains
unmerged and inactive; hosted checks of this patch are separate from parent CI.

### Legacy DM write continuation

[DM write boundary](dm-write-boundary.md) implements the next compatibility stage:
existing message INSERT/UPDATE paths enforce canonical policy under ordered locks,
and pausing cannot erase activation/pair/closure restrictions. It has generated model
checks, old-INSERT counterexample evidence, real race fixtures and complete-schema
verification. Legacy thread/history reads and friendly HTTP denial mapping remain
**failed/incomplete** acceptance items; full old/new application privacy coexistence
and production activation remain blocked. No new message store or consent backfill.

### Published continuation references

- [Session PR #382](https://github.com/diegueins680/tdf-app/pull/382), dependent on #377:
  implementation `a68f235ca`, completed-build evidence `da541b58a`, catalog-policy
  classification `80cd4a1a4b928d6cb4252bfabc30a404be7b937b`. The unchanged catalog
  gate is now green. [Full CI at da541b58a](https://github.com/diegueins680/tdf-app/actions/runs/35000821777)
  completed successfully, including the actual PostgreSQL 17 session HTTP/model-case
  step, all backend tests, merch runtime, full-schema migrations and booking races.
  [Social CI at 80cd4a1a4](https://github.com/diegueins680/tdf-app/actions/runs/35011441891)
  passed; its [full CI run](https://github.com/diegueins680/tdf-app/actions/runs/35011441934)
  also completed successfully at that exact head.
- [DM write PR #386](https://github.com/diegueins680/tdf-app/pull/386), dependent on #382:
  implementation `efc827f76`. Local model, migration/race, complete-schema and
  benchmark results are recorded in [DM write boundary](dm-write-boundary.md).
  [Hosted social CI](https://github.com/diegueins680/tdf-app/actions/runs/35011873153)
  completed successfully at that implementation SHA, including both new model
  configurations/negative controls, generated cases, real DM races and complete
  schema compatibility on PostgreSQL 17. Its [full CI run](https://github.com/diegueins680/tdf-app/actions/runs/35011873154)
  also completed successfully at that same implementation SHA, including backend
  runtime/migration/booking checks. Both PRs are drafts and unmerged.

The earlier Vercel/Cloudflare deployment exceptions remain unresolved. No deployment
command or new production flag activation was issued during this continuation.

## Legacy DM read/API follow-up — 2026-09-15

[Read/API boundary](dm-read-boundary.md) extends #386 to existing thread previews,
message history, thread opening and send error mapping. It preserves DTOs and adds
shared server policy rather than exposing a second client contract. The bounded
DmReads model passed 9,648 distinct states and three specific negative controls;
1,440 generated observations passed on private PostgreSQL, and a membership-only
unsafe control failed at case 36. Complete-schema apply/reapply and pause passed.
The actual bearer HTTP suite passed **93 examples, zero failures**. Full local
Stack build/tests passed **2,542 examples, zero failures**. The Docker PostgreSQL
17.10 complete-schema fixture passed after the temporary-server readiness repair.
Social CI passed at `44a1c7bededf0ba4b54068d482667ac24299f8e4`; full CI remains
separately tracked. Do not infer current checks from earlier PR results.

This reduces the legacy-DM blocker only. Old readers must be drained before any
activation; profile/search/media/notifications, organization authority, moderation,
message retry semantics, thread pagination, native/full-app journeys and product
instrumentation remain incomplete. The historical deployment exception above still
applies. No production flag or deployment is authorized by these tests.

| Follow-up PR | Dependency | Actual implemented verification |
|---|---|---|
| [#390](https://github.com/diegueins680/tdf-app/pull/390), draft | #386 | DM read/API model, 1,440 SQL observations, 93 HTTP examples, full local backend, schema/readiness repair; implementation `02755bc410eecb5c3e03938121f1197102c709e1`, readiness `44a1c7bededf0ba4b54068d482667ac24299f8e4` |
| [#391](https://github.com/diegueins680/tdf-app/pull/391), draft | #390 | Account-scoped web queries, drafts, read markers and selected conversation; denied-refetch display/badge withdrawal; 28 focused tests, lint and app typecheck passed. Current implementation/evidence head `8b7daf9c81da8e2c1d1fe8f785371fb33d174078`; hosted rerun separately tracked |

The extra web TypeScript invocation including all test files failed in unchanged
fixtures; the repository application-only typecheck passed. Logs retain that
distinction. The initial child model job failed on PostgreSQL container startup,
not a formal invariant; the retained failure and readiness repair are documented.
No missing high-degree DM benchmark, native journey, migration rollout lock duration
or outstanding overall criterion is marked passed by these focused results.

## Verified profile continuation — 2026-09-15

[Draft #397](https://github.com/diegueins680/tdf-app/pull/397), base #390, implements
both profile read boundaries. Implementation `7706258a9e8ecbe0b99b4fd23903df606848ad03`;
final model/evidence head `7c452efcbc7b45e797b7ebd590dfffccf0fc9751`. No merge or
new production activation. Local results: TLC 5,760 distinct states and three
intended counterexamples; 2,880 generated PostgreSQL outcomes; 104 HTTP examples;
2,542 Stack examples; complete-schema PG17 migration/reapply/pause; catalog audit;
all passed. Synthetic protected SQL p95 12.11–43.76ms passed the declared 50ms
threshold after the scalar version failed at 76.33ms. No production-scale claim.

[Social CI](https://github.com/diegueins680/tdf-app/actions/runs/35056386037) and
[full CI](https://github.com/diegueins680/tdf-app/actions/runs/35056386064) are running
at the final profile head; the [snapshot](evidence/profile-ci/initial-final-head-checks.json)
is not a completed result. #390 full CI35052835558 still reports its backend
build/test step in progress, so it too remains unqualified remotely. Do not cancel
these runs by pushing evidence changes to their implementation branches. Inspect
the final statuses/logs and append exact-SHA results in this verification branch.

Remaining implementation work is **incomplete**, not an unavailable-tool excuse:
`socialListFollowers`, `socialListFollowing`, `socialListFriends` still load legacy
edges/names without canonical exclusion; `socialListSuggestedFriends` traverses
unbounded second-degree edges and exposes counts before privacy filtering. Those
are the next bounded compatibility repair. Legacy add-friend/vCard writers can
manufacture mutual follows and need a consent-preserving compatibility design,
though canonical DM no longer accepts those edges as consent. Broader search,
notifications/media, entity delegation, moderation/lifecycle integration, native
journeys and outcome instrumentation remain unfinished. Rollout stays blocked.

The supplemental test-inclusive web TypeScript failure is confirmed pre-existing:
parent/candidate diagnostics match byte-for-byte (134 lines); evidence commit
`029dd5382` in #394. Application-only typecheck and focused client tests passed.
Prior automatic provider deployment exceptions remain unresolved; no deployment
command was issued in this continuation.

## Completed parent CI — 2026-09-16

- #390 [full CI35052835558](https://github.com/diegueins680/tdf-app/actions/runs/35052835558)
  completed **success** at `44a1c7bededf0ba4b54068d482667ac24299f8e4`, including
  backend build/tests and runtime/session, merch, schema and booking checks.
- #397 [full CI35056386064](https://github.com/diegueins680/tdf-app/actions/runs/35056386064)
  completed **success** at `7c452efcbc7b45e797b7ebd590dfffccf0fc9751` after one
  failed-job rerun. The initial Chromium desktop rejected-login test hit its
  30-second deadline waiting for a mocked error alert (45 passed, 10 skipped,
  one failed). UI/e2e/package/CI inputs are unchanged from the passing #390 parent.
  No assertions, timeout settings or code were changed to obtain the rerun result.
  This demonstrates an intermittent run outcome, not a diagnosed root cause.
  The [first failure](evidence/completed-parent-ci/profile-browser-first-failure.txt)
  and [final run](evidence/completed-parent-ci/profile-rerun-final.json) are retained.
- #397 [social CI35056386037](https://github.com/diegueins680/tdf-app/actions/runs/35056386037)
  completed both model/PostgreSQL and social-client jobs successfully at that head.
  Scoped-out UI/mobile jobs in full CI remain **skipped**, not passes.

These results supersede earlier pending snapshots, without qualifying unimplemented
platform scope or authorizing deployment. All dependent PRs remain unmerged.

## Published relationship-read delivery — 2026-09-16

[Draft #402](https://github.com/diegueins680/tdf-app/pull/402), dependent on #397,
head `8b2d1557a5e72e0346af1fd43dc7cf36033da067`, implements legacy followers,
following and friends GET authorization plus retirement of inferred suggestions
once canonical enforcement starts. [Review packet](https://github.com/diegueins680/tdf-app/blob/8b2d1557a5e72e0346af1fd43dc7cf36033da067/docs/social/relationship-read-boundary.md)
contains 24,192-state TLC results, three negative controls, 8,064 generated SQL
observations, two expected SQL failures, 116 passing real bearer HTTP examples,
2,542 passing Stack examples, complete-schema/pause tests, source fingerprints and
synthetic degree-100/10,004 performance. Catalog audit and SQL volatility/UTC checks
passed. Hosted CI for this new head is pending; parent success does not qualify it.

Next implementation boundary: `socialAddFriend` and `vcardExchange` manufacture
reciprocal legacy follows and return names without canonical policy; removal also
needs a defined compatibility effect on canonical consent. Model the writer and
retry/block/accept boundary before changing behavior. Preserve historical meaning
and do not backfill accepted consent from legacy reciprocal rows. Broader client
cutover, search/media/notification privacy, delegation and lifecycle/moderation
remain incomplete; unpaginated legacy arrays still require a scale migration.
No production flag activation, merge or deployment command occurred. The prior
provider-triggered deployment exception still requires target/removal verification.

### #402 initial hosted verification

At head `8b2d1557a5e72e0346af1fd43dc7cf36033da067`, both jobs in
[social CI35122347901](https://github.com/diegueins680/tdf-app/actions/runs/35122347901)
passed: models/PostgreSQL and social client. The catalog audit also passed.
[Full CI35122347920](https://github.com/diegueins680/tdf-app/actions/runs/35122347920)
remains in progress, including backend and persona browser jobs. See the
[actual status snapshot](evidence/relationship-ci/initial-checks.json). Its skipped
UI/mobile jobs are not passing tests. Await the final full-run result and inspect
any failed-job logs without weakening assertions; record completion here to avoid
cancelling implementation CI with documentation pushes.


### #402 final full CI and #409 explicit legacy writes — 2026-09-16

[Full CI35122347920](https://github.com/diegueins680/tdf-app/actions/runs/35122347920)
completed **success** at `8b2d1557a5e72e0346af1fd43dc7cf36033da067`, including backend
build/tests, social session/model HTTP fixtures, artist-merch runtime, complete-schema
automatic migrations and public-booking HTTP conflict checks. Browser, migration,
quality and contract jobs also passed. The UI/mobile/API-contract-test jobs were
skipped, not passed. [Final actual run JSON](evidence/relationship-ci/final-full-ci.json)
supersedes the earlier pending snapshots without changing implementation branches.

[Draft #409](https://github.com/diegueins680/tdf-app/pull/409) depends on #402.
Exact source: `7e601607f60b2987e502c4a14db821bbf37df296`.
[Review packet](https://github.com/diegueins680/tdf-app/blob/7e601607f60b2987e502c4a14db821bbf37df296/docs/social/legacy-write-boundary.md)
records the repaired friend POST/DELETE and vCard writes, a shared locked current-token
transaction, and retirement with non-cacheable 410 after canonical enforcement. It
preserves historical timestamps/NFC metadata and never invents canonical consent.
Local checks passed: TLC 4,320 distinct states and three specific counterexamples;
288 generated real HTTP observations; **417 HTTP examples**, **2,542 Stack examples**,
zero failures; PostgreSQL 17 complete-schema/reapply/pause and catalog audit. Guard-only
synthetic p95 was 8.06ms at degree 0 and 12.22ms at degree 10,004 against a predeclared
50ms threshold, excluding bearer/mutation/HTTP. Failed development attempts and
exact source/log hashes are preserved. Hosted checks are **running**, not yet passed:
[actual initial snapshot](evidence/legacy-write-ci/initial-checks.json).

**Next repair:** `Server.fanFollowArtist` auto-follows every fan-club member in both
directions and emits artist-follower notifications. This side effect remains outside
#409, creates unwanted historical graph edges and unbounded work, and blocks activation.
Preserve the artist FanFollow subscription, model membership/notification authority and
retire implicit member-to-member follows without converting old rows into consent.
Canonical DM policy already prevents these legacy edges from granting accepted-connection
rights. Old-client cutover, search/media/notifications, delegation, lifecycle/moderation,
legacy list pagination and native journeys remain incomplete. All PRs remain unmerged;
no production flag activation or deployment command occurred. Earlier automatic provider
deployment exceptions remain unresolved and still need owner target/removal verification.
