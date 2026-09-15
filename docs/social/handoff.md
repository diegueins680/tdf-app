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

PR #355 was made ready by concurrent work; #356/#360/#365/#366/#367 and mobile #80
are drafts at the recorded inspection. The code-bearing stack remains dependent and
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
  Reconciliation ran with zero fixture violations. This is a deliberately minimal
  schema, not a full production-schema rehearsal. Earlier Docker fixtures passed;
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
| Shared server privacy/block policy across all existing surfaces | failed | Legacy DM, notifications, boosted feed, selectors, mentions, shared spaces and media are not consistently integrated; inactive API does not repair their production behavior |
| Principal/entity/token and organization authority | failed | New API excludes organizations and derives actor from auth, but AuthedUser loses token/credential identity; session revocation races and delegated entity contexts require implementation |
| Account suspension/deletion/export and stale external work | failed | Social closure tombstone is implemented; full account lifecycle, retention, export and queued worker refinement are incomplete |
| Abuse/reporting/appeals and proportionate controls | failed | Mutation rate limit implemented; legacy reports reused in design, but general harassment report integration, mention controls and auditable moderation are incomplete |
| Executable permissions/privacy/relationships/concurrency models | satisfied within documented model bounds | TLA+/TLC safety/liveness, explicit negative controls, independent observed read/delivery evidence and command revisions |
| Requirement → model → code → generated test traceability | satisfied for consent/read-model subset | 30 generated SQL cases plus race/HTTP fixtures; full legacy/worker refinement remains failed |
| Tests of both flag states, real persistence/HTTP and denied outcomes | satisfied for preview | Local gates/race/retry/denial fixtures; no fabricated model/runtime results |
| Full application/backend/mobile/E2E and green current CI | blocked pending current CI; mobile/full-app coverage failed | Final CI must be read at exact refreshed heads. Legacy/mobile runtime tests and complete backend qualification cannot be inferred from focused fixtures |
| Performance thresholds and reproducible before/after | satisfied for documented synthetic read workload | Warm threshold met; mutation p95, multiple high-degree clubs, background lag, production histogram/storage/cost remain unqualified |
| Useful-outcome metrics and post-release plan | failed for instrumentation; satisfied for plan | [Operations](operations.md); aggregate reconciliation/connected-pair stock is only a proxy. Consent-aware lead/conversation/booking/sale attribution is not instrumented |
| Additive migration, idempotent publication backfill and preserved-write pause | satisfied on minimal non-production fixture | No manufactured legacy consent; pause retains blocks/preferences/positions. No destructive down migration |
| Complete-schema migration, old/new coexistence and rollback after activation | failed | Need full-schema fixture and every legacy deny guard. Rolling back to code that ignores new blocks is unsafe |
| Small dependent review PRs, flags inactive, no merge | satisfied | Linked stack; dependencies, tests, limitations, flags and rollback documented; no merge performed |
| Leave all deployment pending | failed | Automatic Vercel check reported success; provider target inspection/removal blocked by HTTP 403 |
| New graph engine/ML/broker/contact import/destructive cleanup | intentionally deferred | No measured requirement; no private relationship inference or unsupported infrastructure |
| Production activation and product experiment | intentionally deferred | User explicitly reserved these actions; no new production social flag activated |
| Overall requested completion | failed / incomplete | The required legacy, lifecycle, moderation, mobile, migration and instrumentation work remains |

## Remaining implementation order

1. Retain authenticated credential/token identity and acting entity separately.
   Model revocation and lock the same authority rows during protected writes.
   Add cross-organization/actor-switch/revoked-token races before enabling delegates.
2. Build an endpoint/field coverage map, then integrate shared authoritative denials
   into legacy social, fan-club, DM, notifications, discovery/search and media reads.
   Fence DM insert and notification delivery with revocation; remove manufactured
   mutual consent through explicit compatibility adapters. Preserve old contracts.
3. Connect existing account lifecycle/reporting/appeal authorities, mentions and
   shared-space rules; add terminal stale-event handling and generated worker tests.
4. Finish relationship pagination, private follow decisions and mobile flows, then
   run real authenticated web/mobile journeys and all meaningful legacy tests.
5. Rehearse additive migration/backfill/reconciliation/preserved-write rollback on
   a representative complete non-production schema; measure index-lock duration and
   multi-club/high-degree query growth. Do not activate while any legacy bypass exists.
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
