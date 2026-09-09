# TDF onboarding cross-device reconciliation audit and implementation — 2026-09-09

## Outcome

This bounded onboarding-first batch resolves the highest-priority deferred continuity defect from `reports/onboarding-completion-recovery-audit-2026-09-08.md`: an authenticated Party can now recover onboarding completion on device B using only durable server evidence created by a real action on device A. The client does not submit a Party ID, action category, role, module, permission, or domain object identifier to reconciliation.

The backend now inspects the four supported evidence sources, selects the earliest qualifying action in the signup window, resolves exact timestamp ties with a documented stable order, and uses the existing atomic incomplete-to-complete transition. Existing `/session/onboarding/complete` callers remain compatible, but their supplied value is only a validated observation; server evidence is authoritative and takes precedence over an explicit exit. Web reconciles after authenticated session hydration or login. Mobile reconciles on authenticated hydration, reconnect, and foreground, coalesces overlapping same-session work, and suppresses late Party-A results after a Party-B switch.

Both clients emit completion analytics only from `newlyCompleted=true` and the canonical non-null value returned by the server. The paused `single-feature-onboarding-v1` experiment remains paused. No visual rebrand, price, policy, role, permission, payment, database schema, deployment, production-data, or customer-communication change is included.

## Capability and safety matrix

| Capability | Status | Evidence from an actual check | Consequence |
|---|---|---|---|
| Repository read/write | Available | Source, tests, generated clients, and this report were changed only in `/private/tmp/tdf-event-save-publish-20260907` | Implementation is isolated from the primary checkout |
| Dedicated feature branches | Available | Root and mobile are both on `feature/onboarding-cross-device-reconciliation-20260909` | Root and submodule changes can be committed separately and reviewed as a stack |
| Baseline and unrelated-work protection | Available | Clean baseline recorded before edits: root `4b90f4150b64f41a8f14421393fd42479f0d14e1`; mobile `c790ab586ac324c31bc43e8899de36e9f93803e6` | No unrelated work was absorbed into this batch |
| Branch integration state | Partial | After the implementation commits, root is 103 `origin/main` commits behind / 38 ahead; mobile is four behind / 18 ahead | Do not merge this stack without reconciling both histories, regenerating both clients, and rerunning the full matrix |
| Mobile submodule | Available | Real submodule workspace present at the mobile baseline; `REQUIRE_MOBILE_WORKSPACE=1 npm run generate:api` generated both clients | Mobile generation cannot silently pass by skipping a missing workspace |
| Required runtimes/package managers | Available | Repository doctor found `git`, `node`, `codex`, and `gh`; npm, TypeScript, Jest, ESLint, Stack/GHC, and Hspec commands executed | Local contract and regression checks are supported |
| Backend/database | Partial | Stack compiled against the local project and Hspec uses isolated SQLite fixtures; no PostgreSQL handler fixture was available | State/authorization invariants are locally testable; PostgreSQL query behavior and plans remain unverified |
| Browser and screenshot tooling | Available, not used | Earlier audit capability check found Playwright 1.59.1; this batch changes no rendered pixels | No screenshot was fabricated; visual before/after evidence is not material to this state/API change |
| Native device tooling | Partial, not exercised | Earlier capability check found Android/iOS tooling but no connected device and only shut-down simulators | Jest covers provider lifecycle; real device foreground, offline, and cross-device behavior remain unverified |
| Test runners | Available | Focused/full Jest, typecheck, lint, generated-contract, Stack/Hspec, and repository gates are configured | Automated verification can cover the selected batch |
| Network access | Partial | Cached/local package and Stack work proceeded; no production application request was made | Local work is independent; staging and production behavior are not claimed |
| Local/staging configuration | Partial | Local no-op analytics and synthetic SQLite fixtures are available; no authenticated staging fixture was supplied | No live cross-device walkthrough or external-service verification was possible |
| Synthetic test accounts | Unavailable | No synthetic staging credentials were present in the audited environment | Signup-on-A/sign-in-on-B runtime acceptance remains blocked |
| Analytics access | Unavailable | Mobile test output explicitly reports PostHog disabled because `EXPO_PUBLIC_POSTHOG_KEY` is unset | Event semantics are code/test verified; ingestion, p75 funnels, and uplift remain “not yet measured” |
| GitHub authentication | Unavailable in the current environment | `npm run ai:doctor` exited 0 but `gh auth status` reported the active credential invalid | Local commits remain possible; push and draft-PR creation must not be claimed unless a later authenticated check succeeds |
| Production access | Not used | No deploy, migration, real transaction, production write, or communication command was run | This report contains no production-validation claim |

`npm run ai:doctor` was inspected before execution and returned 14 OK, four warnings, and zero errors. The warnings were the two dated memory files absent from the isolated worktree, expected task changes, and invalid GitHub authentication. Root `AGENTS.md`, `SOUL.md`, `USER.md`, `MEMORY.md`, `AI_WORKFLOW.md`, `CONTRIBUTING.md`, and relevant package scripts were read. No `BOOTSTRAP.md` exists.

## Method and baseline

The batch combines product/UX journey analysis, information-architecture continuity, source and API-contract inspection, a cognitive walkthrough of device-A action/device-B login, authorization and account-isolation review, analytics-semantics review, generated-client comparison, rendered provider tests, and isolated database tests. The coordinated engineering review covered backend, web, and native mobile behavior without inventing reviewers, participants, approvals, field metrics, or conversion improvement.

The verified baseline already provided:

- durable account-bound onboarding progress and a 24-hour signup window;
- Party-bound evidence for artist follows, access requests, event saves, and canonical moment-reaction additions;
- same-device mobile retry metadata scoped to the authenticated Party;
- late acceptance when evidence occurred in-window, even if the handshake arrived after expiry;
- safe auth-session generation checks and a paused onboarding experiment.

The remaining reproduced gap was architectural: device B had no local recovery label and `/complete` inspected only the client-selected category. A later category could therefore win attribution even when an earlier supported action existed.

## Coverage matrix

| Journey/surface | Role | Device/state | Inspection method | Verification status |
|---|---|---|---|---|
| Session onboarding reconciliation API | Authenticated external Party | No progress, missing signup, eligible/expired, in-window/post-window evidence, repeat, unauthenticated | Servant source, OpenAPI, SQLite Hspec | Implemented; local automated verification |
| First useful action selection | New authenticated Party | Artist follow, access request, event save, moment reaction; chronological ordering and exact-time tie | Source and SQLite fixture spanning all four evidence sources | Implemented; local automated verification |
| Explicit optional exit | Eligible new Party | Exit with no evidence; exit when durable evidence already exists; expired exit | Source and SQLite Hspec | Implemented; local automated verification; true concurrent action/exit interleaving not exercised |
| Web session bootstrap/login | Authenticated external Party | Cookie bootstrap, bearer-token login, newly completed, already completed, Party A→B delayed response | Rendered React tests, API tests, TypeScript | Implemented; mocked automated verification |
| Mobile first-run lifecycle | Authenticated new/returning Party | Bootstrap, offline fail-closed, reconnect, foreground, overlapping triggers, Party A→B switch | Rendered React Native Jest tests, TypeScript | Implemented; mocked automated verification |
| Mobile direct first actions | Customer/fan or capability-requesting Party | Follow, save, reaction, access request; server returns an earlier canonical category | Source, shared-helper/gate/social tests, full mobile Jest | Implemented; automated verification; no device runtime |
| Existing onboarding experiment | Eligible treatment/control Party | Reaction conversion and explicit exit | Source and gate tests | Analytics semantics repaired; experiment remains paused and was not runtime exposed |
| Public entry, profiles, events, campaigns, auth, and recovery | External visitor/new user | Desktop/mobile responsive states | Revalidated in earlier 2026-09-05 through 2026-09-08 audit reports; not rerun in this state-only batch | Prior evidence retained; not newly runtime-verified |
| Services, bookings, commerce, education | Customer/provider/student | Revenue and task flows | Existing route/source inventory only in this continuation | Uninspected at runtime in this batch; unchanged |
| Internal management and operations | Staff roles/modules found in code | Dense desktop workflows | Existing route/source inventory only in this continuation | Uninspected at runtime in this batch; unchanged |
| PostgreSQL/staging reconciliation | Synthetic authenticated Party | Device-A action then device-B login; query plan/cardinality | No suitable fixture or credentials | Blocked/unverified |
| Real iOS/Android cross-device behavior | Same synthetic Party on two devices | Relaunch, foreground, offline/reconnect, account switch, enlarged text | No connected device pair or account | Blocked/unverified |
| Field analytics/performance | Consented real cohorts | p75 onboarding funnel, LCP/INP/CLS, request latency | No dashboard/RUM access | Not yet measured |

This matrix is runtime coverage, not merely a route inventory. Areas not exercised here are explicitly labeled rather than implied complete.

## Historical finding revalidation

| Historical finding | Current classification | Evidence |
|---|---|---|
| `reports/onboarding-completion-recovery-audit-2026-09-08.md` OCR-05: pending recovery does not move to another device | **Resolved in code and local automated tests** | Dedicated authenticated reconcile POST; web/mobile hydration; Party-switch tests |
| Same report OCR-04: analytics delivery is at-most-once | **Still present/deferred** | `newlyCompleted` prevents duplicates, but a committed response lost before client receipt can undercount |
| `reports/onboarding-ux-audit-2026-08-20.md`: device-local flags can misclassify returning users | **Superseded for eligibility and completion authority** | Clients now reconcile the Party-bound durable record; local storage is only same-device retry metadata |
| `reports/onboarding-evidence-integrity-audit-2026-09-08.md`: Party/session evidence boundaries | **Still resolved and extended** | Reconciliation derives Party solely from auth and accepts no action/Party body; clients discard stale A→B responses |
| Earlier client-selected first-value semantics | **Regressed risk resolved in this batch** | Both `/complete` and `/reconcile` persist the earliest category across all sources; analytics uses the returned category |
| Existing experiment measurement limitations | **Still present/deferred** | Experiment remains paused; assignment/exposure validity was not changed or claimed |
| `reports/ux-ui-audit-2026-08-05.md` and `UX_AUDIT_REPORT.html` findings outside onboarding continuity | **Not re-inspected in this batch** | Preserved historical reports and the coverage matrix above prevent stale findings from being presented as current defects |

Historical reports were not rewritten.

## Findings and implementation status

### XDR-01 — Device B could not finish onboarding from device A's durable action

- Journey/role: new authenticated Party; any supported first useful action followed by sign-in on another device.
- Baseline reproduction: persist action evidence on A, lose `/complete`, then hydrate B without A's Party-scoped AsyncStorage label. Expected: B derives the real action from account state. Actual: B could only read eligibility and could not know which category to assert.
- Evidence: backend/mobile/web source and the earlier OCR-05 audit; high severity, high confidence. Observed continuity impact; frequency and conversion impact are not measured.
- Cause: reconciliation required a client-selected category, while mobile recovery metadata was deliberately device-local.
- Remedy/effort/dependencies: medium API/provider change using existing evidence tables and progress response; no schema or new dependency.
- Acceptance: authenticated session only; no Party/action body; same-Party in-window evidence; missing/other-Party/post-window evidence fails closed; one idempotent transition; B exits completed onboarding.
- Status: implemented and locally automated-verified; staging/two-device runtime blocked.

### XDR-02 — First-value attribution depended on the client's last request

- Journey/role: a new Party completing more than one meaningful action before a handshake.
- Baseline reproduction: create an earlier event save and later artist follow, then submit `artist_followed`. Expected: first means chronologically earliest supported action. Actual: the requested supported category was persisted when it had evidence.
- Evidence: handler control flow and new cross-category SQLite test; medium user-state harm, high measurement harm, high confidence. No field impact estimate is claimed.
- Cause: evidence lookup was scoped to the request enum and never compared categories.
- Remedy/effort/dependencies: scan all current authoritative sources and choose minimum `(evidence timestamp, stable category priority)`; use the same inference for legacy `/complete`.
- Acceptance: chronology wins; ties resolve `artist_followed`, `access_requested`, `event_saved`, `moment_reaction`; intent never affects authority; returned canonical value drives analytics.
- Status: implemented and locally automated-verified.

### XDR-03 — Mobile could delete retry state while still incomplete

- Journey/role: authenticated Party whose eligibility is false because the window expired or signup metadata is unavailable.
- Baseline reproduction: completion response `{eligible:false, completedAt:null, newlyCompleted:false}`. Expected: do not treat eligibility as proof of durable completion. Actual: the Party marker was removed.
- Evidence: helper source and Jest regression; medium continuity harm, high confidence.
- Cause: `eligible=false` conflated completed, expired, and missing-signup states.
- Remedy/effort: clear only when authoritative `completedAt` exists; small, no dependency.
- Acceptance: completed response clears; ineligible-but-incomplete response retains the Party-scoped marker; account replacement still cannot consume it.
- Status: implemented and automated-verified.

### XDR-04 — Overlapping mobile reconciliation could consume the only completion receipt

- Journey/role: current authenticated Party acting while hydration/foreground reconciliation is in flight.
- Baseline reproduction: let reconciliation win `newlyCompleted=true`, let a direct call return false, then allow the direct path to invalidate the older provider generation. Expected: one response owns state and analytics. Actual risk: the winning response could be suppressed and measurement lost.
- Evidence: provider control-flow analysis and deterministic rendered tests; low user-state harm, medium measurement harm, high confidence.
- Cause: foreground trigger coalescing did not include direct provider completion.
- Remedy/effort: serialize direct same-session completion behind an in-flight reconciliation; the provider owns reconciliation analytics and returns a non-winning result to prevent caller duplication.
- Acceptance: one request observes the winning result; two analytics events at most once; no duplicate direct claim; Party A response never applies to B.
- Status: implemented and automated-verified.

### XDR-05 — Event-save evidence lookup is correctness-first but unbounded

- Journey/role: incomplete Party with many lifetime save audit records; every authenticated reconcile trigger.
- Reproduction: source inspection shows all matching Party/action/event timestamps are decoded and filtered in Haskell to preserve mixed SQLite/PostgreSQL timestamp correctness.
- Expected: a bounded indexed query with the same cross-database time semantics. Actual: work grows with matching audit history. Observed implementation characteristic; real cardinality and latency are not measured.
- Severity/confidence: medium performance risk / high confidence.
- Proposed remedy/effort/dependencies: normalize fixture/storage time encoding or add backend-specific bounded SQL plus PostgreSQL and SQLite contract tests; medium effort and real query-plan evidence.
- Acceptance: earliest in-window result is identical, query work is bounded by indexed time predicates, and both database engines pass.
- Status: deferred; new-user cardinality is expected to be small but was not measured.

### XDR-06 — Lossless analytics and database time constraints remain incomplete

- Journey/role: completion response lost after commit; defense in depth for corrupted legacy rows.
- Actual: clients correctly avoid duplicates but may undercount a lost winning response; database constraints do not independently enforce the full 24-hour/evidence-before-completion relationship.
- Evidence: API/handler/schema inspection; low user harm, medium measurement/data-integrity risk, high confidence.
- Proposed remedy/dependencies: privacy-reviewed durable receipt and deduplicated delivery; separately audit legacy rows before validated database constraints.
- Acceptance: one receipt can be retried without sensitive data or cross-session attribution; constraints validate without deleting or rewriting history.
- Status: deferred as a larger compatible-contract/migration batch.

## Implementation rationale and invariants

- `POST /session/onboarding/reconcile` is a mutation because it can complete progress; `GET /session/onboarding` remains read-only.
- Authentication is the only Party selector. No request body means intent cannot be confused with a permission and clients cannot probe another Party.
- Signup time defines the inclusive action-evidence window. Handshake time may be later; `firstValueCompletedAt` remains evidence time and `completedAt` remains transition time.
- Both completion routes use the same evidence inference. A supplied enum is validated for compatibility but does not override earlier evidence.
- Exact-time category order is contract stability, not a value or permission ranking.
- The existing compare-and-set update makes only one incomplete-to-complete request return `newlyCompleted=true`.
- Completed rows are not rewritten. Historical follow/unfollow, save/unsave, reaction add/remove, or request cancellation evidence retains the existing “first useful action occurred” meaning.
- Mobile recovery state remains Party-scoped and contains only an allowlisted category. It never stores tokens, passwords, email, phone, comments, captions, object IDs, or permission-like values.
- Session generation/auth assertions surround network, cleanup, state, and analytics boundaries. Same-session triggers share a request; different Parties never share one.
- Server-returned canonical values, not button labels, feed both `first_value_completed` and `onboarding_completed`.
- No new dependency, timer, polling supervisor, feature activation, or parallel component library was introduced.

## Accessibility, localization, privacy, and performance

No rendered control, layout, copy, media, focus behavior, or color changed, so this batch makes no new WCAG 2.2 AA or visual-conformance claim and has no honest before/after screenshot. Existing Spanish/English onboarding content remains unchanged; no mixed-language string was introduced.

The new requests contain no body and rely on existing cookie/bearer authentication. Analytics properties remain the allowlisted `platform`, `reason`, and canonical `value`; no sensitive or free-text data is added. Reconciliation runs as a bounded lifecycle action on authentication, reconnect, or foreground, not a timer. Three evidence sources use indexed time predicates; event-save audit lookup remains the explicit performance debt in XDR-05. LCP, INP, CLS, request latency, and p75 field funnels were not measured.

## Verification

Executed final evidence for this batch:

- `REQUIRE_MOBILE_WORKSPACE=1 npm run generate:api`: **pass**; web and mobile clients regenerated from the canonical OpenAPI source.
- Backend `stack test --test-arguments='--match=cross-device'`: **pass**, one example / zero failures.
- Backend `stack test --test-arguments='--match=onboarding'`: **pass**, three examples / zero failures.
- Backend `stack test --test-arguments='--match=evidence'`: **pass**, 21 examples / zero failures. The run used isolated SQLite fixtures; Cabal emitted existing other-module/dependency warnings.
- Focused web API/analytics/session reconciliation Jest: **pass**, four suites / 19 tests, including the bearer-token request and Party A-to-B stale-response case.
- Web TypeScript: initial run **failed** on strict null narrowing in the optional bearer path; the corrected final run **passed**.
- Focused ESLint over the six changed web implementation/test files: **pass**, zero warnings.
- Web production build: **pass**, 12,415 modules transformed and the 413,004-byte gzip initial-JavaScript budget check passed; Vite retained its existing greater-than-500 kB chunk advisory.
- Full web Jest, serial: **failed repository gate**, 185/186 suites and 1,593/1,761 tests passed. All 168 failures were confined to unchanged `src/pages/CourseRegistrationsAdminPage.test.tsx` after its first timeout; no onboarding suite failed. This is recorded as an unrelated unverified gate failure, not hidden or reported as a pass.
- Full web quality wrapper: its first run reached ESLint and reported 102 existing warnings plus one task-owned lint error; the task error was fixed. A later wrapper lint invocation stopped making progress and was interrupted. The independent final TypeScript, focused lint, build, and full Jest results above are the authoritative component results.
- Initial focused mobile set: **failed**, 53/54 tests passed; one experiment retry fixture omitted canonical `firstValue`, and production behavior correctly suppressed attribution. The fixture was corrected rather than weakening behavior.
- Corrected focused mobile onboarding/gate/social set: **pass**, three suites / 34 tests.
- Final focused provider suite: **pass**, one suite / 17 tests, including direct/reconcile serialization and winning-receipt ownership.
- Final full mobile Jest: **pass**, 67 suites / 393 tests; PostHog was explicitly disabled because no key was configured.
- Final mobile TypeScript and ESLint: **pass**; lint completed with zero warnings.
- Mobile `npm run release:check`: **pass**; five release assets, lint, typecheck, production-profile identity/config validation, and Expo public-config inspection completed. This is configuration validation, not an EAS build or store release.
- `npm run audit:catalog-lists`: **pass** in strict mode; 1,268 files scanned, 943 candidates, 132 exact duplicate groups, and 132 normalized variants classified without a blocking exit.
- `npm run quality:repo`: **pass**; generator integrity (seven tests), formal gate (8,963 findings: zero critical/errors, 314 warnings), auto-loop (42 tests), formal-audit tests (four), production-release safety (49), CI-pipeline scope (16), visual artifacts (two), and persona program (26 personas, 78 stories, 17 epics; three tests) completed.
- Final root and mobile `git diff --check`: **pass** before commit preparation.

Two initial backend attempts passed a multi-word value through `--test-arguments` in a form Hspec rejected; both ran zero examples and are command-shape failures, not test passes or product failures. An earlier concurrent full web Jest/build/typecheck attempt was resource-starved and manually interrupted; the final results above came from isolated serial runs. No test was disabled and no assertion was weakened.

All checks are local/mock/SQLite unless explicitly stated otherwise. They do not prove native-device persistence, PostgreSQL/staging behavior, production delivery, analytics ingestion, field performance, or full WCAG conformance.

## Changed files

Root/backend/web:

- `tdf-hq/src/TDF/API.hs`
- `tdf-hq/src/TDF/ServerAuth.hs`
- `tdf-hq/test/TDF/ServerSpec.hs`
- `tdf-hq/docs/openapi/api.yaml`
- `tdf-hq-ui/src/api/session.ts`
- `tdf-hq-ui/src/api/session.test.ts`
- `tdf-hq-ui/src/api/generated/types.ts`
- `tdf-hq-ui/src/analytics/onboardingProgress.ts`
- `tdf-hq-ui/src/analytics/onboardingProgress.test.ts`
- `tdf-hq-ui/src/session/SessionContext.tsx`
- `tdf-hq-ui/src/session/SessionContext.reconciliation.test.tsx`
- this report and the ready-to-use PR description
- mobile submodule pointer after its separate commit

Mobile:

- `src/api/onboarding.ts`
- `src/api/generated/types.ts`
- `src/providers/FirstRunProvider.tsx`
- `src/lib/onboardingIntent.ts`
- `src/experiments/NewUserOnboardingGate.tsx`
- `app/(tabs)/events.tsx`
- `app/(tabs)/social.tsx`
- `app/access-requests/new.tsx`
- `app/eventDetail.tsx`
- `__tests__/FirstRunProvider.test.tsx`
- `__tests__/NewUserOnboardingGate.test.tsx`
- `__tests__/SocialScreen.test.tsx`
- `__tests__/onboardingApi.test.ts`
- `__tests__/onboardingIntent.test.ts`

No screenshots were created because no rendered pixels changed.

## Remaining risks and next batch

1. Bound and measure the event-save evidence query on PostgreSQL while retaining SQLite contract correctness.
2. Add a privacy-reviewed durable analytics receipt if lossless completion measurement is worth the contract and migration cost.
3. Reconcile the long-lived stacked root/mobile branches with their current `main` branches, regenerate both clients, and rerun the full contract/backend/mobile/web matrix before merge.
4. Perform a synthetic two-device staging walkthrough: action on A, lost handshake, login on B, foreground/reconnect, Party A→B switch, and exact-once analytics inspection.
5. Exercise iOS and Android with offline relaunch, virtual keyboard, orientation, enlarged text, and screen-reader/manual accessibility checks.
6. Resume the broader platform audit on the highest-revenue unverified route family—service booking and checkout—while preserving staff workflow density.

## Handoff

Root and mobile branch: `feature/onboarding-cross-device-reconciliation-20260909`.

Focused local implementation commits:

- root/backend/web plus mobile pointer: `08dacddc0f6eab7e50e94dff3272df491380c6a9`
- mobile: `f487de478939b3c19a62b5f54aa876d96a4eb32c`

GitHub authentication remained invalid at commit time, so neither branch was pushed and no draft PR was created. The checked workflows do not deploy a feature-branch push automatically: root image publication targets `main`/manual invocation, root pull requests run CI/previews, mobile validation targets pull requests/`main`, and mobile EAS release requires manual dispatch with its build input enabled. Publication can therefore resume after authentication, but the mobile commit must be made remotely available before publishing the root pointer. No merge or deployment is authorized by this report.
