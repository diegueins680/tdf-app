# TDF onboarding-first audit: first-action evidence integrity

Execution date: 2026-09-08 (America/Guayaquil)

Reviewed worktree: `/private/tmp/tdf-event-save-publish-20260907`

Root baseline: `73cebd3c4ac82d0b2683ac09b2307ecb33cbd59d` (`feature/event-save-continuity-20260907-reviewed`)

Mobile baseline: `ee3f20955b11f66a6663aa9dd4fb023e8eec749c` (`feature/event-save-continuity-20260907-reviewed`)

Implementation branches: `feature/moment-reaction-evidence-20260907-reviewed` in the root and mobile repositories

Publication status: functional root/mobile commits are published and stacked draft PRs are open; this report is prepared for its focused documentation commit.

## Executive outcome

This batch closes the last known client-asserted onboarding first-value label. A moment reaction completes onboarding only after the authenticated Party has durable, in-window server evidence of a real reaction addition. The reaction endpoint now accepts an explicit desired state, so retrying an activation after a lost response cannot accidentally toggle the reaction off. Removing a reaction does not create completion evidence, while retained append-only evidence truthfully records that the useful action occurred.

The audit also found that the mobile artist action and backend evidence check described different domains. Mobile's real artist-follow endpoint writes `ArtistFollow`, while onboarding previously checked the unrelated core `FanFollow` table; conversely, a person-to-person social follow emitted `artist_followed` analytics and requested onboarding completion. The server now records Party-bound `artist/follow` evidence atomically with a newly created real artist follow, completion reads that evidence, and the person-follow path no longer masquerades as artist activation.

A critical mobile privacy/integrity defect was addressed in the same bounded batch. Locally retained moment captions, media URIs, comments, and reactions used one device-global key, allowing another account on the same installation to receive the prior account's local content. Local moment data and query caches are now Party-scoped, the ambiguous old key is preserved but quarantined, and read/write failures surface instead of reporting unpersisted success. Initial onboarding eligibility and completion requests are bound to the initiating authentication session.

Observed impact is stronger cross-account privacy, retry safety, and truthful completion/analytics semantics. No conversion, retention, revenue, performance, or accessibility uplift is claimed because no representative field data, user study, native device run, or new visual runtime capture was available.

## 1. Capability and safety matrix

| Capability | Status | Evidence from an actual check | Consequence |
| --- | --- | --- | --- |
| Repository read/write | Available | Backend, OpenAPI, generated clients, mobile source/tests, and this report were edited in the isolated worktree. | Safe implementation was possible without touching the user's unrelated primary checkout. |
| Branch/commit/worktree inspection | Available | `git status`, `git log`, `git rev-parse`, and submodule status established the baselines above and current feature branches. | Task changes remain reviewable and separable. |
| Unrelated uncommitted work | Isolated | Work continued in `/private/tmp/tdf-event-save-publish-20260907`, not the changing primary worktree. | No unrelated user change is included. |
| Mobile submodule | Available after install | The initial required generation failed because the real submodule lacked `node_modules`; `npm ci --prefix tdf-mobile` installed 1,404 packages, then `REQUIRE_MOBILE_WORKSPACE=1 npm run generate:api` generated both clients. | Mobile could not silently skip; its own publishable commit is required before the parent pointer moves. |
| Node/npm | Available | Node `v24.8.0`, npm `11.6.0`; web/mobile TypeScript, Jest, ESLint, Vite, and OpenAPI generation executed. | Static, unit, contract, and build verification is available. |
| Backend runtime/toolchain | Available with wrapper caveat | Stack `3.7.1` compiled/linked the 184-module test target. The first wrapper run later failed in package copy because a built executable path was absent; the freshly linked Hspec executable itself ran focused and full collections successfully. A second elevated wrapper attempt was interrupted after more than one hour of CPU-active optimization before it reached tests. Hosted `backend-quality` passed. | Compilation and tests are verified through direct and hosted runs, but neither incomplete local wrapper attempt is reported as passed. |
| Backend/database | Partial | Synthetic SQLite schemas exercised evidence and mutation helpers. Existing `EngagementEvent` storage is reused; no migration was needed. No staging or production database write ran. | Logic is locally verified; PostgreSQL concurrency and deployed schema state remain unverified. |
| Browser tooling/screenshots | Available but not used in this batch | Playwright remains installed and earlier real artifacts remain under `artifacts/ux-audit-2026-09-05/`; the web app has no moment-reaction UI to capture. | No before/after screenshot or browser runtime claim is made for this backend/mobile-state batch. |
| Android tooling/device | Partial | ADB was previously verified, but no attached device was available. | Jest/static verification ran; no Android runtime, TalkBack, keyboard, safe-area, orientation, or offline OS test is claimed. |
| iOS tooling/device | Unavailable | CoreSimulator had no usable runtime in the established baseline. | No iOS launch, VoiceOver, enlarged-text, orientation, or screenshot validation is claimed. |
| Test runners | Available | Hspec, Jest, TypeScript, ESLint, Vite, catalog audit, workflow doctor, and OpenAPI generation executed. | Exact results and limitations are recorded below. |
| Network/GitHub authentication | Partial but publication-capable | `npm run ai:doctor` found an invalid inherited token; with only stale token variables removed, `gh auth status` confirmed the keyring account and SSH Git operations. | Publication must avoid the stale environment token. No credential value is stored in source/report. |
| Local/staging configuration | Partial | Local/example configuration exists; values were not printed. Tests used synthetic fixtures and mocked providers. No staging runtime was exercised. | OAuth, email, payment, media upload, and communications behavior is not validated. |
| Synthetic accounts/fixtures | Partial | Synthetic Parties, signup timestamps, moments, reactions, follows, and storage payloads exercised account/time/state boundaries. | Domain invariants are covered; there was no real signup or two-device session. |
| Analytics access | Source/test only | Event call sites and completion rules were inspected; PostHog was disabled locally because no key was configured. | Baselines and uplift are “not yet measured.” |
| Push/PR automation safety | Available for feature review | Root image publication runs only on `main`/manual/workflow-call; mobile EAS release is manual and requires `start_eas_build`; PR workflows validate/synthesize without production deployment. Vercel and Cloudflare created automatic PR previews. | Feature branches and draft PRs may be published; no merge or production deployment is authorized. |

`npm ci --prefix tdf-mobile` reported 38 existing audit findings (27 moderate, 11 high). No automatic dependency upgrade or audit fix was attempted because it would be an unrelated, potentially breaking change. No secret value was intentionally printed, written, or included in an artifact.

## 2. Method and prioritization

The continuation coordinated product strategy, UX/IA, UX writing/localization, accessibility, web/mobile/backend/API engineering, privacy/security, analytics, performance, and QA perspectives in one plan. It combined source and canonical-contract inspection, historical-finding revalidation, cognitive walkthroughs from authentication to first value, state/identity invariant analysis, synthetic task tests, full regression runs, generated-client comparison, and push-automation review.

No separate reviewers, approvals, participants, interviews, quotations, analytics figures, or usability outcomes are invented. The usability roles below are walkthrough lenses only. Priority was based on privacy or user harm, onboarding and repeat-task frequency, measurement/business relevance, confidence, reversibility, dependency risk, and effort.

The implemented batch is deliberately bounded to evidence and continuity. It does not reactivate the paused experiment, add a web reaction surface, redesign navigation, change roles, alter ranking or pricing rules, introduce a library, perform a framework upgrade, or initiate a production deployment.

## 3. Current architecture and authorization contract

The authenticated security identity is the server-derived `AuthedUser` Party; a submitted follower Party must equal it. Product intent remains personalization only and cannot assign a role, module, or permission.

The four currently accepted first values are `artist_followed`, `event_saved`, `access_requested`, and `moment_reaction`. Every supplied value now requires durable evidence for the authenticated Party between authoritative signup time and the database/request clock. Omitting `firstValue` remains the explicit optional-onboarding exit and records no invented action. Unknown values fail closed.

Moment reaction activation/deactivation now has desired-state semantics through optional `emrrActive`. Current clients always send it; omission preserves the legacy toggle contract. Reaction row mutation and an append-only `EngagementEvent(actorPartyId, entityType=event_moment, eventType=reaction_added)` insert share one database transaction. An idempotent repeated activation does not create duplicate evidence. Removal changes current state but retains historical evidence.

Real artist follow creation and `EngagementEvent(entityType=artist,eventType=follow)` likewise share a transaction. Existing follows do not create repeated evidence. Person-to-person `Social.addFriend` is not an artist follow and no longer emits or completes as one.

Mobile completion and initial eligibility bind an immutable token/session snapshot to each request and validate it again after the response. Account replacement aborts or suppresses stale eligibility, UI success, and completion analytics. Local moment storage uses `tdf-event-moments:v2:party:{partyId}` (or an isolated guest scope); the former device-global key is not imported, exposed, or deleted.

## 4. Coverage matrix

Source inventory alone is not treated as runtime coverage.

| Route/screen/system | Actual access | Device class | Important states | Inspection/verification | Status |
| --- | --- | --- | --- | --- | --- |
| `POST /social-events/events/{eventId}/moments/{momentId}/reactions` | Authenticated Party; Party reference server-validated | API/mobile | desired on/off, repeated on/off, type change, legacy toggle, invalid IDs, missing entity | Servant handler, DTO/OpenAPI, Hspec helper, generated clients/Jest | Implemented and locally verified; route-level staging request untested |
| `POST /session/onboarding/complete` (`moment_reaction`) | Eligible authenticated Party | All clients | missing signup/evidence, other Party, pre-signup, future, wrong entity/event, valid, repeat | Server source and synthetic Hspec | Verified locally |
| Artist-follow endpoint | Authenticated Party matching follower | API/mobile | new follow, duplicate follow, Party mismatch contract, evidence count | Handler/source and Hspec helper | New/duplicate evidence verified; live/staging request untested |
| Mobile Event detail moments | Anonymous/authenticated entry; mutations require auth for remote completion | iOS/Android source | loading/error, remote/local, desired reaction on/off, lost-response retry semantics, stale session, comments/media, account switch | Source, rendered Jest, repository/storage tests, full mobile suite | Automated-verified; native runtime unverified |
| Mobile onboarding gate | Eligible new authenticated Party only | iOS/Android source | loading/offline/error/empty, remote acknowledgement, local fallback, remove, evidence pending/retry, exit | Source/rendered Jest | Implemented and automated-verified; experiment remains paused |
| Mobile initial onboarding progress | Authenticated Party/token | iOS/Android source | eligible/ineligible, offline fail-closed, replaced session, optional exit, evidence pending | Provider/API source and rendered Jest | Verified with mocks; real token refresh/device untested |
| Mobile Social artist candidate | Authenticated Party | iOS/Android source | real artist follow, person follow, unavailable list/event fallback, stale session | Source/rendered Jest plus server helper | Implemented and automated-verified; native/API integration untested |
| Local moment persistence | Party or scoped guest | iOS/Android source | separate Parties, legacy key, corrupt JSON, read/write error, desired state, comments/media | AsyncStorage mock tests | Implemented/verified with mocks; OS storage failure untested |
| Web moment reactions | No production UI/wrapper found | Responsive web | n/a | Route/component/API inventory | Unimplemented/deferred; backend contract available |
| Generated OpenAPI consumers | Web/mobile engineers | Cross-platform | optional desired-state field, legacy behavior, response/errors, completion evidence description | Required generation, byte comparison, TS/Jest/build | Verified locally |
| Existing event save/access request first values | Authenticated Party | Web/mobile/API | durable Party/time evidence and idempotent completion | Prior implementation plus current full backend/client regressions | Retained; staging not rerun |
| Experiment assignment/exposure | Eligible authenticated Party | Mobile | eligibility, exposure, conversion, exit | Source/tests and historical audit | Still paused; measurement contract not rehabilitated |
| Public discovery/auth/profile/community/service/booking/commerce/education/internal management | Actual roles/modules documented in prior audit | Web/mobile | representative loading/empty/error/success/permission states | Prior reports plus current full regression collections | No new task runtime walkthrough; do not infer comprehensive runtime coverage |

Explicitly inaccessible, uninspected, or untested in this batch: production/staging data and schema, real OAuth/recovery email/payment/media/communications, representative analytics or p75 Web Vitals, browser screen reader, VoiceOver/TalkBack, native safe areas, virtual keyboards, orientation, enlarged text, physical touch targets, OS storage denial, two devices, true offline/reconnect, real concurrent writes, and production deployment.

## 5. Historical finding revalidation

The original reports remain unchanged: `reports/ux-ui-audit-2026-08-05.md`, `reports/onboarding-ux-audit-2026-08-20.md`, `UX_AUDIT_REPORT.html`, `reports/onboarding-first-ux-audit-2026-09-05.md`, `reports/onboarding-continuity-audit-2026-09-06.md`, `reports/onboarding-first-action-evidence-2026-09-07.md`, and `reports/event-save-continuity-audit-2026-09-07.md`.

| Topic | Classification on 2026-09-08 | Evidence |
| --- | --- | --- |
| Client-asserted first values (`MEASURE-01`) | Resolved in implementation for every accepted supplied label | Current predicates require Party/time/domain evidence; full Hspec passes. Optional exit remains distinct. |
| `moment_reaction` evidence deferred in 2026-09-07 reports | Resolved in this batch | Missing/cross-Party/out-of-window/wrong-domain evidence fails; activation writes evidence atomically; focused Hspec passes. |
| Artist follow described as authoritative | Regressed/incorrect contract, now superseded | Baseline checker used `FanFollow`, while the shipped mobile action used `ArtistFollow`; handler and checker now share `EngagementEvent` evidence. |
| Person follow emitted `artist_followed` | Newly confirmed; resolved | Mobile social success path no longer emits artist or onboarding events; rendered test covers it. |
| Device-first experiment identity from 2026-08-20 | Resolved earlier; hardened here | Backend eligibility remains account-durable; initial read and completion are now token/session-bound. |
| Paused experiment measurement | Still present/deferred | Flag remains paused; no historical result is treated as reliable. |
| Local moment data account boundary | Newly confirmed critical defect; resolved in implementation | Baseline global AsyncStorage key versus Party-scoped v2 keys and two-Party test. |
| Local persistence false success | Newly confirmed; resolved | Storage I/O now propagates; rejected write test proves no returned moment. |
| Mobile mixed/hard-coded language | Still present/deferred | This batch adds no user-visible copy catalog and does not introduce a parallel localization source. |
| Web reaction capability | Still absent | No web UI/API wrapper was found; backend support alone is not described as a web feature. |

## 6. Findings and disposition

### ONB-EVID-04 — a reaction click could claim completion without a real durable action

- Journey/role: eligible new authenticated user reacting to an event moment.
- Reproduction: baseline client invoked completion after its callback; server accepted the allowlisted label without checking a domain record.
- Expected/actual: expected Party-bound, in-window server evidence; actual trusted the client assertion and could report conversion after a local fallback.
- Evidence: baseline `ServerAuth`, mobile gate/card/repository call chain, historical deferred item, synthetic SQLite cases.
- Severity/confidence/impact: high measurement and onboarding-state integrity risk, high confidence. False completion is observed structurally; incidence and conversion impact are unmeasured.
- Cause/remedy/effort/dependencies: compatibility completion path lacked reaction evidence. Reuse existing `EngagementEvent`, write it atomically on real activation, and require its Party/entity/type/time fields. Medium; no migration.
- Acceptance: missing, other-Party, pre-signup, future, wrong target/type, local-only, and removal paths cannot complete; valid activation completes once; repeat is idempotent.
- Status: implemented and locally verified.

### API-STATE-04 — toggle semantics made a lost-response retry destructive

- Journey/role: authenticated moment reactor on an unreliable network.
- Reproduction: server commits toggle-on, response is lost, client retries the same toggle.
- Expected/actual: expected reaction remains active; baseline retry toggled it off.
- Evidence: request DTO and delete/insert handler flow.
- Severity/confidence/impact: high state-integrity risk, high confidence; production incidence unknown.
- Remedy/effort/dependencies: optional desired-state Boolean with backward-compatible omitted-field toggle; current clients send true/false. Small-medium.
- Acceptance: repeated true stays active with one row/evidence; repeated false stays inactive; type replacement stays exclusive in ordinary sequential use.
- Status: implemented and helper-tested. Concurrent different-type requests remain a deferred database-invariant risk.

### ONB-EVID-05 — real artist follow and completion evidence referred to different domains

- Journey/role: new customer/fan following an artist; person-to-person social follow.
- Reproduction: complete mobile `Artists.follow`; inspect `ArtistFollow`, then observe completion checked `FanFollow`. Separately complete `Social.addFriend` and observe artist analytics/completion calls.
- Expected/actual: expected only a real artist follow to qualify; baseline real action could not qualify while a different social action claimed it.
- Evidence: mobile Social screen/API, server follow helper, baseline predicate, tests.
- Severity/confidence/impact: high onboarding dead-end and measurement-integrity risk, high confidence; business impact unmeasured.
- Remedy/effort/dependencies: one append-only artist/follow evidence vocabulary shared by writer and checker; remove false person-follow events; bind success to initiating Party/token. Medium.
- Acceptance: new real follow creates one evidence row; repeat creates none; person follow emits no artist completion; replaced session emits no stale success.
- Status: implemented and locally verified.

### MOB-PRIV-02 — locally retained moment content crossed account boundaries

- Journey/role: two Parties using the same mobile installation.
- Reproduction: Party A creates/falls back to a local moment/comment/reaction, signs out, Party B opens the same event.
- Expected/actual: expected isolation; baseline both read one `tdf-event-moments` store and a query key without Party identity.
- Evidence: storage/repository/caller source and two-Party AsyncStorage fixture.
- Severity/confidence/impact: critical local privacy and integrity defect, high confidence. Production incidence is unknown.
- Cause/remedy/effort/dependencies: global fallback store and account-neutral query identity. Namespace by Party/guest actor, include actor in query keys, and quarantine ambiguous legacy data. Medium.
- Acceptance: A/B/default scopes are separate; old global data is neither exposed nor destroyed; every local mutation uses the same scope.
- Status: implemented and mock-storage verified; real device upgrade/relaunch unverified.

### MOB-STATE-04 — local moment writes could report success after persistence failure

- Journey/role: mobile user creating/commenting/reacting during OS storage failure.
- Reproduction: reject AsyncStorage read/write; baseline helpers returned empty/success because exceptions were swallowed.
- Expected/actual: expected recoverable failure with no false completion; baseline could display/measure an action that was not retained and could overwrite after a failed read.
- Evidence: storage helper and rejection fixture.
- Severity/confidence/impact: high data-loss/false-state risk, high confidence; device incidence unknown.
- Remedy/effort/dependencies: only malformed JSON is treated as quarantined empty data; I/O failures propagate to existing error owners. Small.
- Acceptance: failed read/write rejects; no returned successful moment; malformed raw payload is not deleted.
- Status: implemented and mock-storage verified.

### MOB-SESSION-03 — eligibility/completion responses were not fully bound to the initiating login

- Journey/role: any user switching accounts while an onboarding request is in flight.
- Reproduction: start eligibility or completion, replace auth token, then resolve the original response.
- Expected/actual: expected old response ignored; baseline completion cleared eligibility in `finally` and requests did not carry/assert an immutable session binding.
- Evidence: `FirstRunProvider`, API client binding primitives, rendered asynchronous tests.
- Severity/confidence/impact: high cross-account state/measurement risk, high confidence; frequency unknown.
- Remedy/effort/dependencies: capture token/version/abort signal, send explicit Authorization, assert session after response, and retain eligibility when evidence is still pending. Small-medium.
- Acceptance: old response cannot update new session; failed first-value handshake remains retryable; failed optional exit does not trap the session.
- Status: implemented and automated-verified.

## 7. Implementation rationale, design system, copy, and accessibility

Existing architecture and dependencies are retained. `EngagementEvent` is reused as append-only action evidence, so no schema migration or generated-model edit is required. OpenAPI is canonical; web/mobile generated outputs were regenerated rather than hand-edited.

No visual rebrand, component fork, ranking change, navigation change, or business-policy copy was introduced. The reaction card keeps its visible emoji/label controls and now reports conversion only for an acknowledged activation. Existing practical 44-pixel action sizing and accessibility state remain, but no native assistive-technology or touch measurement was performed. This batch does not claim WCAG 2.2 AA or native accessibility conformance.

No new user-facing string required catalog work. Existing mobile inline Spanish/English inconsistency remains a verified broader localization gap. The next visible UX batch should move saved-event and moment recovery/status copy through the existing ES/EN localization system rather than adding another translation mechanism.

## 8. Performance and measurement

No before/after runtime performance measurement was performed; this is principally a correctness/state batch. The new evidence insert occurs only on a newly activated reaction or newly created artist follow, and repeated desired-state activation is idempotent. No LCP, INP, CLS, mobile startup, request-waterfall, or field p75 claim is made.

Analytics semantics improve without adding an SDK: a person follow is no longer labeled `artist_followed`; a local fallback or reaction removal cannot complete onboarding; only `newlyCompleted=true` emits first-value/completion events. No password, token, email, caption, comment, or other free text is added to analytics. PostHog/dashboard access was unavailable, so signup/action funnel baselines remain not yet measured.

## 9. Verification record

| Executed check | Result | Limitation |
| --- | --- | --- |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run generate:api` | Passed after real mobile dependency install; web/mobile SHA-1 both `64d9c98d452a736420fd66498bb89ea6fe909838` | Contract generation, not runtime integration |
| Full Hspec executable | 2,474 examples / 0 failures in 12.3149 s | Synthetic/local dependencies; not staging/PostgreSQL concurrency |
| Focused Hspec moment evidence | 2 examples / 0 failures | Server predicate and atomic helper, not HTTP transport |
| Focused Hspec artist follow | 1 example / 0 failures | Helper idempotence/evidence, not live HTTP |
| `STACK_ROOT=/Users/diegosaa/.stack REQUIRE_STACK=1 bash scripts/quality-backend.sh` | First attempt failed after compile/link in package copy because `.stack-work/.../tdf-hq-exe` did not exist; an elevated retry remained CPU-active in optimized `TDF.Server` compilation for more than one hour and was interrupted with exit 130 before tests | Neither attempt is counted as passed; the direct linked executable produced the Hspec results above and hosted `backend-quality` passed in 35m41s |
| Mobile focused final identity tests | 2 suites / 12 tests passed | Rendered/mocked React Native, not device runtime |
| Mobile full exact-state Jest | 67 suites / 368 tests passed in 22.612 s | Mock/local dependencies |
| Mobile TypeScript and ESLint | Both passed; ESLint used `--max-warnings=0` | Static checks |
| Web TypeScript | Passed | Generated contract compile only |
| Web ESLint | Exit 0 with 102 pre-existing warnings, 0 errors | Warnings remain; touched output is generated only |
| Web full Jest | 185 suites / 1,757 tests passed in 259.611 s | JSDOM/mocked dependencies; existing console warnings remain |
| Web production build | Passed; 12,415 modules; 5 initial preloads / 412,175 gzip bytes | Lab build only; existing >500 kB chunk warning remains; not deployed |
| `npm run audit:catalog-lists` | Passed | Static governance audit |
| `npm run test:ci-pipeline` | 16/16 passed | CI contract tests, not hosted CI execution |
| `npm run test:production-release` | 49/49 passed | Release safety invariants; no release/deploy |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run ai:doctor` | 14 OK, 4 warnings, 0 errors | Expected dirty-worktree/current-memory warnings and stale inherited GitHub token warning |
| Root draft-PR checks at functional commit `c6d44945` | 15 passed, 1 expected migration skip, 0 failed/pending | Includes hosted backend, web, mobile, contract, persona-E2E, release-policy, and preview checks; automated Vercel/Cloudflare previews are not production validation |
| Mobile draft-PR checks at commit `bc02f52d` | No checks reported | The stacked PR targets a feature branch while current mobile workflows trigger pull requests against `main`; local mobile verification above is the available evidence |
| `git diff --check` | Passed at review snapshots | Whitespace only |

One intermediate full mobile run exposed one test-fixture ordering failure after the eligibility request gained a session assertion (66 suites passed, one suite failed; 366 tests passed, one failed). The fixture was corrected to model an initial valid session followed by replacement. That intermediate run is not described as passing; final exact-state results are recorded above after rerun.

No test was skipped, disabled, weakened, or removed to obtain a green result. Mocked API/storage checks are not described as backend/native integration. No transaction, communication, production write, merge, production deployment, or real user session occurred. GitHub integrations did create automatic Vercel and Cloudflare PR previews.

## 10. Changed files

Root/backend/contract:

- `tdf-hq/src/TDF/ServerAuth.hs`
- `tdf-hq/src/TDF/Server/SocialEventsHandlers.hs`
- `tdf-hq/src/TDF/DTO/SocialEventsDTO.hs`
- `tdf-hq/docs/openapi/api.yaml`
- `tdf-hq/test/Spec.hs`
- `tdf-hq/test/TDF/ServerSpec.hs`
- `tdf-hq/test/TDF/Social/FollowHandlerSpec.hs`
- `tdf-hq-ui/src/api/generated/types.ts`

Mobile submodule:

- `src/api/events.ts`
- `src/api/onboarding.ts`
- `src/api/generated/types.ts`
- `src/lib/eventMoments.ts`
- `src/lib/eventMomentsRepository.ts`
- `src/components/EventMomentCard.tsx`
- `src/experiments/NewUserOnboardingGate.tsx`
- `src/providers/FirstRunProvider.tsx`
- `app/eventDetail.tsx`
- `app/(tabs)/social.tsx`
- `app/access-requests/new.tsx`
- `__tests__/eventMoments.test.ts`
- `__tests__/eventMomentsRepository.test.ts`
- `__tests__/EventMomentCardReaction.test.tsx`
- `__tests__/NewUserOnboardingGate.test.tsx`
- `__tests__/FirstRunProvider.test.tsx`
- `__tests__/SocialScreen.test.tsx`
- `__tests__/socialApiMapperSanitization.test.ts`

Documentation:

- this report

No new screenshot was captured. Earlier real screenshots remain under `artifacts/ux-audit-2026-09-05/`; none is presented as evidence of this nonvisual batch.

## 11. Deferred work and acceptance criteria

1. **Concurrent reaction exclusivity.** Impact: without a Party/moment unique invariant, simultaneous different-type activation requests could create more than one row. Dependency: compatible database migration and production-schema rehearsal. Acceptance: exactly one current reaction per Party/moment under concurrent writes, without duplicate evidence or destructive rollback. Status: deferred.
2. **Moment route visibility/privacy.** Impact: interaction loading appears to apply stricter visibility only to imported private events; authored/private state needs a full contract review. Dependency: event visibility semantics and fixtures. Acceptance: anonymous/ordinary Parties cannot read or mutate any non-public moment target; staff behavior remains authorized. Status: proposed/high priority.
3. **Reaction response minimization.** Impact: moment DTO reaction collections expose reactor Party identifiers to authenticated consumers. Dependency: client contract design (counts plus current-user state). Acceptance: ordinary responses reveal counts and the caller's state without enumerating other Party IDs. Status: proposed/high priority.
4. **Automatic completion-handshake retry.** Impact: the action can succeed while a later completion request fails; desired-state retry makes manual retry safe but no durable retry marker exists. Dependency: Party-scoped outbox semantics and privacy review. Acceptance: retry completion without duplicating domain actions/events, across relaunch and account switch. Status: deferred.
5. **Web reaction UI.** Impact: web users cannot perform this first value. Dependency: public moment discovery/product decision, accessible controls, ES/EN copy, and privacy-minimized response. Acceptance: keyboard/pointer/touch operation, real auth continuation, loading/error/offline/empty/success, server desired-state acknowledgement. Status: deferred.
6. **Saved-event/moment localization.** Impact: touched surrounding mobile journeys still mix inline Spanish and partial English. Dependency: existing localization catalog. Acceptance: coherent ES/EN auth-to-action path with Ecuadorian defaults and international inputs. Status: next production-visible UX batch.
7. **Native and browser accessibility/runtime.** Impact: screen-reader, reflow, focus, safe-area, keyboard, orientation, and touch behavior remain unverified. Dependency: working simulator/device and browser fixtures. Acceptance: documented manual WCAG 2.2 AA/native checks plus automated scans; no scanner-only conformance claim. Status: blocked by runtime availability in this execution.
8. **Field performance and funnels.** Impact: p75 targets and abandonment/completion rates remain unknown. Dependency: consent-respecting analytics/RUM access and stable event contract. Acceptance: signup/auth failure/first-action/completion funnels and LCP ≤2.5 s, INP ≤200 ms, CLS ≤0.1 evaluated separately for field and lab data. Status: not yet measured.

The next highest-value implementation batch is route visibility plus reaction-response privacy, followed by the user-visible ES/EN saved-event/moment state cleanup. Broader public discovery, booking/purchase, and internal workflow runtime coverage remains in the consolidated backlog rather than being mislabeled as completed here.

## 12. Task-based usability script (not conducted)

- Stuart: create a synthetic account from an event link, react once, simulate a response interruption, retry, refresh/relaunch, and explain whether onboarding is complete and why.
- Customer/fan: follow a real artist, then follow a person; explain which action counts as artist activation. Switch accounts mid-response and verify the new account receives no old success.
- Artist: arrive from a shared event/profile link, authenticate, react, return to the original context, and confirm no privileged role was self-assigned.
- Staff: inspect the synthetic Party's evidence and distinguish historical `reaction_added`/artist `follow` from current reaction state and person follows.
- Two-account privacy task: Party A creates a local-only moment/comment, signs out, Party B opens the event, and confirms no A content appears; return to A and verify its scoped data remains.
- Recovery/accessibility task: interrupt storage/network, retry, navigate reaction controls with assistive technology and nonvisual focus, enlarge text, rotate, and use the virtual keyboard without obscuring status/errors.

No participant was contacted and no task result, quotation, completion rate, or approval is claimed.

## 13. Branch and draft-PR handoff

- Root branch: `feature/moment-reaction-evidence-20260907-reviewed`
- Mobile branch: `feature/moment-reaction-evidence-20260907-reviewed`
- Root functional commit: `c6d44945e20a33135ffec7ef689fd330c2a59fcc`
- Mobile commit: `bc02f52d0cc9bc31d66afcfd0e6f1c28d14466d8`
- Root draft PR: [tdf-app #264](https://github.com/diegueins680/tdf-app/pull/264)
- Mobile draft PR: [TDF-mobile #47](https://github.com/diegueins680/TDF-mobile/pull/47)
- Stacked bases: root `feature/event-save-continuity-20260907-reviewed`; mobile `feature/event-save-continuity-20260907-reviewed`
- Root hosted validation at the functional commit: 15 passed, 1 expected migration skip, 0 failed or pending.
- Mobile hosted validation: no checks reported for the stacked feature-base PR; exact local results are recorded above.
- No merge or production deployment was performed; automatic Vercel and Cloudflare PR previews were created by repository integrations.
