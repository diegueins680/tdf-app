# TDF onboarding continuity audit and implementation

Execution date: 2026-09-06 (America/Guayaquil)

Original audit baseline: `b62ccaa11908ecba062680edde580593d5cf6574`

Continuation baseline: `edbb90e4c98f174946fc39c15d362e4bd9084f3c` (`feature/onboarding-first-ux-20260904`)

Working branch: `feature/onboarding-continuity-20260906`

Related unchanged evidence: `reports/onboarding-first-ux-audit-2026-09-05.md`

Method: coordinated expert product, UX, accessibility, design-system, localization, frontend, mobile, backend, analytics, performance, and QA assessment using repository evidence and synthetic fixtures. No user interview, production analytics review, real OAuth, real transaction, customer communication, deployment, or usability session was performed.

## Executive outcome

The remaining first-batch onboarding blocker was account continuity: web used `localStorage` and mobile used AsyncStorage to decide whether an account was new and whether onboarding or a first useful action had completed. That could repeat onboarding on another device, duplicate completion analytics, or let one browser/device state influence another account.

This batch introduces a Party-bound backend progress contract initialized atomically during password or Google signup. The server owns the signup marker, 24-hour eligibility, intent, optional exit, completion, first-value label, and idempotent `newlyCompleted` result. Existing-account sign-in may store product intent but never creates a signup marker or grants a role. Web and mobile now use this contract for authenticated eligibility and completion, and analytics emit only for the one request that claims completion.

The highest-priority acquisition action also improves: a guest following an artist is returned to that same artist after authentication and receives an explicit “Follow now” confirmation. No follow is executed silently. Mobile restores interrupted intent and uses it as the post-login fallback when no safe task-specific return URL exists.

The existing onboarding experiment remains paused. Its exposure state is still device-local, so it is not ready to reactivate. Event saving itself also remains device-local on mobile; only the onboarding completion marker becomes account-durable after that save succeeds.

## 1. Capability and safety matrix

| Capability | Status | Evidence from actual check | Consequence |
| --- | --- | --- | --- |
| Repository read/write | Available | Isolated worktree `/private/tmp/tdf-onboarding-continuity-20260906` accepted source, test, migration, and report changes. | Safe implementation is possible without touching the user’s unrelated dirty checkout. |
| Branch and commit | Available | Continuation branch started at `edbb90e4…`; schema commit `64702d26…` was created before the manifest entry. | Migration ancestry is immutable and reviewable. The branch is stacked on root PR #238. |
| Existing work protection | Available | All continuation work stayed in the isolated worktree; no hard reset, force push, or default-branch mutation occurred. | Unrelated user work remains outside this batch. |
| Mobile submodule | Available | Required client generation and checks executed against the initialized submodule. Mobile commit `29299d042e0874bd7c49a993dac03245d4edeeaa` was confirmed with `git ls-remote`. | The parent pointer references a published commit, not an unpublishable local object. |
| Runtimes and package managers | Available | Existing Node/npm, TypeScript, Jest, ESLint, OpenAPI generator, Stack/GHC, Docker, and PostgreSQL tooling executed. | Web/mobile/contract/migration verification is supported. |
| Backend/database | Partial | Disposable PostgreSQL 16 Docker test applied, reapplied, rolled back non-destructively, and reapplied the new migration. A clean optimized Stack build linked the backend and test executables, and the focused onboarding Hspec suite passed 3/3. | Migration and compiler/unit behavior are verified locally; no staging or production database was changed. |
| Browser/device tooling | Partial | Browser tooling and prior real local screenshots remain available from the 2026-09-05 batch; no browser or native device runtime was launched for this continuation. | Source/unit evidence is not represented as device or browser proof. |
| Screenshot capability | Available but unused in this batch | Prior actual screenshots remain under `artifacts/ux-audit-2026-09-05/`; no new screenshot was captured. | No fictitious before/after visual evidence is supplied for continuity behavior. |
| Test runners | Available | Web/mobile Jest, TypeScript, ESLint, Node release tests, catalog audit, migration test, and Stack were invoked. | Executed results are separated from configured or skipped checks. |
| Network access | Partial/available with approval | Sandboxed DNS was blocked once; approved `git ls-remote`, push, and GitHub CLI operations succeeded. | Remote publication and PR creation are possible; network is not assumed for unperformed live-service tests. |
| Local/staging configuration | Partial | Local dependency/build state was available; no controlled staging environment was established. | Validation remains local and mocked except for the disposable database. |
| Synthetic test accounts/fixtures | Available but not exercised end-to-end here | Existing fictional persona fixtures remain in the repository; this batch’s UI tests used mocks. | No real identity, communication, OAuth, or payment was used. |
| Analytics access | Partial | Event code and taxonomy were inspected and unit-tested; no PostHog dashboard or representative field data was accessed. | Completion semantics are tested in code; conversion uplift and field p75 remain not measured. |
| GitHub authentication | Available | Mobile branch push and draft PR #40 succeeded. Root publication is recorded in the handoff section after completion. | Draft review is possible without merging or deploying. |
| Push/deploy safety | Available for feature branches | Mobile workflows were inspected: validation/synthetics run only for `main` or PR events; release readiness is manual and gated. Root automation is rechecked before publication. | Feature publication does not authorize or perform production deployment. |
| Documented preflight | Available from first batch | The mandatory initial `npm run ai:doctor` result is preserved in the 2026-09-05 report; this continuation did not reinterpret a configured command as a new execution. | Initial warnings remain disclosed; no false fresh preflight claim is made. |

No secret values were printed. No production write, deployment, merge, real payment, real communication, or indefinite supervisor was started.

## 2. Architecture and authorization contract

The web client remains React/Vite/Material UI/TanStack Query; native remains Expo/React Native; the backend remains Haskell/Servant/Persistent/PostgreSQL. The canonical API remains `tdf-hq/docs/openapi/api.yaml`, and both generated TypeScript clients were regenerated with repository scripts.

The existing authorization roles remain unchanged. `onboardingIntent` is a closed product-personalization value and is never accepted as a role/module/permission. Post-auth destinations continue to be resolved from the roles/modules returned by the server. The new progress endpoints derive Party identity only from the authenticated bearer token or session cookie; they do not accept a caller-supplied Party ID.

## 3. Methodology and prioritization

The continuation revalidated the 2026-09-05 source/runtime audit, traced onboarding state across signup, session, web, and mobile boundaries, inspected API generation and deployment migration rules, and tested the smallest coherent account-continuity contract. Priority combined user harm, task frequency, acquisition/revenue relevance, confidence, reversibility, and implementation cost.

Observed behavior and estimated business impact remain separate. “Verified” below means the named source/test/migration command actually ran; it does not imply production, device, screen-reader, or representative-user validation.

## 4. Coverage matrix

| Route/screen or system | Roles | Devices | Important states | Inspection method | Status |
| --- | --- | --- | --- | --- | --- |
| Web `/login`, `/signup`, Google login | Anonymous prospective Customer; intent may describe artist/intern/fan goals | Responsive web source | New signup, existing login, Google new/existing, intent present/absent/invalid | Source, generated contract, TypeScript/Jest | Implemented; live OAuth/backend journey untested |
| Web `/a/:slugOrId`, `/artista/:slugOrId` | Guest to authenticated Customer/Fan | Responsive web source | Matching target, mismatched/malformed ID, already followed, follow failure/success | Source and helper Jest | Safe explicit resume implemented; component/browser flow untested |
| Web `/fans` | Anonymous, Customer/Fan, authorized manager | Responsive web source | Loading, eligible, completed, GET failure, dismiss, follow success, account change | Source, API Jest, type/lint | Authenticated durable state implemented; no focused component/E2E test |
| Web `/solicitudes-acceso/nueva` | Authenticated requester | Responsive web source | Request success/failure, completion sync success/failure | Source and shared completion unit test | First value is requested only after real request success; integration untested |
| Backend `/session/onboarding*` | Any authenticated Party | All clients | No row, eligible, completed, expired, invalid intent/value, duplicate completion, explicit exit | Haskell source/unit, OpenAPI, migration test | Implemented; clean Stack build and focused 3/3 Hspec pass; concurrency not load-tested |
| Password/Google signup transaction | Anonymous new account | All clients | Duplicate account, valid/invalid intent, Google existing/new distinction | Haskell source/JSON contract | Atomic marker in source; real PostgreSQL signup journey untested |
| Mobile auth | Anonymous to authenticated Party | iOS/Android source | Interrupted intent, invalid stored value, login/signup failure/success, safe return, authorized intent fallback | Jest, TypeScript, ESLint | Mock-verified; native runtime untested |
| Mobile first-run gate | New/returning authenticated Party | iOS/Android source | GET success/failure, account change, complete success/failure, repeat completion | Provider/gate Jest | Server-backed and fail-closed; no two-device runtime |
| Mobile events/social/access request | Customer/Fan/requester | iOS/Android source | Action success, duplicate completion, API failure | Source/Jest | Completion marker durable; event save payload itself remains device-local |
| Experiment `single-feature-onboarding-v1` | Eligible new account only | Mobile | Eligibility, exposure, conversion, exit | Source/tests | Still paused; exposure remains device-local; reactivation blocked |
| Public discovery, bookings, commerce, education, internal management | Roles documented in 2026-09-05 report | Web/mobile as previously recorded | Loading/empty/error/success/permission/checkout states | Prior runtime/source audit plus delta source check | No new regression found in touched paths; not rerun wholesale in this continuation |
| OpenAPI/client contract | Backend, web, mobile | Cross-platform | Required login fields, optional artist claim, Agency role, authenticated onboarding operations | Canonical generation and byte comparison | Verified generated clients match |
| Production migration path | Deployment operator | PostgreSQL 16 | Forward, repeated forward, constraints, cascade, rollback, reapply, schema guard | Disposable Docker PostgreSQL and Node release tests | Verified locally; full production-shaped automatic migration fixture deferred |

Explicitly uninspected or untested in this continuation: real browser UI, Safari/Firefox, physical iOS/Android, VoiceOver/TalkBack, virtual keyboard/orientation/safe areas, real OAuth, staging email, live analytics delivery, representative field performance, real payment providers, and production deployment.

## 5. Historical finding revalidation

Historical reports remain unchanged: `reports/ux-ui-audit-2026-08-05.md`, `reports/onboarding-ux-audit-2026-08-20.md`, `UX_AUDIT_REPORT.html`, and `reports/onboarding-first-ux-audit-2026-09-05.md`.

| Historical topic | Current classification | Evidence |
| --- | --- | --- |
| Device-local signup/onboarding completion | Resolved in implementation; local/runtime integration partial | New Party-bound schema/API; web and mobile production callers; migration/client/unit verification. No real two-device run. |
| Existing-account sign-in mistaken for new signup | Resolved in source | Only password/Google account creation initializes `signupCompletedAt`; existing login may store intent but remains ineligible. |
| Artist follow intent requires rediscovery | Partially resolved | Artist pages carry a validated artist-bound return URL and require explicit confirmation after auth. Directory contact is still unresolved. |
| OpenAPI omits `Agency`, permits null claim, and makes login fields optional | Resolved | Canonical schema fixed and both clients regenerated to identical output. |
| Experiment eligibility/completion is device-local | Partially resolved/superseded | Eligibility and completion are server-bound; assignment/exposure are still device-local. Experiment remains paused and historical results remain unreliable. |
| Mobile interrupted intent is written but not restored | Resolved in source/tests | Pending intent is validated/restored, cleared after success, persisted for an existing account, and used as post-login fallback. |
| Mixed-language authentication/public journeys | Still present overall | New artist-follow strings have ES/EN equivalents, but the broader historical localization backlog remains. |
| Mobile deep-link query/hostname handling | Still present/deferred | No deep-link parser/device change is included here. |

## 6. Findings and implementation disposition

| ID / journey | Reproduction, expected vs actual | Evidence and priority | Remedy, effort, dependencies | Acceptance and status |
| --- | --- | --- | --- | --- |
| **CONT-01** Returning onboarding | Sign up on device A, then sign in on device B. Expected: completed onboarding stays complete. Baseline: local flags repeat or duplicate it. | Web/mobile storage and experiment source; **high severity, high confidence**, observed architectural impact. | Add Party-bound signup/intent/completion with server eligibility and idempotent claim. Medium; database/API/client coordination. | No-row and existing login fail closed; completion survives devices; explicit exit supported. **Implemented; unit/migration verified, real two-device runtime unverified.** |
| **CONT-02** Artist follow acquisition | From a public artist, choose Follow while signed out. Expected: resume the same action safely. Baseline: return to profile and rediscover Follow. | `ArtistPublicPage`; **high, high**, frequent first-value friction. | Carry bounded internal artist identity and show explicit confirmation; no automatic side effect. Small. | Matching target resumes; mismatch/injection rejected; follow success alone records first value. **Implemented; helper-tested, component/E2E unverified.** |
| **CONT-03** Intent on existing login | Enter auth with a product intent and no task URL. Expected: authorized intent destination. Baseline mobile stored then ignored it; web routed but did not persist it. | Web/mobile auth source; **medium-high, high**. | Persist through authenticated PUT and use intent destination only after session roles/modules are known. Small. | Intent never becomes a role; Social resumes for follow intent. **Implemented and mocked-test verified on mobile; web source/type verified.** |
| **API-01** Contract drift | Generate clients from canonical schema. Expected: runtime-required fields/roles/null rules match. Baseline differed. | OpenAPI/backend types; **high, high** contract reliability. | Require login fields, add Agency, remove explicit-null claim, document security, regenerate. Small. | Web/mobile clients byte-identical and contain corrected types. **Verified.** |
| **DB-01** Destructive rollback | Roll back after progress exists. Expected: additive app rollback preserves history. Initial draft dropped the table. | Migration policy and rollback SQL; **high, high** data-loss risk. | Make rollback non-destructive; assert history survives; add schema guard. Small. | Disposable DB test preserves the row through rollback/reapply. **Verified.** |
| **MEASURE-01** Claimed first value | Directly call completion with an allowlisted value. Expected for authoritative analytics: action independently exists. Actual: endpoint deduplicates a client-observed success but does not verify the domain record. | Server/API/UI source; **medium, high** measurement integrity. | Future domain-event/outbox integration or server-side existence checks; add retry/reconciliation. Medium-large, domain-specific. | Completion cannot be forged/missed and corresponds to committed action. **Deferred; limitation documented, experiment paused.** |
| **MOB-STATE-02** Saved events | Save an event on device A and inspect B. Expected for cross-device saved content: event appears. Actual: event remains AsyncStorage-only although completion marker is durable. | Mobile events source; **medium-high, high** repeat engagement. | Add authenticated saved-event API and offline reconciliation without changing event policy. Medium, backend model/API. | Two devices converge; account switch isolates saves; offline retry is truthful. **Deferred.** |
| **ACT-02** Directory contact | Select Contact from a public directory detail while signed out. Expected: return to explicit contact action. Actual: only page URL resumes. | `DirectoryPublicDetailPage`; **medium-high, high** lead friction. | Add bounded explicit confirmation similar to artist follow, with target revalidation. Small-medium. | No automatic message; target missing/error/cancel covered. **Deferred.** |
| **EXP-01** Experiment exposure | Use the same account on two devices after reactivation. Expected: stable eligibility/assignment/exposure. Actual: exposure remains device-local. | Experiment/firstRunFlags source; **high measurement risk, high confidence**. | Keep disabled until server-backed assignment/exposure/version/window exist. Medium. | Cross-device atomic exposure and valid denominator. **Blocked from reactivation; not re-enabled.** |

## 7. Onboarding journey after this batch

### Entry and orientation

Public exploration remains available. A public artist Follow handoff preserves the artist and presents a clear confirmation after authentication. The destination is internal, bounded, and tied to the rendered artist ID; malformed/mismatched IDs do not resume. Directory Contact continuity remains the next comparable public-action gap.

### Signup, login, and recovery

The first-batch semantic forms, bounded requests, password-manager attributes, shared password contract, truthful recovery, and conditionally configured Google control remain unchanged. New signup requests may include only an allowlisted product intent. Server initialization occurs inside account creation; a Google existing-account response never receives a new-signup marker.

### Intent and permissions

Intent is stored separately from security state. Server session roles/modules still authorize the destination. Existing-account intent updates use the authenticated identity and cannot self-assign Artist, Intern, Agency, Admin, or any other privilege.

### First useful action and persistence

Artist follow and access-request success can claim first value on web. Mobile social follow, access request, moment reaction, and the current local event-save path can request the same idempotent claim. Only `newlyCompleted=true` emits first-value/completion analytics. Explicitly closing optional first steps completes without inventing a first value.

If completion sync fails after the action, the real action remains successful but progress may reappear later. That is preferable to a fake success and remains a retry/reconciliation backlog item.

### Returning and account switching

Eligibility is loaded for the current authenticated Party and fails closed when absent or unavailable. Web Fan Hub no longer uses one global dismissal flag; manager tips use a Party-namespaced local preference because they are workflow guidance, not new-user eligibility. Mobile removes local signup/completion/first-value authority and clears pending pre-auth intent at the successful account boundary.

## 8. Design system, copy, localization, and accessibility

The batch reuses existing Material UI/React Native components and design tokens; it adds no library, framework, brand, price, cancellation, payment, or permission change. The new web confirmation is an existing accessible `Alert` plus labeled button, not an automatic action or icon-only control. New artist-follow copy has Spanish and English catalog entries. The overall mixed-language backlog remains open.

No new drag-and-drop or ranking behavior changed. The prior audit’s source finding still stands: current ranking tools provide drag, keyboard operation, and separate pointer Move up/Move down controls; PartySelector exposes name/username/avatar fallbacks without requiring ordinary users to know Party IDs.

No screen-reader, contrast, 400% zoom, focus-obscuring, native enlarged-text, touch-target, safe-area, or orientation runtime was performed for this continuation. No WCAG conformance claim is made from source or unit tests.

## 9. Performance and measurement

No comparable browser performance run was made because this batch primarily changes authenticated state coordination and the backend was not launched under a controlled browser fixture. The prior constrained-network lab remains the current evidence: LCP was about 4.3 seconds and missed the 2.5-second target; CLS met 0.1; INP and representative field p75 were not measured.

No uplift, completion rate, or conversion rate is claimed. The completion endpoint is idempotent but currently records client-observed success; this is sufficient to prevent ordinary duplicate emissions, not proof of a domain action against malicious or interrupted clients.

## 10. Verification record

| Executed command/scope | Result | Proves / does not prove |
| --- | --- | --- |
| Canonical web + required mobile API generation | Passed; generated clients match byte-for-byte | Clients reflect the same OpenAPI; not runtime compatibility by itself. |
| Focused web Jest (`session`, onboarding analytics, artist intent) | 3 suites / 12 tests passed | Request shapes, duplicate event suppression, and safe artist binding pass with mocks. |
| Web TypeScript | Passed | Current web client compiles. |
| Web ESLint | Full source passed with `--quiet`; final focused changed-file run reported 0 errors and 17 pre-existing `prefer-nullish-coalescing` warnings in untouched Fan Hub expressions, and the same focused scope passed with `--quiet` | No lint error was reported in the executed scope; the final changed-file scope is not warning-clean. |
| Mobile initial focused Jest | 7 suites / 37 tests passed | Auth/provider/gate/API completion behavior with mocks. |
| Mobile full Jest | 66 suites / 329 tests passed | Full mobile JavaScript test collection ran; not native device/backend integration. |
| Mobile follow-up auth Jest | 1 suite / 14 tests passed | Existing-login intent fallback resumes Social. |
| Required mobile TypeScript and ESLint | Passed after follow-up | Final mobile TypeScript compiles and lint has zero warnings. |
| Onboarding migration PostgreSQL test | Passed | Fresh/repeat apply, constraints, FK cascade, non-destructive rollback, and reapply work on disposable PostgreSQL 16. |
| Production-release Node tests | 49/49 passed | Manifest/SHA/release/schema-verifier invariants pass; no deployment. |
| CI-pipeline Node tests | 16/16 passed | Changed scopes retain required CI selection. |
| Catalog list audit | Passed | No unreviewed/stale scanned list remained in the audited tree. Intent remains a documented transitional duplicated catalog. |
| Backend Stack compile/link plus focused Hspec | Clean optimized backend and test executables linked; focused onboarding suite passed 3/3 in 0.0144 seconds | Intent/value validation, authoritative eligibility, Party-bound session persistence, and idempotent completion pass locally; not staging/production. |
| `git diff --check` | Clean at review snapshots | No whitespace errors; not functional proof. |

The full web Jest suite was attempted and reproduced the unrelated `CourseRegistrationsAdminPage.test.tsx` timeout/overlapping-`act()` cascade already recorded by the first-batch baseline, followed by another unrelated `PromoCodeField` failure. The noisy run was stopped after those failures; the three touched onboarding suites were rerun separately and passed 12/12. No test was disabled or assertion weakened.

## 11. Implementation inventory

Backend/API/migration:

- `tdf-hq/src/TDF/Models.hs`
- `tdf-hq/src/TDF/API.hs`
- `tdf-hq/src/TDF/DTO.hs`
- `tdf-hq/src/TDF/ServerAuth.hs`
- `tdf-hq/docs/openapi/api.yaml`
- `tdf-hq/sql/2026-09-06_user_onboarding_progress.sql`
- `tdf-hq/sql/2026-09-06_user_onboarding_progress_rollback.sql`
- `tdf-hq/test/TDF/ServerAuthSpec.hs`
- `tdf-hq/test/TDF/ServerSpec.hs`
- `scripts/production-migrations.json`
- `scripts/lib/production-release.mjs`
- `scripts/test-user-onboarding-progress-migration.sh`
- `scripts/__tests__/production-release.test.mjs`

Web:

- `tdf-hq-ui/src/api/session.ts` and test
- `tdf-hq-ui/src/api/auth.ts`
- `tdf-hq-ui/src/api/generated/types.ts`
- `tdf-hq-ui/src/analytics/onboardingProgress.ts` and test
- `tdf-hq-ui/src/pages/LoginPage.tsx`
- `tdf-hq-ui/src/pages/ArtistPublicPage.tsx` and intent test
- `tdf-hq-ui/src/pages/FanHubPage.tsx`
- `tdf-hq-ui/src/pages/AccessRequestsPage.tsx`
- `tdf-hq-ui/src/i18n/locales/es.ts`
- `tdf-hq-ui/src/i18n/locales/en.ts`

Mobile commit `29299d042e0874bd7c49a993dac03245d4edeeaa`:

- `src/api/onboarding.ts` and generated types
- `src/lib/onboardingIntent.ts`, `src/lib/firstRunFlags.ts`
- `src/providers/FirstRunProvider.tsx`
- `src/experiments/NewUserOnboardingGate.tsx`
- `app/auth.tsx`, `app/(tabs)/events.tsx`, `app/(tabs)/social.tsx`, `app/access-requests/new.tsx`
- Focused auth, API, provider, gate, flags, and intent tests

Governance/documentation:

- `docs/catalog-persistence/catalog-list-decisions.json`
- `package.json`
- this report

## 12. Screenshots and artifacts

No new runtime screenshot was captured for this backend/state batch. Existing actual baseline/after browser screenshots and machine-readable Playwright/performance results remain in `artifacts/ux-audit-2026-09-05/` and are indexed by `reports/onboarding-first-ux-audit-2026-09-05.md`. They are real local renders with synthetic/redacted fixtures, not generated mockups.

## 13. Deferred work and acceptance

1. **Server-observed first value.** Impact: analytics can still be forged or missed. Dependency: domain events/outbox or per-domain existence checks. Acceptance: the same transaction or durable event proves follow/request/save/reaction, with retry and no duplicate conversion.
2. **Cross-device saved events.** Impact: a “saved” mobile event does not follow the account. Dependency: saved-event model/API/offline policy. Acceptance: two devices converge, account switch isolates data, failure/offline remains truthful.
3. **Directory contact resume.** Impact: prospective leads repeat the action. Dependency: bounded target/action state. Acceptance: explicit post-auth confirmation, no automatic message, missing/unauthorized/cancel cases tested.
4. **Experiment exposure/version.** Impact: invalid denominator and cross-device duplication if reactivated. Dependency: server assignment/exposure/version window. Acceptance: one stable cohort/exposure per account and defined conversion window. Reason: paused experiment has no user benefit worth expanding this batch.
5. **Web Fan Hub and artist component/E2E.** Impact: loading/error/account-switch/focus behavior lacks runtime regression coverage. Dependency: controlled auth/backend fixtures. Acceptance: desktop/phone tests for eligible/completed/error/follow/cancel/back/refresh plus axe and keyboard checks.
6. **Mobile native verification.** Impact: safe areas, virtual keyboard, orientation, enlarged text, VoiceOver/TalkBack, OAuth, and deep-link lifecycle remain unknown. Dependency: working device/simulator and safe provider configuration. Acceptance: recorded iOS/Android cold/warm flows without real transactions.
7. **Intent catalog governance.** Impact: six values remain duplicated across SQL/Haskell/OpenAPI/web/mobile and may drift. Dependency: governed catalog model and compatibility rollout. Acceptance: one canonical source/client mapping with strict validation retained.
8. **Learning/professional first actions.** Impact: those intents land on generic More rather than a concrete task. Dependency: select an existing authorized capability. Acceptance: each displayed intent has one real first action or is removed/deferred truthfully.
9. **Localization/legal coherence and LCP.** Impact: broader auth/legal journey still mixes languages and the prior lab LCP misses target. Dependencies: reviewed legal copy and controlled performance work. Acceptance: coherent ES/EN journey; comparable lab LCP at or below 2.5 seconds, then separate field p75 evidence.

## 14. Task-based usability script (not conducted)

Use synthetic/staging accounts only. Record task completion, wrong turns, recovery, and comments; do not infer statistics from these sessions.

- Stuart: arrive through a public artist link, explain TDF, create or use an account, follow that exact artist, refresh, then sign in on another device.
- Staff: sign in to an authorized internal route, confirm no new-user gate appears, dismiss Party-scoped manager tips, and complete one frequent scan/edit task.
- Artist: choose the artist path, create an account, request/claim only the allowed profile route, and explain pending versus granted access.
- Customer/fan: follow an artist, save an event, interrupt/relaunch authentication, and verify what is account-durable versus device-local.

No participant was contacted and no session result, quotation, completion rate, or approval is claimed.

## 15. Branch and pull-request handoff

- Root: `feature/onboarding-continuity-20260906`, commits `64702d26ac2129f96f40b921f943dea907828419` and `f24ec8cde83c2f50d569cd52ca164c2ca265dd7a`, stacked on `feature/onboarding-first-ux-20260904` / draft PR #238. Draft PR: https://github.com/diegueins680/tdf-app/pull/241.
- Mobile: `feature/onboarding-continuity-20260906`, commits `c1421e0b14509713daa3bcd7cf3bffaca5d475aa` and `29299d042e0874bd7c49a993dac03245d4edeeaa`. Draft PR: https://github.com/diegueins680/TDF-mobile/pull/40, stacked on `feature/onboarding-first-ux-20260905` / PR #39.
- No PR was merged and no production deployment was performed.

At the last pre-publication check, `origin/main` was `850a7b63dfa08395c312443d4faf57f9baad636d`, 47 commits ahead of and 13 commits behind the current stacked root history; their merge base remains the original audited baseline `b62ccaa…`. Reviewers must update/reconcile the stacked branch and rerun CI before merge rather than treating local green checks as current-main integration proof.
