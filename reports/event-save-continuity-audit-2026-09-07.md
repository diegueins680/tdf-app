# TDF onboarding-first audit: authoritative saved-event continuity

Execution date: 2026-09-07 (America/Guayaquil)

Reviewed implementation worktree: `/private/tmp/tdf-event-save-continuity-20260907`

Local implementation branch: `feature/event-save-continuity-isolated-20260907`

Published reviewed root branch: `feature/event-save-continuity-20260907-reviewed`

Published stacked base: `feature/onboarding-continuity-20260906` at `8cd5cae4ecee4385ef0c228012368fa576162145`

Published root implementation/report head before this final metadata update: `f7e3463cdb108597d8918bf47de03ca9d753aefa`

Draft root pull request: [tdf-app #254](https://github.com/diegueins680/tdf-app/pull/254)

Local reconciled baseline: `21edb4da6427850d907e3c62b2570ec07c4e7dda`

Published onboarding parent: `8cd5cae4ecee4385ef0c228012368fa576162145`

Local implementation head before this report: `c5caf960cab59f036e7624c45a9749fb79f2797e`

Published mobile implementation: `ee3f20955b11f66a6663aa9dd4fb023e8eec749c`

Primary earlier reports: `reports/onboarding-first-ux-audit-2026-09-05.md`, `reports/onboarding-continuity-audit-2026-09-06.md`, and `reports/onboarding-first-action-evidence-2026-09-07.md`

## Executive outcome

This review completed the highest-priority deferred onboarding action from the prior reports: saving an event is now an authenticated, account-durable action rather than a device-only assertion. Web and mobile wait for the server's desired-state acknowledgement, keep the active Party boundary through asynchronous work, and only then update UI state and attempt the existing idempotent onboarding-completion handshake.

The review also found a critical anonymous-data boundary defect outside the planned onboarding path. `directory_public_event` did not enforce `social_event.metadata.isPublic`; an explicitly private event could enter the anonymous public event/venue views and search projection when its workflow state was public-listable. The new additive production migration repairs the view with a fail-closed metadata predicate and removes stale derived search documents without deleting source events, transactions, or user-owned favorites. The migration was not deployed by this task.

Account favorites now accept only canonical supported identifiers and eligible public targets. Event favorites must refer to public upcoming events. A newly inserted favorite and its append-only `favorite.saved` audit record are created in one PostgreSQL statement, and onboarding accepts `event_saved` only when that authenticated Party has in-window server evidence. Duplicate saves remain idempotent and do not create duplicate completion evidence.

The observed impact is improved privacy, cross-device continuity, and truthful onboarding state. No conversion, retention, performance, or revenue uplift is claimed because no representative field analytics or user study was available.

## 1. Capability and safety matrix

| Capability | Status | Evidence from actual check | Consequence |
| --- | --- | --- | --- |
| Repository read/write | Available | Source, tests, migration, generated clients, and this report were written in the isolated worktree. | Implementation could proceed without editing the user's concurrently changing primary checkout. |
| Branch, commit, and worktree state | Available | `git status`, `git log`, and `git rev-parse` recorded the baselines and heads above. The submodule-only dirt after commit is its intentionally untracked `node_modules` symlink/install. | Task changes are separable; dependency directories are not committed. |
| Unrelated changes | Isolated | The batch began from local reconciliation `21edb4da6`; publication is prepared from published parent `8cd5cae4e` rather than rewriting the dirty/current checkout. | Unrelated work and intervening `main` history are preserved. |
| Mobile submodule | Available | Required generation ran with `REQUIRE_MOBILE_WORKSPACE=1`; both generated files have SHA-1 `1c9284bb97dd954525840f304216582d92abae77`. Mobile commit `ee3f20955` is published. | The parent may safely reference a remotely available mobile commit. |
| Node/npm | Available | Node `v24.8.0`, npm `11.6.0`; root `npm ci` installed 1,565 packages. | Web/mobile generation, compile, lint, build, and Jest checks ran. |
| Backend runtime/toolchain | Available | Stack `3.7.1`; the executable built and the full Hspec suite ran. | Backend compilation and synthetic behavior are locally verified. |
| Database | Partial | Docker `28.5.1`, PostgreSQL client `16.10`; an isolated `postgres:16-alpine` database applied the migration twice and passed all assertions. | PostgreSQL migration behavior is verified locally; staging/production schema state is not. |
| Browser tooling and screenshots | Available but unused in this batch | Playwright `1.59.1` is installed. Existing real screenshots/results remain in `artifacts/ux-audit-2026-09-05/`. | No new runtime screenshot or browser walkthrough is claimed for this state/backend batch. |
| Android tooling/device | Partial | ADB `1.0.41` is installed; `adb devices -l` returned no attached device. | Source/Jest/static checks ran; no emulator or physical Android runtime was verified. |
| iOS tooling/device | Unavailable in this execution | `xcrun simctl list devices available` could not connect to CoreSimulatorService and reported no usable runtimes. | No iOS launch, safe-area, orientation, keyboard, VoiceOver, or screenshot evidence is claimed. |
| Test runners | Available | Hspec, Node test, Jest, TypeScript, ESLint, Vite, OpenAPI generation, Docker/psql migration scripts all executed. | Exact completed results and warnings are recorded below. |
| Network | Partial/available for scoped publication | Git push published the distinct reviewed root and mobile branches. GitHub API access required an unsandboxed call. | Feature-branch publication works; ordinary sandbox network access is restricted. |
| GitHub authentication | Partial | The inherited token used by `npm run ai:doctor` is invalid; after unsetting only the stale token variables, the keychain login was valid and draft root/mobile PR creation succeeded. | Publication must avoid the stale environment token and must not expose credential values. |
| Local configuration | Partial | Example/local environment files exist, but values were not printed. The implementation used synthetic fixtures and local databases. | No claim is made about every provider-backed integration. |
| Staging/production access | Not verified for mutation; intentionally unused | No staging/production migration, write, transaction, message, or deploy command ran. | Release and live-data behavior remain outside this local validation. |
| Synthetic accounts/fixtures | Partial | In-memory SQLite Party/auth rows, synthetic public/private events, and disposable PostgreSQL fixtures ran. No controlled OAuth/email/payment user was exercised. | Authorization/state logic is tested; external delivery and real transactions are not. |
| Analytics access | Source/test only | Existing event calls and idempotent completion semantics were inspected/tested; no PostHog dashboard or representative RUM data was accessed. | Funnel baselines and uplift remain “not yet measured.” |
| Push/deploy automation | Available and safe for feature review | Root image publication is limited to `main`/manual/workflow-call; mobile EAS builds are manual and require `start_eas_build=true`. PR events run validation/synthetics, not production deployment. | Draft feature PRs may be created without authorizing production release. |

No secret value was intentionally printed, stored in source, or included in an artifact. The GitHub CLI displayed only a masked credential indicator during authentication diagnosis.

## 2. Method and prioritization

This continuation used coordinated product strategy, UX/IA, UX writing/localization, accessibility, web/mobile/backend engineering, API-contract, privacy/security, analytics, performance, and QA perspectives. It combined source/contract inspection, historical-finding revalidation, cognitive walkthrough of the arrival-to-first-value journey, synthetic task tests, full regression collections, a real disposable PostgreSQL migration test, generated-client comparison, build/lint checks, and automation-safety inspection.

No reviewer panel, participant, interview, quotation, or independent approval is invented. The personas below are walkthrough lenses, not research participants. Priority combined severity of user harm, privacy/transaction integrity, task frequency, acquisition/repeat-engagement relevance, confidence, reversibility, dependency risk, and effort.

The coherent implementation scope was deliberately bounded to private event projection and authenticated saved-event continuity. It did not expand into a rebrand, navigation rewrite, payment-policy change, role assignment, experiment activation, or production rollout.

## 3. Architecture and authorization contract

Actual authenticated identity is the server-derived `AuthedUser` Party. `onboardingIntent` remains personalization only; it does not assign a security role, module, or permission. The favorite API accepts no caller-provided owner Party and derives ownership from the authenticated session.

Supported favorite targets are the actual public directory entities: `event` and `venue` use positive canonical 64-bit decimal identifiers; `profile` and `classified` use canonical UUIDs. Save validates target existence and public eligibility. Event save additionally requires `start_time >= CURRENT_TIMESTAMP`. Delete remains idempotent and can remove exact invalid historical data or every semantically equivalent valid legacy identifier.

The save write, duplicate test, and append-only audit insertion occur in one data-modifying CTE. Only a newly inserted favorite creates `favorite.saved`. `event_saved` onboarding completion then reads the latest Party-bound event audit no later than the database clock and compares its typed timestamp with authoritative signup time. This prevents a client click, legacy row, another Party, pre-signup record, future record, or wrong entity type from claiming completion.

The migration does not loosen confidentiality on rollback. Restoring the old view could re-expose private events, so recovery is a forward repair or stricter replacement.

## 4. Coverage matrix

This matrix distinguishes source inventory from executed runtime coverage. Broader rows reference prior reports only where they were not rerun.

| Route/screen/system | Actual roles | Device class | Important states | Inspection/verification | Status |
| --- | --- | --- | --- | --- | --- |
| Public `/buscar` directory | Anonymous; authenticated Party | Responsive web source/JSDOM | Public result, favorite loading/error/ready, saved/unsaved, stale city preference, sign-in return intent | Source, focused accessibility assertion, full Jest, build | Implemented and automated-verified; no new live browser screenshot |
| Public event/venue views and search projection | Anonymous | All web/API consumers | Missing/blank/valid/private/malformed/duplicate/unknown/wrong-type metadata; inactive workflow | SQL/source, disposable PostgreSQL 16 | Privacy repair verified locally; not deployed |
| `GET /directory/favorites?targetKind=event` | Authenticated Party | Web/mobile/API | Empty, filtered, result available/missing, auth failure, connectivity cache | Servant/OpenAPI/generated clients/Jest | Implemented; mocked client and compile verified; no staging request |
| `PUT /directory/favorites/{kind}/{id}` | Authenticated Party | Web/mobile/API | Canonical/legacy identifier, invalid kind/id, missing/private/expired target, duplicate, concurrent intent, audit | Haskell compile/tests plus real PostgreSQL CTE assertions | Implemented and locally verified; production migration pending |
| `DELETE /directory/favorites/{kind}/{id}` | Owning authenticated Party | Web/mobile/API | Canonical, legacy valid equivalent, exact invalid legacy, missing/idempotent | Source, OpenAPI, PostgreSQL assertions, client tests | Implemented and locally verified |
| `POST /session/onboarding/complete` (`event_saved`) | Eligible new authenticated Party | All clients | Missing/legacy-only/other Party/pre-signup/future/wrong kind/valid/repeated | Haskell source and Hspec | Verified locally, including idempotence |
| Mobile Events | Customer/fan Party; anonymous entry | iOS/Android source | Unknown state, server success, auth/5xx failure, connectivity-only cache, import notice/error/success, account switch, detail partial failure | Source, Jest/full static checks | Implemented/automated-verified; native runtime unverified |
| Mobile Event detail | Customer/fan Party; anonymous entry | iOS/Android source | Sign-in continuation, loading/error, save/remove, stale session, first-value handshake, cached notice | Source and affected/full Jest | Implemented/automated-verified; native runtime unverified |
| Mobile Profile saved tab | Customer/fan Party | iOS/Android source | Signed out, loading/error/retry, cache, empty, removal, partial detail failure | Source and full Jest/static checks | Implemented/automated-verified; native runtime unverified |
| Mobile account transition | Any authenticated Party | iOS/Android source | Token replacement during read/write/import, aborted request, old cache write, stale success/analytics | API client/source/unit tests | Verified with mocks; real two-account device test unverified |
| Generated OpenAPI consumers | Web/mobile engineers | Cross-platform | Filter parameter, canonical IDs, response shape, 400/404 semantics | Required generation, SHA equality, contract test, compilers | Verified locally |
| Signup/login/recovery/intent | Anonymous/new/returning Party | Web/mobile | Context return, safe destination, recovery/provider/error/duplicate states | Prior reports plus full current client regressions | Prior implementation retained; provider/staging runtime not rerun |
| Artist follow/access request | Fan; artist/professional requester | Web/mobile | Authorized action, governed request, evidence, repeat completion | Prior implementation/reports; current full regressions | Retained; live API not rerun in this batch |
| Experiment `single-feature-onboarding-v1` | Eligible new Party only | Mobile | Eligibility, exposure, exit, completion | Prior source/tests | Still paused and deferred; not reactivated |
| Public booking, ticket, marketplace, service, course commerce | Anonymous/customer/staff roles from server | Web/mobile | Loading/error/empty/success and transaction invariants | Full current unit suites plus prior runtime artifacts | Regression suites passed; real payment/provider transaction not run |
| Education/community/internal management | Authorized roles/modules from server | Web/mobile | Route access, forms, lists, dialogs, errors | Full unit regressions and prior audit inventory | No new runtime walkthrough; do not treat test inventory as complete runtime coverage |

Explicitly inaccessible, uninspected, or untested in this batch: production data and schema, staging OAuth, recovery email delivery, real payments, real communications, representative analytics, field p75 metrics, browser screen reader, VoiceOver/TalkBack, mobile OS offline/reconnect, safe areas, virtual keyboards, orientation, enlarged-text device behavior, physical touch targets, two physical devices, and real concurrent write load.

## 5. Historical finding revalidation

Historical reports were preserved unchanged: `reports/ux-ui-audit-2026-08-05.md`, `reports/onboarding-ux-audit-2026-08-20.md`, `UX_AUDIT_REPORT.html`, `reports/onboarding-first-ux-audit-2026-09-05.md`, `reports/onboarding-continuity-audit-2026-09-06.md`, and `reports/onboarding-first-action-evidence-2026-09-07.md`.

| Topic | Classification on 2026-09-07 | Evidence |
| --- | --- | --- |
| Intent discarded or treated as role | Resolved/retained | Current full web/mobile regressions pass; intent remains a closed personalization value and no role logic changed. |
| Device-local onboarding completion | Resolved in implementation; native two-device proof still missing | Prior Party-bound server progress remains present and current backend/client suites pass. |
| `access_requested` client assertion | Resolved/retained | Prior Party/time evidence implementation remains; full backend suite passes. |
| `event_saved` client assertion | Resolved in this batch | Only append-only Party-bound, in-window `favorite.saved` audit can complete it; focused/full backend passes. |
| Saved events do not follow an account across devices | Resolved in implementation; real device/staging verification pending | Mobile/web use account favorites as authority and Party-scoped connectivity cache only. |
| Saved-event state can leak or complete across an account switch | Resolved in implementation; real device verification pending | Immutable auth-session binding, abort, async-gap assertions, Party query keys, and switch tests. |
| Empty/offline/auth/server favorite states collapse together | Resolved for touched flows | Connectivity-only cache fallback; 401/5xx propagate; unknown state disables authenticated save; recovery UI is explicit. |
| Explicitly private events may appear anonymously | Newly confirmed critical defect; repaired locally | Baseline migration test reproduces private event `id=5` in the old view, then verifies it absent after repair. |
| Paused onboarding experiment has authoritative assignment/exposure | Still present/deferred | The experiment remains paused; no historical measurement is rehabilitated. |
| Mobile hard-coded Spanish/mixed language | Still present/deferred | Touched screens use the broader pre-existing inline pattern. No parallel catalog was introduced. |
| Mobile modal/screen-reader/touch target audit gaps | Partially improved, still present | Saved controls have explicit names/state and practical 44 px actions where touched; no native assistive-tech run. |
| Field Web Vitals and complete funnel baseline | Still not measured | No RUM dashboard access; laboratory/build output is not presented as field evidence. |

## 6. Findings and disposition

### EVT-PRIV-01 — explicitly private events entered anonymous projections

- Journey/role: anonymous event discovery and venue/search consumers.
- Reproduction: create a public-listable workflow event with `metadata={"isPublic":false}`; query `directory_public_event` or its search document before the repair.
- Expected versus actual: expected no anonymous projection; baseline actual included the event and could also keep a venue/search document derived only from it.
- Evidence: pre-repair view definition and executable PostgreSQL fixture (`before_private=1`).
- Severity/confidence: critical confidentiality risk; high confidence. Incidence and user impact are not measured.
- Likely cause: the view enforced workflow visibility but omitted the event metadata boundary already used by stricter decoders.
- Remedy/effort/dependencies: additive fail-closed metadata predicate, repaired view, scoped derived-document cleanup; medium effort; PostgreSQL 16 and reviewed production migration order.
- Acceptance: explicit private, malformed, duplicate, unsupported, and wrong-type metadata do not project; supported/missing legacy metadata behavior remains deliberate; source/business records stay intact; repeat apply succeeds.
- Status: implemented and disposable-PostgreSQL verified; not staged or deployed.

### EVT-SAVE-01 — favorite writes accepted arbitrary/non-public/stale targets

- Journey/role: authenticated customer/fan saving a discovery result.
- Reproduction: call favorite PUT with malformed, missing, private, expired, or past-event target.
- Expected versus actual: expected a real eligible directory target; baseline wrote any non-empty identifier for an allowlisted kind.
- Evidence: prior handler versus canonicalization/eligibility CTE, OpenAPI, policy tests, PostgreSQL assertions.
- Severity/confidence: high data-integrity and journey-truthfulness risk; high confidence.
- Likely cause: favorites were initially a generic thin persistence endpoint with no domain eligibility contract.
- Remedy/effort/dependencies: canonical kind-specific identifiers and public/upcoming predicates in the server write; medium; public directory views and migration function.
- Acceptance: invalid input is 400, ineligible target is 404, valid desired state is 200/NoContent, duplicate is idempotent, no false audit.
- Status: implemented and locally verified.

### ONB-EVID-03 — event-save completion trusted the client

- Journey/role: eligible newly signed-up Party reaching first useful value.
- Reproduction: call onboarding completion with `event_saved` without saving, or use another Party/pre-signup/future/wrong-kind evidence.
- Expected versus actual: expected durable evidence for the authenticated account; baseline accepted the allowlisted client label.
- Evidence: `TDF.ServerAuth`, synthetic SQLite fixture, full Hspec.
- Severity/confidence: high onboarding-state and measurement-integrity risk; high confidence.
- Likely cause: first-value labels were staged before saved events became server-authoritative.
- Remedy/effort/dependencies: atomic append-only server audit plus Party/time/kind evidence predicate; medium; hardened favorite endpoint and onboarding progress.
- Acceptance: missing/legacy-only/cross-Party/pre-signup/future/wrong-kind remain pending; valid save completes once; repeated handshake does not duplicate completion.
- Status: implemented and locally verified.

### MOB-AUTH-01 — in-flight favorite work could finish under a new session

- Journey/role: two accounts used sequentially on one mobile install.
- Reproduction: begin favorite read/import/write as A, replace global bearer with B before the promise settles.
- Expected versus actual: expected cancellation/no B-side cache or analytics; mutable global bearer could otherwise let later requests or callbacks observe a different session.
- Evidence: API client source, saved-event async paths, account-switch tests.
- Severity/confidence: high privacy/integrity risk; high confidence by architecture, production incidence unknown.
- Likely cause: async workflows relied on mutable Axios defaults and checked Party only at selected UI callbacks.
- Remedy/effort/dependencies: versioned immutable auth binding, explicit Authorization header, shared abort signal, assertions across async gaps; medium; existing auth provider.
- Acceptance: token replacement aborts old requests; old work cannot write cache, clear pending import, emit success, or claim onboarding for B.
- Status: implemented and mocked/full-Jest verified; native network cancellation unverified.

### MOB-STATE-04 — cache fallback mislabeled auth/server failures as offline data

- Journey/role: returning mobile customer/fan opening saved events.
- Reproduction: retain confirmed cache and make server return 401 or 5xx.
- Expected versus actual: expected explicit session/server error; a broad catch could show stale cache as though connectivity alone failed.
- Evidence: saved-event source and API error fixtures.
- Severity/confidence: high truthful-state/security-boundary risk; high confidence.
- Remedy/effort/dependencies: permit cache fallback only for no-response/timeout connectivity errors; small; Axios error taxonomy.
- Acceptance: connectivity may show dated device cache; 401/403/5xx reject and expose recovery; server response is authority.
- Status: implemented and Jest verified.

### WEB-SAVE-01 — web save state lacked authoritative hydration and disabled/error semantics

- Journey/role: authenticated or acquisition-link customer/fan in public search.
- Reproduction: load an event result with an existing favorite, slow/error favorite GET, or save after arriving anonymously.
- Expected versus actual: expected known desired state, explicit recovery, and preserved event intent; baseline controls did not represent account favorites end to end.
- Evidence: page/API diff, JSDOM accessibility test, full web suite/build.
- Severity/confidence: medium-high onboarding/repeat-engagement risk; high confidence.
- Remedy/effort/dependencies: hydrate Party favorites, disable only while unknown/error/updating, update cache after acknowledgement, refetch, preserve return path and `intent=events`; medium.
- Acceptance: saved/unsaved labels and `aria-pressed` are correct, errors do not imply empty, anonymous user can enter auth and return, onboarding is attempted only after save acknowledgement.
- Status: implemented and automated-verified; live browser/backend integration unverified.

### FAV-LEGACY-01 — noncanonical legacy IDs could duplicate or become hard to delete

- Journey/role: returning Party with older favorite rows.
- Reproduction: retain `0042` plus `42`, or uppercase UUID, then save/delete canonical ID.
- Expected versus actual: expected one semantic favorite and reliable removal; byte-string keys allowed duplicates or a stuck variant.
- Evidence: migration fixture and canonical-delete CTE assertions.
- Severity/confidence: medium data-consistency risk; high confidence.
- Remedy/effort/dependencies: converge valid equivalents preserving earliest timestamp; semantic duplicate check/delete; preserve invalid row for exact owner removal; medium.
- Acceptance: one canonical row remains, earliest `created_at` survives, repeated migration is safe, invalid data is not silently destroyed, canonical delete removes equivalents.
- Status: implemented and PostgreSQL verified.

## 7. Onboarding journey after this batch

### Entry and orientation

Public discovery remains account-free. An anonymous event result provides the task-specific action “Ingresar para guardar,” preserving the detail destination and `intent=events` rather than forcing a generic tour. No identity provider was exposed or enabled by this batch.

### Authentication and intent continuity

The prior safe return-destination and durable Party onboarding work remains intact. Mobile Events and Event detail route anonymous save intent to auth with an internal return. Product intent continues to personalize the next task only; privileged roles still require actual server authorization or the governed access-request path.

### First useful action

A public upcoming event is a deterministic first action that does not depend on stale moments or a new user's understanding of the whole product. Save succeeds only after the authenticated backend validates the target and commits the Party favorite. The UI then reflects the acknowledged state; onboarding completion remains a separate idempotent handshake backed by the append-only audit.

### Continuity and truthful state

The favorite follows the Party across web/mobile devices through the server. Confirmed Party-scoped device cache is a connectivity fallback, not authority. Unknown, connectivity cache, auth/server error, unavailable detail, pending legacy import, updating, success, and empty states remain distinct. A token change aborts or invalidates old work. Passwords and tokens are never persisted by this saved-event layer.

The provenance-safe import reads only the already Party-namespaced legacy list. The old unscoped key is deliberately never read or assigned to the next user. Pending data is removed only after every server save is acknowledged.

### Returning users and experiments

Returning accounts load server state and are not forced through completed onboarding. The existing experiment remains paused; this batch did not assign cohorts, record new exposure, or reinterpret historical results.

## 8. Design system, copy, localization, and accessibility

No parallel component library, rebrand, ranking change, or navigation rewrite was introduced. Web continues to use MUI and its existing tokens; mobile continues to use the app theme. The web city selector now waits for a valid taxonomy option and replaces a stale stored ID with the Ecuador/Quito-first governed option instead of rendering an invalid selection.

Web save controls expose visible Spanish labels, item-specific accessible names, `aria-pressed`, `aria-busy`, disabled unknown/error states, and an explicit retry/error alert. Mobile save controls expose selected/busy/disabled state, item-specific names, and practical 44 px actions where touched. Fixed `maxFontSizeMultiplier=1.5` caps were removed from EventCard so enlarged text is not arbitrarily truncated by the component.

Automated accessibility assertions ran on the web search fixture, but no scanner or component library is treated as proof of WCAG 2.2 AA conformance. Keyboard/focus behavior outside this component, browser screen reader, zoom/reflow, native screen reader, switch control, contrast measurement, touch geometry, safe areas, keyboard, and orientation remain unverified.

The saved-event changes use the broader screen-local Spanish pattern already present in these mobile surfaces. Supported English settings therefore still produce mixed language in these screens. This is a confirmed localization gap, not a reason to create another translation source. Acceptance for the next localization batch is to move the complete Events/Event detail/Profile journey into the supported ES/EN catalog in one coherent change, test both locales, and keep locale consistent across auth return.

## 9. Performance and analytics

No new field or laboratory Web Vitals run was performed. The production bundle gate passed with five preloads and 412,171 gzip bytes of initial JavaScript; this is a build budget, not LCP/INP/CLS or p75 proof. Vite continues to warn about large chunks. Saved-event detail hydration still performs one request per ID and partial failure is exposed rather than hidden; batching is the highest-value performance follow-up.

Favorite GET is filtered to `event` on mobile, reducing irrelevant account-favorite transfer. Cache writes are serialized to prevent lost updates. Import is deliberately bounded/sequential for correctness; a future outbox/bulk endpoint should be measured before parallelization.

The existing taxonomy is reused: `feature_favorite_changed`, `first_value_completed`, and `onboarding_completed`. Favorite change and first-value calls occur only after server acknowledgement. `newlyCompleted=true` gates completion analytics, preventing duplicate conversion on refresh/retry. Event IDs and state are captured; passwords, tokens, emails, phone numbers, and free text are not added. Client analytics delivery remains best effort and no field funnel completeness is claimed.

## 10. Verification record

| Executed check | Result | Scope/notes |
| --- | --- | --- |
| `npm ci` | Passed; 1,565 packages installed | npm audit reported 17 existing findings (10 moderate, 7 high); no unrelated upgrade/fix applied |
| `STACK_ROOT=... stack build tdf-hq:exe:tdf-hq-exe --fast` | Passed | Backend executable compile/build; existing `allow-newer` warning |
| Focused Hspec `--match canonicalizes` | 37 examples, 0 failures | Includes supported favorite target invariant; matcher also selected other canonicalization cases |
| Focused Hspec `--match event-save` | 1 example, 0 failures | Missing, legacy-only, other Party, pre-signup, future, wrong-kind, valid, and repeat states |
| Full Hspec | 2,472 examples, 0 failures in 5.5545 s | Synthetic/local suite with property checks; emits existing verbose SQLite debug and Cabal `other-modules` warnings |
| `./scripts/test-directory-event-visibility-migration.sh` | Passed | Real disposable PostgreSQL 16; old defect reproduction, apply twice, projection cleanup, ID convergence, atomic save/audit/idempotence |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run generate:api` | Passed | Web and actual mobile submodule generated with `openapi-typescript 7.10.1` |
| Generated client SHA comparison | Equal: `1c9284bb97dd954525840f304216582d92abae77` | Byte identity only; consumers separately compiled |
| `node scripts/test-music-directory-contract.mjs` | Passed | Privacy/public-auth/taxonomy/sponsorship/idempotency contract assertions |
| `npm run test:production-release` | 50 tests, 50 passed | Migration/release invariants; no deployment |
| Web focused Jest | 2 suites, 5 tests passed | Public directory accessibility, hydrated remove, acknowledged save, completion analytics |
| Web full Jest | 185 suites, 1,757 tests passed in 288.979 s | No skipped test reported; extensive pre-existing MUI and missing-`act` console warnings remain |
| Web TypeScript | Passed | `tsc --noEmit -p tsconfig.app.json` through build |
| Web full ESLint | Exit 0; 0 errors, 102 warnings | Existing warnings outside touched files; touched-file ESLint is clean |
| Web production build/budget | Passed; 12,415 modules; 5 preloads / 412,171 gzip initial JS | Existing >500 kB chunk warning; build is not deployment/runtime performance |
| Mobile affected Jest after final code | Included in full pass; earlier focused saved/EventCard run 2 suites, 18 tests passed | Mocks/JSDOM-style RN tests, not native OS |
| Mobile full Jest | 66 suites, 349 tests passed | No skipped test reported; PostHog deliberately disabled because test key is unset |
| Mobile TypeScript and ESLint | Both passed; ESLint `--max-warnings=0` | Static checks, not device runtime |
| `git diff --check` (root and mobile) | Passed after removing four EventCard whitespace lines | Whitespace only |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run ai:doctor` | 14 OK, 4 warnings, 0 errors | Expected dirty worktree/missing isolated memory notes plus stale inherited GitHub token; keychain auth separately succeeded |
| Workflow inspection | Passed for feature review safety | Production image only on `main`/manual; mobile EAS release manual with explicit boolean |

Diagnostic failures were not hidden: the first multi-word Stack matcher invocation ran no tests because Stack split the argument; it was replaced with valid single-token matchers. The initial event-evidence fixture exposed a real SQLite/PostgreSQL timestamp-representation portability problem; the implementation now keeps the database-clock future bound in SQL and performs the signup bound as typed `UTCTime`, after which focused and full Hspec passed. The first full mobile run found two optional-argument call-shape assertions; compatibility was restored and the complete suite reran green. A web test-isolation change exposed a stale selected-city rerender; the product selector and fixture were made stable, then focused/full suites reran green. No test was disabled or assertion weakened.

## 11. Changed files

Root/backend/contract/web:

- `tdf-hq/sql/2026-09-07_directory_event_visibility_and_favorite_evidence.sql`
- `scripts/test-directory-event-visibility-migration.sh`
- `scripts/production-migrations.json`
- `tdf-hq/src/TDF/Directory/Policy.hs`
- `tdf-hq/src/TDF/API/Directory.hs`
- `tdf-hq/src/TDF/Server/Directory.hs`
- `tdf-hq/src/TDF/ServerAuth.hs`
- `tdf-hq/test/TDF/Directory/PolicySpec.hs`
- `tdf-hq/test/TDF/ServerSpec.hs`
- `tdf-hq/docs/openapi/directory.yaml`
- `tdf-hq-ui/src/api/directory.ts`
- `tdf-hq-ui/src/api/generated/types.ts`
- `tdf-hq-ui/src/pages/DirectorySearchPage.tsx`
- `tdf-hq-ui/src/pages/DirectorySearchPage.test.tsx`
- `package.json`
- mobile submodule pointer

Mobile repository:

- `src/api/client.ts`
- `src/api/directory.ts`
- `src/api/onboarding.ts`
- `src/api/generated/types.ts`
- `src/lib/savedEvents.ts`
- `src/lib/onboardingIntent.ts`
- `src/components/EventCard.tsx`
- `app/(tabs)/events.tsx`
- `app/eventDetail.tsx`
- `app/userProfile.tsx`
- six affected API/state/component test files

Documentation:

- this report

## 12. Screenshots and artifacts

No new before/after screenshot was captured because this batch changes backend authority and state continuity, no Android device was attached, and iOS Simulator services were unavailable. No generated mockup is presented as runtime evidence.

Existing actual local web artifacts with synthetic/redacted fixtures remain available under `artifacts/ux-audit-2026-09-05/`, including:

- `baseline/signup-events-pixel7.png`
- `baseline/signup-artist-desktop-1440x900.png`
- `after/signup-events-pixel7.png`
- `after/booking-customer-safe-pixel7.png`
- `persona-public-playwright-results.json`
- `commerce-playwright-results.json`
- `performance/login-lab.json`

Those artifacts predate this event-save batch and are not claimed as screenshots of its changed behavior.

## 13. Deferred backlog and acceptance

1. **Deploy and verify the privacy/favorite migration through the reviewed release process.** Impact: code cannot safely rely on the repaired view/function until migration is present. Dependency: reviewed PR/CI/release authorization. Acceptance: staging preflight, apply, verification SQL, anonymous private-event check, favorite save/delete/onboarding integration, monitoring, and rollback-forward plan all pass. Reason deferred: production change is outside automatic authority.
2. **Real cross-device/account runtime.** Impact: mocked tests cannot prove native request cancellation/storage behavior. Dependency: synthetic staging accounts and two controlled devices/emulators. Acceptance: save on A appears on B; switch during every async phase leaks no state; relaunch/offline/reconnect converge; no duplicate analytics. Reason deferred: no device/staging fixture.
3. **Batch saved-event detail hydration.** Impact: N+1 requests raise latency and partial-failure probability. Dependency: filtered/bulk public event summary contract. Acceptance: one bounded request returns visible summaries plus explicit unavailable IDs, with cache/ETag behavior and measured request reduction. Reason deferred: schema/API design exceeds this integrity batch.
4. **Full ES/EN event journey localization.** Impact: English preference can still produce mixed-language save, import, error, and profile states. Dependency: existing supported catalog/provider. Acceptance: auth return, Events, Event detail, EventCard, Profile saved tab, alerts, accessibility names, plurals, and offline/import states pass in both locales. Reason deferred: partial string migration would create another inconsistent source.
5. **Native accessibility validation.** Impact: automated/source semantics do not prove screen-reader order, large-text reflow, focus, or touch usability. Dependency: usable iOS/Android environments. Acceptance: VoiceOver/TalkBack, switch/non-drag pointer alternatives where relevant, 200% text, orientation, keyboard, safe areas, and touch targets documented with remaining gaps.
6. **Performance/RUM.** Impact: Core Web Vitals and native interaction targets remain unmeasured. Dependency: consent-respecting representative telemetry/test environment. Acceptance: field p75 LCP ≤2.5 s, INP ≤200 ms, CLS ≤0.1 when sufficient data exists; comparable lab baselines remain separately labeled.
7. **Complete server evidence for `moment_reaction`.** Impact: one remaining first-value label is not server-authoritative. Dependency: durable Party/time reaction evidence. Acceptance: missing/cross-Party/pre-signup/future/wrong-target claims fail; valid claim completes once.
8. **Experiment measurement contract.** Impact: historical device-local exposure cannot support decisions. Dependency: authoritative signup/cohort/exposure/completion identity. Acceptance: control/treatment eligibility and one-shot exposure are server-bound, privacy-reviewed, test-complete, and approved before activation. Keep paused meanwhile.
9. **Pre-existing quality noise/dependencies.** Impact: 102 web lint warnings, noisy React test console, large bundles, Cabal module warnings, and 17 npm audit findings reduce signal. Dependency: separately scoped cleanup/upgrades. Acceptance: no behavior regression, warning baselines shrink, lockfile/release review passes. Reason deferred: unrelated upgrades/fixes were not authorized by this UX batch.

## 14. Task-based usability script (not conducted)

Use synthetic accounts and sandbox/staging services only. Record observations, not leading satisfaction claims.

- Stuart: arrive on a shared public event link, explain TDF's offer, choose Save, create/sign into a customer account, return to the same event, confirm it is saved, refresh, and find it from another controlled device.
- Customer/fan: browse without an account, save an upcoming event, distinguish loading/offline/server-error/empty states, remove it, and verify a past/private event cannot be saved.
- Artist: arrive on an artist/event link, follow or save as the first useful action, then request governed artist capabilities without assuming the artist security role.
- Staff: make a synthetic event private, confirm it disappears from public event/search/venue discovery while remaining in the internal source record; inspect but do not alter real data.
- Account-switch recovery: start a save/import as account A, switch to B before completion, verify B sees no A state/success, then return to A and reconcile.
- Accessibility pass: complete each task by keyboard on web and screen reader/switch control on native, with enlarged text, reduced motion, rotation, virtual keyboard, and a non-drag pointer alternative for any ranking interaction encountered.

No participant was contacted and no session result, quotation, completion time, conversion, or satisfaction score is claimed.

## 15. Branch, commits, and pull-request handoff

Local root source commits before clean publication:

- `dfea3d014` — fail closed for private event projections
- `3736095cb` — register the production migration
- `c5caf960c` — canonical/eligible favorites, atomic evidence, web/mobile integration pointer, and tests

Published mobile:

- Branch: `feature/event-save-continuity-20260907-reviewed`
- Commit: `ee3f20955b11f66a6663aa9dd4fb023e8eec749c`
- Draft PR: https://github.com/diegueins680/TDF-mobile/pull/46

The shorter mobile branch name already contained a separate concurrent implementation (`fa0ff3e`). It was inspected and preserved; no force-push or blind merge occurred. This reviewed implementation uses the distinct `-reviewed` branch.

The root commits were cleanly replayed onto published onboarding parent `8cd5cae4e` and published on the distinct reviewed branch without overwriting concurrent branch work. Draft root PR [#254](https://github.com/diegueins680/tdf-app/pull/254) targets the onboarding parent so the review diff contains only this coherent batch; it can be retargeted to `main` after parent draft PR [#241](https://github.com/diegueins680/tdf-app/pull/241) merges. The parent gitlink references the published mobile commit from draft mobile PR [#46](https://github.com/diegueins680/TDF-mobile/pull/46). No merge or deployment is part of this task.
