# TDF onboarding-first UX/UI audit and implementation

Execution date: 2026-09-05 (America/Guayaquil)

Baseline: `b62ccaa11908ecba062680edde580593d5cf6574` (`origin/main`)

Working branch: `feature/onboarding-first-ux-20260904`
Method: expert product/UX/accessibility/engineering assessment with synthetic personas and fixtures. No user interviews, production analytics review, production transactions, or usability sessions were performed.

## Executive outcome

The highest-risk onboarding defects were not cosmetic. Self-signup could route a chosen password into a welcome-email path whose unconfigured fallback logged the entire message; reset tokens and personal identity properties could reach PostHog; password-recovery transport failures appeared successful; signup asked every user to process unrelated profile choices; and the mobile client could retain a phantom authenticated state after an authoritative `200 null` session response. These issues directly affected trust, truthful recovery, and the ability to reach a useful authorized task.

This batch removes those credential and analytics leak paths, simplifies the web signup to the fields required by the current API, retains acquisition intent in a safe same-origin URL, aligns client password validation with the server contract, adds bounded auth requests and truthful recovery errors, corrects two broken intent links, and fixes mobile revoked-session and anonymous ticket-checkout continuity. Privileged access still comes only from the returned server session; onboarding intent never assigns a security role.

The broader audit found important deferred work: durable server-side onboarding progress, resumption of follow/contact intent, local-storage contact-data retention, mixed-language legal/auth content, legacy booking completion routing, mobile deep-link normalization, campaign input labels, and OpenAPI/client drift. The existing onboarding experiment remains paused; its historical measurement contract does not reliably distinguish a new signup from an existing account on a new device.

## 1. Capability and safety matrix

Checks were performed before product edits. Secret values were not printed; only configuration file presence and key names were inspected.

| Capability | Status | Actual evidence | Consequence |
| --- | --- | --- | --- |
| Repository read/write | Available | Read the repository and created an isolated worktree under `/private/tmp`; task files and artifacts were written successfully. | Audit, implementation, tests, and local commits are possible. |
| Baseline/branch | Available | `git ls-remote` and fetch resolved `origin/main` to `b62ccaa…`; isolated branch `feature/onboarding-first-ux-20260904` starts at that exact commit. | Findings and screenshots have a reproducible source baseline. |
| Existing work protection | Available | The original checkout was already dirty only in unrelated `MEMORY.md`, `memory/*`, and `DREAMS.md` files; it later moved externally to `fix/party-selector-identity-followups` at `a5a9ab…`. No task edits were made there. | The isolated worktree prevents accidental inclusion or overwriting of another contributor's work. |
| Mobile submodule | Available | `tdf-mobile` was initialized at `3e2ce584…`; its task branch is `feature/onboarding-first-ux-20260905`, and commit `10d5dc9e2a733c9c61b5b5f288d6cdfc28a2e623` is published on the mobile remote. | Native source changes and tests are included without leaving an unavailable parent pointer. |
| Runtimes/package managers | Available | Node `v24.8.0`, npm `11.6`, Stack `3.7.1`, Docker, PostgreSQL 16, Playwright, and installed browser binaries were detected. | Web, native JavaScript, browser, and targeted Haskell validation are supported. |
| Dependencies | Available with known risk | Root and mobile dependencies installed. `npm audit` reported 17 existing root findings (10 moderate, 7 high) and 38 mobile findings (27 moderate, 11 high). | Tests/builds can run; dependency advisories remain a separate remediation stream and were not hidden by upgrades. |
| Backend/database | Partial | An already-running local backend answered `/health` on port 8080 and PostgreSQL 16 accepted local connections. It was not started or controlled by this task. | Read-only/local integration observation is possible, but its fixtures and commit provenance are not controlled enough to call it an end-to-end backend proof. |
| Browser/runtime inspection | Available | Playwright Chromium rendered the app, ran desktop and Pixel 7 flows, axe checks, screenshots, and throttled lab measurements. | Comparable web runtime evidence is included. |
| Native device tooling | Partial | iOS simulators and an Android AVD exist, but no device was booted for this run. | Mobile code, Jest, lint, and type checks are verified; safe areas, virtual keyboard, orientation, enlarged text, and screen-reader behavior remain unverified at runtime. |
| Screenshot capability | Available | Real baseline and after PNGs were captured from local browser instances with synthetic/redacted data. | Visual comparison is inspectable; images are not generated mockups. |
| Test runners | Available | 351 test/spec files were inventoried. Jest, Playwright, TypeScript, ESLint, Vite, and Stack commands executed. | Relevant automated regression coverage is possible; a configured command is not counted unless its executed result appears below. |
| Network access | Available | Git remotes were fetched/queried and the mobile feature branch was pushed. | Remote commit publication is possible. |
| Local/staging configuration | Partial | `.env.example` and `tdf-hq/.env.lo` exist; no controlled staging environment was proven. | Validation is local; no staging or production claim is made. |
| Synthetic accounts/fixtures | Available | Browser tests use clearly fictional `@persona.test` identities and synthetic catalog/event data. | Public/auth task testing avoids real customer data and communications. |
| Analytics access | Partial | PostHog integration and event taxonomy were inspectable in source; no live dashboard or representative field dataset was available. | Event semantics/privacy were audited in code; signup/conversion baselines and p75 Web Vitals are “not yet measured.” |
| GitHub authentication | Partial | Git SSH/HTTPS remote operations work; `gh auth status` reports an invalid token. | Feature branches can be pushed. Draft PR creation/check inspection through `gh` is unavailable in this run. |
| Push/deploy safety | Available | Root workflows were inspected: feature pushes do not deploy; image/release paths are default-branch, scheduled, or manual. Mobile feature pushes likewise do not run a deployment workflow. | Feature-branch publication is safe; nothing was merged or deployed. |
| Documented preflight | Available with warnings | `npm run ai:doctor` completed with 16 OK and 2 warnings: dirty original checkout and invalid GitHub CLI authentication. | The warnings are explicitly preserved rather than treated as a clean preflight. |

No production data was changed, no real payment or customer communication was sent, and no unattended loop, supervisor, deployment, merge, or default-branch push was launched.

## 2. Architecture, authorization, and baseline

The web client is a React/Vite application with Material UI, React Router, TanStack Query, i18next, PostHog, Jest, and Playwright. The backend is Haskell/Servant with PostgreSQL. Native mobile is an Expo/React Native submodule with AsyncStorage/SecureStore, Jest, ESLint, and TypeScript. The canonical API source includes `tdf-hq/docs/openapi/api.yaml`; no generated API output was hand-edited in this batch.

Security roles discovered in `tdf-hq/src/TDF/Models.hs` are: `Admin`, `Manager`, `StudioManager`, `Engineer`, `Teacher`, `Reception`, `Accounting`, `LiveSessionsProducer`, `Intern`, `Artist`, legacy `Artista`, `Webmaster`, `Promotor`, `Promoter`, `Producer`, `Agency`, `Songwriter`, `DJ`, `Publicist`, `TourManager`, `LabelRep`, `StageManager`, `RoadCrew`, `Photographer`, `AandR`, `Student`, `Vendor`, `ReadOnly`, `Customer`, `Fan`, and `Maintenance`. These are authorization roles, not product personas. Signup intent is personalization/acquisition context only; the changed routing resolves destinations from the returned authorized session.

### Baseline failures

- Pre-change `npm run test:ui`: 1 suite failed and 180 passed; 168 tests failed and 1,564 passed (1,732 total). All failures were concentrated in the pre-existing `CourseRegistrationsAdminPage.test.tsx` suite with overlapping async/act state contamination. This batch does not claim to fix it.
- The first pre-change mobile full run had one 5-second `TicketCheckout` timeout (63 suites passed, 313/314 tests); an independent rerun passed 64 suites and 314 tests. It is recorded as a pre-existing timing flake, not an implementation regression.
- Static UI audit at baseline/current source reports zero errors and three warnings: one likely false positive in `LabelArtistsPage`, and two genuine unlabeled campaign inputs in `TdfDomoCampaignPage`.
- Dependency audit findings are recorded in the capability matrix and were not introduced by this work.

## 3. Methodology and prioritization

Evidence combined:

1. Repository instructions, architecture, routes, contracts, authorization, feature flags, analytics, localization, state persistence, scripts, tests, and historical reports.
2. Cognitive walkthroughs from public landing/profile/event/service links through authentication to the intended authorized action.
3. Runtime task tests using fictional personas on desktop Chromium and an emulated Pixel 7, including loading, failed transport, validation, success, refresh-routing, keyboard submission, and automated accessibility checks where implemented.
4. Source-level accessibility and design-system review, including party selection and rank-reordering interactions.
5. Comparable local production builds and five-run constrained-network/4× CPU laboratory measurements.

Severity considers user harm, security/privacy, task frequency, revenue/operational relevance, confidence, and implementation cost. “Observed” means reproduced in code or runtime; business impact remains estimated unless real analytics exist. Simulated personas are not user research and generated test data are not customer evidence.

## 4. Coverage matrix

`Runtime` means an actual local journey was exercised. `Source/tests` is not presented as equivalent runtime coverage.

| Area and representative routes/screens | Actual roles/capabilities | Devices/states inspected | Method | Verification status |
| --- | --- | --- | --- | --- |
| Public orientation/discovery: `/inicio`, `/tdf`, `/buscar`, public profile/artist links | Anonymous; authenticated users | Desktop/phone; loading, populated, public navigation | Runtime tests, screenshots, source | Partially verified; discovery runtime covered, every campaign/shared-link variant not exercised |
| Signup/login/reset: `/login`, `/reset-password` | Anonymous; returned session roles | Desktop/Pixel 7; signup, keyboard submit, invalid login, reset network error, safe redirect | Runtime, axe, Jest, source | Verified for implemented paths; OAuth cancel/expired live provider unverified |
| Artist/profile claim and following | Anonymous, Fan/Customer, Artist after server grant | Desktop signup claim; existing/missing claim data | Runtime screenshot, source/tests | Claim option preserved; automatic follow intent resumption deferred |
| Events/tickets: `/eventos/*/entradas`, order tracking, native `ticketCheckout` | Anonymous guest web; authenticated mobile user | Web runtime with synthetic event/order; mobile unit tests; loading/auth-required/success truthfulness | Runtime web, mobile Jest, source | Web synthetic checkout verified; native physical-device and real sandbox-provider flow unverified |
| Services/bookings: `/servicios`, public booking/order, `/reservar`, `/estudio/calendario` | Anonymous customer, Customer, staff | Responsive source; success/authorization routes | Source/tests | Audited, not fully runtime-tested; legacy success CTA defect deferred |
| Marketplace/commerce/orders | Anonymous/customer, Vendor, staff | Responsive source; draft/contact/checkout persistence | Source/tests | Audited; local-storage personal-data issue deferred, real sandbox payment unverified |
| Education/classes/courses/trials | Student, Teacher, staff | Responsive source; empty/loading/forms | Source/tests | Audited; admin registration suite has pre-existing failures; no complete runtime journey in this batch |
| Community/fan/social/chat | Fan/Customer and staff | Responsive source; empty/loading/follow paths | Source/tests | Audited; useful-action resume remains partial |
| Studio/CRM/inventory/operations | Admin, Manager, StudioManager, Engineer, Reception, Accounting, others by route | Desktop source; dense tables/forms/permission gates | Source/tests | Broad static coverage only; protected workflows not run with controlled role fixtures |
| Label/distribution/releases/DDEX | Artist, LabelRep, Agency, staff | Desktop source; auth handoff/query state | Source plus focused Jest | Distribution redirect regression verified fixed; full distribution submission untested |
| Internships/school | Intern, Student, Teacher, staff | Web source; auth intent | Source/tests | Audited; no live application workflow performed |
| Campaigns/Domo/venues | Public customer, Promoter/Promotor, staff | Source; input names/labels, quote/booking paths | Static audit/source | Two label gaps confirmed and deferred; no production campaign actions |
| Admin/access requests/users/roles | Admin and governed approvers | Desktop source; denied/pending states | Source/tests | Permission model inspected; no role mutation performed; mailto access request remains deferred |
| Shared `PartySelector` | Authorized staff contexts | Name, username, avatar/fallback, pagination | Source/tests | Existing capability is strong; ordinary users need not know Party IDs; no replacement introduced |
| Ranking/category drag interfaces | Authorized staff/curators | Keyboard and pointer move controls | Source/tests | Source confirms drag plus Move up/Move down alternatives; touch/screen-reader runtime remains unverified |
| Mobile session/onboarding/deep links | Returned server roles; anonymous | Jest/type/lint; null session, anonymous checkout, persisted state, route parsing | Source and Jest | Two critical paths verified in tests; query-loss/double-slash/durable completion deferred; no device runtime |
| Localization/legal/media | All | Spanish/English catalogs, locale continuity, reduced motion, media controls | Source/runtime spot checks | Partial: reduced motion/media control present; auth/legal language coherence incomplete |
| Performance/analytics/privacy | All | Pixel 7 lab profile; PostHog source | Lab/browser/Jest/source | Lab LCP/CLS measured; no field p75 or INP; privacy hardening verified in unit tests |

Inaccessible or untested: live PostHog/field analytics, controlled staging, real OAuth providers, password-reset email delivery, real payment providers, production content freshness, staff workflows with every role, native screen readers/devices, Safari/Firefox for the changed signup batch, and production deployment behavior.

## 5. Historical report revalidation

Historical inputs were preserved unchanged: `reports/ux-ui-audit-2026-08-05.md`, `reports/onboarding-ux-audit-2026-08-20.md`, and `UX_AUDIT_REPORT.html`.

| Historical topic | Current classification | Evidence |
| --- | --- | --- |
| Product intent could become a role/forbidden redirect | Resolved | Current signup derives baseline roles independently of intent; post-auth routes are safe and capability-aware. |
| Public exploration blocked by authentication | Resolved for inspected discovery | Public directory, profiles, events, and offers have anonymous surfaces and semantic shells. |
| Reduced motion and user-controlled media | Resolved in source | Existing theme/media components respect reduced motion and do not auto-play onboarding audio. |
| Consent/minimum data | Partially resolved, improved here | Phone is deferred; legal acceptance and marketing are separate. This batch removes signup-time marketing choice and unrelated artist selection, but legal pages/auth language remain inconsistent. |
| General request timeout | Partially resolved, improved here | General API infrastructure had timeouts; auth endpoints did not. This batch adds a consistent 30-second auth timeout. |
| Offline feedback | Partially resolved | Authenticated surfaces include handling, but public authentication still lacks a dedicated offline banner. |
| Mixed-language authentication | Still present | Spanish-first login includes linked legal pages and residual strings not consistently localized. |
| Competing/duplicate `/tdf` account CTAs | Still present, fixed here | “General” and fan CTAs resolved to the same exact URL; the general CTA now retains a distinct neutral signup path. |
| Durable onboarding completion | Not verifiable / prior claim superseded | Web uses local storage and mobile uses device storage; neither proves account-bound, cross-device completion. |
| Prior test/pass totals | Not independently verifiable as current | This report records only commands executed against baseline/current worktrees. |
| Older HTML findings about missing shells/routes | Largely resolved or superseded | Current router and public surfaces contain later architecture; remaining findings are recorded separately below. |

## 6. Findings and disposition

Each row combines the required reproduction, expectation/actual, evidence, prioritization, remedy, dependencies, acceptance criteria, and status. Effort is a relative engineering estimate, not a delivery promise.

| ID / journey / role | Reproduction; expected vs actual | Evidence; severity / confidence / observed impact | Cause and remedy; effort / dependency | Acceptance and status |
| --- | --- | --- | --- | --- |
| **SEC-01** Self-signup / anonymous | Sign up while SMTP is absent. Expected: chosen password never enters email/logging. Actual: signup passed it as a temporary password and the fallback logged the composed message. | `ServerAuth.hs`, `Email.hs`; **critical / high**. Observed credential-exposure path; no claim that production logs contained it. | Replaced self-signup welcome with credential-free account-created email; unconfigured fallbacks log only generic skips. Small; no schema change. | Changed backend/application compiled and linked; Stack-backed credential sentinel invariant passed. **Implemented and focused-invariant verified; full Hspec target not completed.** |
| **SEC-02** Reset URL / anonymous | Open `/reset-password?token=…` with analytics enabled. Expected: reset secrets never leave auth handling. Actual: automatic pageview could include the full URL. | `posthog.ts`, `ResetPasswordPage.tsx`; **critical / high**. Observed source path. | Sanitize sensitive query keys before analytics and remove token from visible history after one read. Small; PostHog hook. | Token sentinel is redacted in analytics tests; address no longer retains token. **Implemented and unit-verified.** |
| **PRIV-01** Analytics identity / authenticated | Establish a session. Expected: analytics uses the minimum pseudonymous identity. Actual: username/email, display name, roles, and DOM autocapture were enabled/permitted. | `SessionContext.tsx`, `posthog.ts`, `docs/analytics.md`; **high / high**. Observed unnecessary personal-data collection risk. | Identify only by opaque party ID, disable DOM autocapture, enable personal-data masking, sanitize event properties, update policy. Small. | Tests reject email/token sentinels and assert autocapture off. **Implemented and unit-verified.** |
| **REC-01** Password recovery / anonymous | Abort `/v1/password-reset`. Expected: generic retryable failure. Actual: UI showed success in catch path. | Runtime Playwright and `LoginPage.tsx`; **high / high**. Observed false success/dead end. | Show the same non-enumerating success only after HTTP success; on transport failure show a retryable error. Small. | Desktop/Pixel 7 aborted transport never renders success. **Implemented and runtime-verified.** |
| **AUTH-01** Auth reliability / all authenticating users | Leave login/signup/reset request unresolved. Expected: bounded wait and useful error. Actual: no auth-specific abort. | `api/auth.ts`; **high / high**. Observed indefinite-wait risk. | Shared 30-second AbortController with stable Spanish error. Small. | Never-resolving signup unit test times out deterministically. **Implemented and unit-verified.** |
| **ONB-01** Signup / new public user | Open general/event signup on a phone. Expected: only immediately necessary fields and one next task. Actual: oversized dialog included optional marketing and unrelated artist selection/guide. | Baseline/after screenshots, runtime, `LoginPage.tsx`; **high / high**. Observed cognitive and viewport burden. | Actual `<form>`, autocomplete, Enter submit, name/email/password/terms only; artist lookup only for claim intent; no duplicate follow payload. Medium. | Synthetic signup submits correct payload, `marketingOptIn:false`, no `fanArtistIds`, and has no serious/critical axe violations. **Implemented and runtime-verified.** |
| **ONB-02** Intent continuity / new public user | Enter via an intent CTA, open/close signup, refresh. Expected: safe context survives auth UI and redirects only within app. Actual: duplicate general/fan target and dialog-only intent made continuity ambiguous. | `/tdf` tests, login routing/source; **medium-high / high**. | General CTA uses neutral `/login?signup=1`; signup writes/removes normalized intent in current URL and existing safe-redirect validator remains authoritative. Small. | Exact CTA URLs differ; external redirects remain rejected by existing tests. **Implemented and unit/runtime-verified.** |
| **DIST-01** Distribution acquisition / artist | Follow distribution signup CTA. Expected: auth returns to requested route. Actual: CTA used unsupported `next`, while login reads `redirect`. | `DistributionLandingPage` focused test; **high / high**. Observed dead-end/misdirection. | Emit canonical encoded `redirect`. Trivial. | Test asserts `/login?redirect=…`. **Implemented and unit-verified.** |
| **MOB-AUTH-01** Returning mobile user | Persist token, then receive authoritative `/session` JSON `null`. Expected: anonymous state and cleared cache. Actual: client retained phantom authentication. | `AuthProvider.tsx` and Jest; **critical / high**. Observed permission/session integrity risk. | Treat `200 null` as revocation and clear token, party/session cache, SecureStore, and legacy storage. Small. | Test asserts anonymous state and all persistence cleared. **Implemented and Jest-verified.** |
| **MOB-COM-01** Native ticket checkout / anonymous buyer | Open `ticketCheckout?eventId=…` signed out. Expected: event context plus auth continuation. Actual: auth loading/gate could remain an indefinite spinner. | `ticketCheckout.tsx` and Jest; **high / high**. Observed revenue-journey blocker. | Load public event without auth; separate auth-loading from anonymous; show Create account / Existing account / Back with encoded return target. Medium. | Anonymous fixture sees event and auth choices; return URL contains event. **Implemented and Jest-verified; device runtime unverified.** |
| **REV-01** Legacy public booking completion / Customer | Complete legacy public booking then follow calendar CTA. Expected: customer-safe receipt/tracking/resume. Actual: CTA points to protected `/estudio/calendario`, typically forbidden to Customer. | Route/role source; **high / high**. Observed authorization dead end; frequency unknown. | Route to public order tracking or capability-aware destination. Medium; requires legacy contract fixture. | Customer completion never exposes a forbidden CTA and can retrieve order status. **Deferred to next revenue batch.** |
| **ACT-01** Follow/contact intent / new fan/customer | Start follow/contact from public profile, authenticate. Expected: intended action resumes or is explicitly confirmed. Actual: destination is preserved but action is not automatically resumed. | Route/action source; **medium-high / high**. | Add single-use, account-bound intent record and idempotent resume/confirmation. Medium; backend/state semantics required. | One follow/contact attempt, no duplicate event, safe cancellation and cross-account isolation. **Deferred.** |
| **PRIV-02** Marketplace/booking drafts / public customer | Enter contact data then inspect browser storage. Expected: minimum retention scoped to account/session. Actual: PII-bearing drafts can persist in `localStorage`. | Marketplace/booking source; **high / high**. | Minimize fields, use scoped/expiring storage or server draft, clear at session boundary. Medium; product retention decision needed. | No contact PII survives expiry/logout/account switch; interrupted draft behavior documented. **Deferred for privacy design.** |
| **STATE-01** Onboarding completion / returning web/mobile user | Complete onboarding, then use another browser/device or switch accounts. Expected: durable identity-bound state. Actual: localStorage/AsyncStorage device flags can repeat or leak classification. | Web/mobile progress source; **high / high**. | Add server-owned versioned progress keyed to authenticated identity, with device cache only as a hint. Large; API/database migration. | Cross-device completion persists, account switch isolates state, optional onboarding is escapable. **Deferred pending contract.** |
| **I18N-01** Auth/legal / international and Ecuador users | Traverse Spanish signup to linked terms/privacy. Expected: coherent selected language. Actual: mixed catalogs/legal pages and hard-coded strings. | Runtime/source; **medium / high**. | Move remaining strings into existing i18next catalogs and provide reviewed Spanish/English legal equivalents without changing promises. Medium; legal/content review. | No mixed-language path for supported locale; Ecuador examples retain international acceptance. **Deferred.** |
| **A11Y-01** Domo campaigns / staff | Inspect keyboard/accessibility tree for two campaign inputs. Expected: programmatic labels. Actual: static audit identifies unlabeled fields near lines 982/1000. | `npm run audit:ui:static`; **medium / high**. | Associate visible labels/IDs and error descriptions using existing form primitives. Small. | Zero static warnings for those inputs plus keyboard/axe route check. **Deferred outside coherent onboarding batch.** |
| **MOB-ROUTE-01** Native protected return / authenticated user | Open a protected link containing query parameters, authenticate. Expected: exact safe internal target resumes. Actual: route handling drops query state. | Mobile router/auth source; **high / medium-high**. | Preserve parsed path+query in typed allowlisted return target. Medium; navigation integration test/device check. | Event/service IDs and filters survive auth; external schemes rejected. **Deferred.** |
| **MOB-LINK-01** Native deep links / shared-link visitor | Open a supported link with a doubled slash. Expected: normalized supported route or clear not-found. Actual: parser misses it. | Mobile link parser source; **medium / high**. | Normalize path separators before allowlist matching and add fixtures. Small. | Single/double slash fixtures resolve identically without broadening external routes. **Deferred.** |
| **API-01** Signup/session contract / clients | Compare OpenAPI roles and signup/login schemas to server models. Expected: generated client contract matches runtime. Actual: `Agency` is omitted, `claimArtistId` nullability differs, and login fields are marked optional. | `tdf-hq/docs/openapi/api.yaml`, model/source audit; **high / high**. | Correct canonical schema, regenerate both clients with supported scripts, run compatibility tests. Medium; coordinate consumers. | Generated web/mobile types match server JSON and role enum; no hand edits. **Deferred to contract batch.** |
| **EXP-01** Existing onboarding experiment / new vs returning | Review cohort identity, completion, and persistence. Expected: stable account eligibility/exposure/completion. Actual: device flags can classify existing-account sign-in as new and completion is not durable. | Experiment/progress source; **high / high** for measurement invalidity. | Keep paused; define identity assignment, eligibility window, idempotent exposure/completion, and server state before any relaunch. Medium-large. | Contract distinguishes signup from sign-in/new device and survives refresh/account switch. **Deferred; not reactivated.** |

## 7. Onboarding journey after this batch

### Entry and orientation

Public directory, artist, event, service, and TDF surfaces remain explorable without a forced generic tour. Account creation is invoked when an action needs identity. Event/general/artist intent is visible in a concise signup alert and represented by an allowlisted query value; arbitrary external redirects are rejected by the existing routing utility.

### Signup, login, and recovery

Signup and recovery are now semantic forms, support Enter, password-manager autocomplete, copy/paste, native email input behavior, and accessible visibility controls. The client password policy mirrors server constraints: at least eight Unicode characters, at most 72 UTF-8 bytes, and no control/format/line/paragraph characters. Phone, biography, marketing choice, and artist selection are not required for general signup. Recovery success is shown only after the request succeeds; the generic wording continues to avoid account enumeration.

OAuth remains conditionally rendered only when `VITE_GOOGLE_CLIENT_ID` exists. Live canceled-OAuth and provider delivery were not exercised.

### Intent, permissions, and first useful action

Intent remains personalization. Server-returned roles/capabilities determine the landing destination, and an artist claim is offered only for the artist-profile path. Event and professional paths can continue to their public/capability-safe route; governed roles still require the existing approval mechanisms. Automatic resumption of follow/contact remains incomplete and is the most important onboarding continuity follow-up.

### Continuity and truthful states

Web auth requests are bounded and distinguish server/transport errors from success. Mobile now distinguishes session loading, authoritative revocation, and anonymous checkout; it displays the requested event before asking the buyer to authenticate. Cross-device completion is not solved: current completion flags remain device-local and are explicitly deferred to a versioned server contract.

### Existing experiment

No paused flag was changed. Historical completion or conversion results should not be trusted for relaunch decisions until eligibility and completion are account-bound and new signup is distinguished from existing-account sign-in on another device.

## 8. Design-system, content, and interaction rationale

Changes reuse the existing Material UI theme, field primitives, dialogs, semantic colors, focus treatment, responsive breakpoints, and TDF assets. There is no rebrand, new component library, framework rewrite, dependency, price/policy change, or role-rule change.

“Minimal” was applied as reduced decision burden: one primary account path, contextual artist claiming only, deferred optional data, concise Spanish guidance, truthful recovery, and fewer duplicate CTAs. The final dark-mode floating labels use an opaque dialog-colored backing and AA-compatible foreground after axe exposed a contrast regression during verification.

The shared PartySelector already displays names/usernames, avatars/fallbacks, and paginated choices without requiring an ordinary user to know an internal Party ID. Ranking/category tools retain drag behavior and expose Move up/Move down controls for keyboard and non-dragging pointer operation; no ranking formula or business meaning changed.

## 9. Accessibility

Target: WCAG 2.2 AA for web, with applicable native practices. Authoritative criteria used in review include [Dragging Movements (2.5.7)](https://www.w3.org/WAI/WCAG22/Understanding/dragging-movements.html), [Target Size (Minimum) (2.5.8)](https://www.w3.org/WAI/WCAG22/Understanding/target-size-minimum.html), [Accessible Authentication (Minimum) (3.3.8)](https://www.w3.org/WAI/WCAG22/Understanding/accessible-authentication-minimum.html), and [Focus Not Obscured (Minimum) (2.4.11)](https://www.w3.org/WAI/WCAG22/Understanding/focus-not-obscured-minimum.html).

Actually verified in the changed web flow: form semantics, labels/names, required attributes, autocomplete, password reveal name, copy/paste-compatible policy, Enter submission, keyboard reachability, responsive Pixel 7 rendering, saved light/dark theme behavior, reduced-motion browser preference, and zero serious/critical axe violations in the tested desktop light/dark and phone signup states after the contrast fix. Static source review covered heading/shell semantics, dialog use, existing focus conventions, draggable alternatives, and campaign label warnings.

Not verified: screen-reader announcements, 400% zoom for the changed dialog in every browser, focus not obscured by every virtual keyboard, native safe areas/orientation/enlarged text, VoiceOver/TalkBack, all route contrasts, and full WCAG conformance. Automated axe success is not a conformance claim. Existing two campaign input labels remain open.

## 10. Performance and measurement

Artifact: `artifacts/ux-audit-2026-09-05/performance/login-lab.json`.

Five fresh Pixel 7-profile runs per exact-baseline/current production build used Chromium, 4× CPU throttling, 150 ms latency, 200,000 B/s download, 93,750 B/s upload, `es-EC`, and `America/Guayaquil`.

| Metric, median (min–max) | Baseline | After | Target / interpretation |
| --- | --- | --- | --- |
| FCP | 3,240 ms (3,188–3,504) | 3,296 ms (3,276–3,312) | No material improvement established |
| LCP | 4,324 ms (4,232–4,604) | 4,360 ms (4,336–4,388) | Both miss the ≤2.5 s target |
| CLS | 0.0385 | 0.0385 | Both meet ≤0.1 in this lab |
| DOMContentLoaded | 2,744 ms | 2,761 ms | Effectively unchanged |
| Load | 5,013 ms | 5,000 ms | Effectively unchanged |

This is laboratory evidence, not representative field p75 data. INP was not measured because the script did not perform a representative interaction. No conversion or performance uplift is claimed. Final production build passed its bundle guard with 5 preloads and 411,487 gzip bytes of initial JavaScript, while Vite still warns that the MUI chunk is over 500 kB minified. Highest-value performance work is route-shell/critical CSS and vendor/chunk analysis using a controlled profile, followed by field measurement when consented RUM is available.

Analytics events remain semantically distinct: signup start/failure/completion, login, intent selection, and first actions are not conflated. The batch removes personal identity properties and credential-bearing query values; it does not invent baseline conversion rates or launch an experiment.

## 11. Implementation inventory

### Backend security

- `tdf-hq/src/TDF/Email.hs`
- `tdf-hq/src/TDF/Email/Service.hs`
- `tdf-hq/src/TDF/ServerAuth.hs`
- `tdf-hq/test/Spec.hs`

### Web onboarding, routing, privacy, and tests

- `tdf-hq-ui/src/pages/LoginPage.tsx`
- `tdf-hq-ui/src/pages/ResetPasswordPage.tsx`
- `tdf-hq-ui/src/pages/DistributionLandingPage.tsx` and test
- `tdf-hq-ui/src/pages/TdfPlatformPage.tsx` and test
- `tdf-hq-ui/src/api/auth.ts` and test
- `tdf-hq-ui/src/utils/passwordPolicy.ts` and test
- `tdf-hq-ui/src/analytics/posthog.ts`
- `tdf-hq-ui/src/session/SessionContext.tsx`
- `tdf-hq-ui/src/__tests__/analytics.test.ts`
- `e2e/web/persona-public.spec.mjs`
- `docs/analytics.md`

### Native mobile

- Parent `tdf-mobile` pointer to published mobile commit `10d5dc9e2a733c9c61b5b5f288d6cdfc28a2e623`
- Mobile `src/providers/AuthProvider.tsx` and test
- Mobile `app/ticketCheckout.tsx` and test

## 12. Screenshots and artifacts

All screenshots are actual local browser renders using synthetic/redacted fixtures.

Baseline at `b62ccaa…`:

- `artifacts/ux-audit-2026-09-05/baseline/login-desktop-1440x900.png`
- `artifacts/ux-audit-2026-09-05/baseline/signup-events-pixel7.png`
- `artifacts/ux-audit-2026-09-05/baseline/signup-artist-desktop-1440x900.png`

After:

- `artifacts/ux-audit-2026-09-05/after/signup-events-pixel7.png`
- `artifacts/ux-audit-2026-09-05/after/signup-artist-desktop-1440x900.png`

The baseline Pixel 7 dialog required users to process optional marketing and unrelated artist selection before the account fields. The after image keeps core fields, required legal acceptance, a contextual next task, and an exit. Desktop artist intent retains the eligible-profile claim selector.

## 13. Verification record

| Executed command/scope | Result | What it proves / does not prove |
| --- | --- | --- |
| `npm run ai:doctor` | 16 OK, 2 warnings | Workflow prerequisites checked; original dirty checkout and invalid `gh` auth remain. |
| Focused web Jest: analytics, auth, distribution, TDF CTA, password policy | 5 suites / 18 tests passed | Changed utility/API/privacy contracts pass with mocks; not live providers. |
| Full `persona-public` Playwright run on desktop + Pixel 7 | 11 passed / 1 intentionally skipped duplicate desktop reflow check | Protected-route return, rejected login, signup, recovery failure, discovery/back-state, phone 320px reflow, and truthful ticket hold with synthetic HTTP fixtures. |
| Final dark/light signup Playwright regression | 3 passed / 1 intentionally skipped phone duplicate light-theme check | Current theme-token implementation has no serious/critical axe violation in tested desktop light/dark and phone dark states. |
| `npm run typecheck:ui` | Passed | Current web TypeScript contract compiles. |
| `npm run lint:ui` | Completed with zero errors and 102 existing warnings | No lint error; warnings are not hidden. |
| `npm run build:ui` | Passed; 5 preloads / 411,487 gzip bytes initial JS | Current production bundle builds and passes repository guard; not a deployed build. |
| `npm run audit:ui:static -- --exit-zero` | 0 errors / 3 warnings | Static heuristic only; two campaign labels remain. |
| Mobile focused Jest | 2 suites / 31 tests passed | Revoked-session and anonymous-checkout cases pass with mocks. |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run test:mobile` | 64 suites / 316 tests passed | Full accessible native Jest workspace ran; not a simulator/device or live backend. |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run typecheck:mobile` | Passed | Mobile TypeScript compiles. |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run lint:mobile` | Passed, zero warnings | Mobile lint passes. |
| `stack test --test-arguments='--match accountCreatedEmailContent'` | Changed backend and app executable compiled/linked; the command was stopped while rebuilding the full test executable at 22/183 modules | The Hspec match scopes execution, not compilation. It did not execute the target and is reported as skipped/incomplete, not passed. |
| `stack exec -- ghc -isrc src/TDF/Email.hs -e …` credential sentinel invariant | Passed: `PASS accountCreatedEmailContent credential invariant` | Current email content excludes temporary-password phrase, token, and email sentinel; this is a focused executable invariant, not SMTP delivery or the full Hspec suite. |
| Baseline/current Playwright performance script | 10 total successful runs | Comparable local lab timings only; no p75 field proof and no INP. |

The full current web Jest suite was not rerun to green because the exact baseline already established 168 failures in one pre-existing admin suite. No tests were disabled or assertions weakened.

## 14. Prioritized remaining backlog

1. **Revenue continuity:** replace the legacy public-booking calendar CTA with customer-safe order tracking. Dependency: synthetic legacy order fixture and capability-aware route. Acceptance: Customer completes booking and never sees a forbidden destination.
2. **Durable onboarding:** create versioned, account-bound progress/intent state. Dependency: minimal API/database contract and privacy retention decision. Acceptance: cross-device persistence, account isolation, optional skip, idempotent completion.
3. **Useful-action resume:** idempotently resume follow/contact after authentication. Dependency: durable intent from item 2. Acceptance: one explicit result, no duplicate follow/contact, safe cancellation.
4. **Browser draft privacy:** remove or expire PII-bearing marketplace/booking local storage. Dependency: product decision on interrupted-draft retention. Acceptance: logout/account switch/expiry clears personal data.
5. **Mobile route continuity:** preserve safe queries and normalize supported double-slash deep links. Dependency: navigation integration fixtures and device validation. Acceptance: exact event/service context after auth without external redirect capability.
6. **API truth:** correct OpenAPI role/nullability/requiredness and regenerate both clients. Dependency: coordinated compatibility run. Acceptance: canonical schema and runtime models agree.
7. **Language and legal coherence:** route all auth text through current catalogs and publish reviewed Spanish/English legal pages. Dependency: legal/content approval; no policy rewrite by engineering. Acceptance: no mixed-language supported flow.
8. **Accessibility follow-up:** label two campaign inputs and perform native device/screen-reader, virtual-keyboard, reflow, and focus-obscuring checks. Acceptance: static warnings removed plus documented manual outcomes.
9. **Performance:** profile the login shell/vendor waterfall and split noncritical work; establish consented field Web Vitals at p75. Acceptance: comparable lab LCP ≤2.5 s without CLS/INP regression, then separate field evidence.
10. **Experiment integrity:** keep onboarding experiment paused until account-bound eligibility/exposure/completion exists. Acceptance: signup versus returning-device sign-in is unambiguous and events are idempotent.

## 15. Task-based usability script (not conducted)

Use synthetic/staging accounts and record task success, confusion, recovery, and qualitative comments. Do not coach unless the participant is blocked.

### Stuart / product owner

1. From `/tdf`, explain the platform and choose the most appropriate account path.
2. Create a fictional general account and identify the first useful action.
3. Refresh/sign out/sign in and explain what resumed versus repeated.

### Staff member

1. Sign in through a protected internal link and confirm the intended route resumes.
2. Select a person by name/username/avatar without using a Party ID.
3. Reorder a ranked item using drag, then using Move up/Move down with pointer and keyboard.

### Artist

1. Arrive from the artist-profile CTA, create an account, and claim an eligible synthetic profile.
2. Explain whether access is immediate, pending, or requires a governed request.
3. Recover from an unavailable/already-claimed profile without losing the account path.

### Customer/fan

1. Explore a synthetic artist/event without signing in.
2. Begin ticket/service intent, authenticate only when required, and confirm the original context remains.
3. Simulate a recovery network failure, retry, and identify when the operation truly succeeded.

No participant was contacted and no session, quote, completion rate, or usability statistic is claimed.

## 16. Branch, publication, and pull-request handoff

- Root branch: `feature/onboarding-first-ux-20260904`; implementation commits `aba01d1f8` (email/log security), `8d62611de` (web onboarding/privacy), and `86e234045` (published mobile pointer). The report/artifact commit and remote link are recorded in the final handoff.
- Mobile branch: `feature/onboarding-first-ux-20260905`; commits `3c132ff` and `10d5dc9`; remote commit `10d5dc9e2a733c9c61b5b5f288d6cdfc28a2e623` is published.
- No PR was created because GitHub CLI authentication is invalid. No merge or deployment occurred.
- Ready-to-use PR text is in `reports/onboarding-first-ux-pr-description-2026-09-05.md`.

`origin/main` advanced from the captured baseline to `ca64c3cde` (16 commits) after implementation and validation. The feature branch was deliberately not rebased after the evidence run: the new upstream changes do not overlap this batch's implementation files, but rebasing would make the recorded build/test commit graph inaccurate without another full regression pass. The draft-PR reviewer should update the branch and rerun CI before merge.
