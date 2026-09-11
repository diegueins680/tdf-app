# TDF Domo fallback browser and accessibility audit — 2026-09-10

## Outcome

This bounded revenue-onboarding batch closes the highest-priority browser gap left by `reports/public-booking-http-concurrency-audit-2026-09-09.md`. An anonymous prospective customer can arrive directly at `/domo-del-pululahua`, understand the available experiences without creating an account, send the supported manual request when authoritative quote checkout is unavailable, recover from an ambiguous transport failure, and receive a truthful confirmation that does not claim a date hold or payment.

Runtime inspection exposed four current defects:

1. the no-authoritative-quote fallback interpreted an Ecuador local date-time as UTC unless a Domo-only build variable happened to be configured, shifting the submitted instant by five hours;
2. the contact fields omitted password-manager/browser autocomplete semantics;
3. axe found one serious color-contrast rule affecting 20 rendered nodes across the four Domo themes;
4. the full-screen hero video autoloaded and autoplayed without a user control.

The fallback now defaults to `America/Guayaquil`, preserves one idempotency key and identical payload across a retry, adds contact autocomplete, uses contrast-safe theme values, and presents a poster-first video with an explicit play/pause control. The video has no autoplay and `preload="none"`, so the tested page made zero `/videos/` requests before the visitor opted in.

No price, tax, deposit, cancellation, payment, quote formula, availability rule, permission, role, schema, API contract, generated client, backend, native-mobile, dependency, analytics, or production-data behavior changed.

## Capability and safety matrix

| Capability | Status | Evidence from an actual check | Consequence |
|---|---|---|---|
| Repository read/write | Available | Isolated worktree `/private/tmp/tdf-event-save-publish-20260907` was clean at baseline and accepted focused source/test commits | The heavily modified primary checkout was not touched |
| Dedicated branch/baseline | Available | Branch `feature/domo-fallback-browser-runtime-20260910` was created from exact PR #324 head `8ccc3b6d1deedf6547994c2601bbbc438ec54be7` | Review can be stacked without mixing unrelated work |
| Repository instructions | Available | Root `AGENTS.md`, `AI_WORKFLOW.md`, `CONTRIBUTING.md`, `docs/venue-manager/AGENTS.md`, `tdf-hq/AGENTS.md`, package scripts, and invoked wrappers were inspected | Current commands and scoped safety rules governed execution |
| Preflight | Available with warnings | `REQUIRE_MOBILE_WORKSPACE=1 npm run ai:doctor`: 15 checks OK, 3 warnings, 0 errors | Work could proceed; absent Sep 9/10 isolated-worktree memory files and stored GitHub auth were not treated as product failures |
| Web runtime/toolchain | Available | Node `v24.8.0`, npm `11.6.0`, Playwright `1.59.1`; Vite production build transformed 12,415 modules | Source, unit, production-build, and real-browser checks were possible |
| Browser/device classes | Available | Chromium desktop, Pixel 7-like phone, and tablet projects executed the Domo journey; configured critical Firefox/WebKit tests ran in the broader suite | Responsive runtime evidence and screenshots are available; Domo itself was not run in Firefox/WebKit |
| Screenshot/visual inspection | Available | Three final full-page PNGs were captured by Playwright and manually inspected | Final layout evidence is real local runtime evidence, not a mockup or production screenshot |
| Backend/PostgreSQL | Available but not invoked in this batch | Docker client/server and Stack were available; PR #324 separately exercised the real compiled booking handler and PostgreSQL 17 | This browser test deliberately isolates UI behavior with mocked HTTP responses; no combined browser/backend claim is made |
| Mobile submodule | Available, unaffected | Required preflight found the initialized mobile workspace at `57abd23c…`; no native Domo/public-booking screen or contract changed | No parent pointer or native code was changed; native UI was not rerun |
| Local/staging configuration | Partial | Local Vite test configuration and synthetic routes were usable; no approved staging fixture or Domo quote provider was available | Local journeys are verified; staging/provider behavior remains unverified |
| Synthetic accounts/data | Available for tested public route | Playwright used fictional `persona.test` contact data and deterministic service/storefront responses; the route is anonymous | No real account, booking, payment, or customer data was used |
| Analytics/field performance | Unavailable | No representative consented telemetry or analytics access was present | Conversion, abandonment, p75 Web Vitals, and production impact are “not yet measured” |
| Network | Partial | Authoritative W3C documentation and current GitHub metadata were reachable; product API calls were intercepted locally | Standards and repository state could be verified without touching production services |
| GitHub | Available in current session; stored auth partial | Current `gh` access worked while preflight reported the stored login as invalid after environment tokens were cleared | Push/draft PR can proceed only after automation safety inspection; credentials are not logged here |
| Production/external communications | Disabled by scope | No production endpoint, live database, payment provider, mail, WhatsApp, or deployment command was invoked | Nothing here is production validation or rollout |

## Baseline and methodology

- Baseline commit: `8ccc3b6d1deedf6547994c2601bbbc438ec54be7`.
- Implementation commit: `4ca0bdeb4fabbcdb7a280f462ae9ffea87f424f6`.
- Environment: macOS host, local Vite server, Playwright browser engines, synthetic public service/storefront/booking responses, Spanish-visible Domo route.
- Methods: source and contract trace, historical-gap revalidation, cognitive walkthrough from direct arrival to first useful action, controlled red/green unit tests, real-browser task execution, keyboard and pointer operation, network observation, 320 CSS-pixel reflow check, automated axe checks, manual screenshot inspection, comparable production builds, and repository regression gates.
- Evidence boundary: expert assessment and synthetic runtime evidence only. No user interview, usability session, analytics uplift, external provider, real booking, staging, or production result is claimed.

The WCAG target remains Level AA. The relevant normative thresholds are W3C [WCAG 2.2 SC 1.4.3](https://www.w3.org/TR/WCAG22/#contrast-minimum) for 4.5:1 normal text and 3:1 large text, [SC 1.4.10](https://www.w3.org/TR/WCAG22/#reflow) for 320 CSS-pixel reflow, and [SC 2.5.8](https://www.w3.org/TR/WCAG22/#target-size-minimum) for the 24 CSS-pixel minimum/spacing rule. TDF's 44–48 pixel product aim is intentionally stronger than the conformance minimum where feasible.

## Coverage matrix

| Route/state | Role | Device/runtime | Inspection method | Status |
|---|---|---|---|---|
| `/domo-del-pululahua#cotizar`, direct public arrival | Anonymous prospective customer | Chromium desktop/phone/tablet | Full browser journey and screenshots | **Verified locally**; no authentication gate |
| Storefront has no approved authoritative checkout; event-production service exists | Anonymous prospective customer | Chromium desktop/phone/tablet | Synthetic API fixture plus visible form assertions | **Verified**; manual request is offered without invented price |
| First manual request returns ambiguous `503` | Anonymous prospective customer | Chromium desktop/phone/tablet | Keyboard submit, response assertion, enabled retry | **Verified**; error remains visible and retry is available |
| Retry returns `200` | Same customer | Chromium desktop/phone/tablet | Pointer retry, captured request bodies/headers, visible status | **Verified**; payload and idempotency key are identical, and confirmation disclaims hold/payment |
| Ecuador local input `2030-01-15T10:00` | Anonymous prospective customer | Jest and real Chromium | Exact payload assertion | **Verified** as `2030-01-15T15:00:00Z`, duration 480 minutes |
| Name/email/WhatsApp data entry | Anonymous prospective customer | Jest and Chromium desktop/phone/tablet | DOM attribute assertions | **Verified**: `name`, `email`, and `tel` autocomplete tokens |
| Nature theme, final success state | Anonymous prospective customer | Chromium desktop/phone/tablet | axe serious/critical scan | **Verified**: zero serious/critical violations in tested state |
| Eventos, Música, Ceremonias themes | Anonymous prospective customer | Chromium desktop | Theme selection plus axe scan after each transition | **Verified**: zero serious/critical violations in tested states |
| Hero media before opt-in | Anonymous visitor | Chromium desktop/phone/tablet | Request observer, DOM semantics, source/unit assertions | **Verified**: no `/videos/` request, no autoplay, `preload="none"`, 44px control |
| Hero media play/pause control | Anonymous visitor | Chromium desktop/phone/tablet | Browser click/state test with deterministic media-method shim | **Verified at UI event/state layer**; real decoding/playback unverified |
| 320 CSS-pixel route | Low-vision/mobile-web visitor | Chromium phone project | Resize plus document/client width comparison and axe scan | **Verified**: no horizontal overflow in tested state |
| Authoritative quote API failure | Anonymous prospective customer | Jest/jsdom | Existing quote-mode failure test | **Verified**: no false hold/payment claim |
| Loading/service-empty states | Anonymous prospective customer | Source and existing unit setup | Static trace, existing no-service fixture | **Partially inspected**; not separately screenshot-tested here |
| Offline, expired session, back/refresh after submission | Anonymous prospective customer | Browser/network | Not exercised | **Unverified**; route is public, but post-submit resilience still needs a task test |
| Real handler/PostgreSQL combined with browser | Anonymous prospective customer | Local integration | Not connected in this batch | **Deferred**; complementary backend evidence exists in PR #324 |
| Firefox/WebKit Domo path | Anonymous prospective customer | Browser | Not selected by current critical tags | **Untested**; critical auth/internship subset passed in both engines |
| Native mobile Domo journey | Any | Native | No corresponding native route found in current scope | **Not applicable to changed surface / not runtime-tested** |
| Staging, production, quote provider, analytics | Customers/operators | External systems | Not invoked | **Skipped by scope/safety/capability** |

## Historical finding revalidation

Historical reports remain unchanged.

| Historical finding/gap | Current classification | Evidence |
|---|---|---|
| `public-booking-http-concurrency-audit-2026-09-09`: Domo fallback browser retry remained source/type-only | **Superseded for isolated browser runtime** | New desktop/phone/tablet test observes two POSTs with one key/payload and truthful retry/success states |
| `public-booking-idempotency-conflict-audit-2026-09-09`: fallback must never claim hold/payment | **Verified at browser UI layer** | Visible error/success copy and whole-body negative assertions pass; backend replay integrity remains separately verified in PR #324 |
| `ux-ui-audit-2026-08-05`: web lint had six errors including Domo | **Resolved** | Current lint completes with 0 errors; 102 existing warnings remain outside this diff |
| `ux-ui-audit-2026-08-05`: autoplay hero motion lacked a pause/reduction policy on the TDF landing route | **Resolved on that historical route; analogous Domo regression newly found and fixed here** | Domo baseline had `autoPlay` plus `preload="auto"`; final route is poster-first and user-controlled |
| Historical contrast findings | **Not repeated as current defects** | The Domo 20-node failure was produced by this batch's current browser inspection, not copied from an earlier report |

## Findings and implementation status

### DOMO-TZ-01 — fallback submitted Ecuador wall time as UTC

- Journey/role: anonymous prospective customer requesting a Domo event when authoritative quote checkout is unavailable.
- Reproduction: omit `VITE_DOMO_TIMEZONE`, enter `2030-01-15T10:00`, and inspect `pbStartsAt`.
- Expected: the fixed-location Domo default interprets the wall time in Ecuador and submits `2030-01-15T15:00:00Z`.
- Baseline actual: `2030-01-15T10:00:00Z`, five hours earlier.
- Evidence/severity/confidence: controlled unit regression and source trace; **high severity**, **high confidence**. Production frequency is unknown.
- Likely cause: the route's Domo-only environment fallback was `UTC`, while documented Ecuador defaults did not guarantee that variable in deployed builds.
- Remedy/effort/dependencies: default the venue-specific fallback to `America/Guayaquil`; small, no schema or contract dependency.
- Acceptance: exact UTC payload assertion passes with no Domo build variable, and authoritative storefront timezone remains authoritative when checkout is available.
- Status: **implemented and verified locally**.

### DOMO-AUTO-01 — contact fields omitted browser identity assistance

- Journey/role: new/prospective customer completing the first revenue action.
- Reproduction: inspect the rendered Nombre, Correo, and WhatsApp inputs.
- Expected: supported autocomplete tokens help browser autofill and assistive input tools without storing credentials in app code.
- Baseline actual: all three attributes were absent.
- Evidence/severity/confidence: failing unit assertion plus browser DOM inspection; **medium severity**, **high confidence**.
- Likely cause: MUI fields were labeled but autocomplete metadata was never set.
- Remedy/effort/dependencies: add `name`, `email`, and `tel`; trivial, no backend dependency.
- Acceptance: unit and all three Chromium device projects observe exact tokens.
- Status: **implemented and verified locally**.

### DOMO-CONTRAST-01 — theme colors failed minimum text contrast

- Journey/role: any visitor reading discovery, offer, location, and footer content.
- Reproduction: render the four Domo experience themes and run axe against the page.
- Expected: normal text meets 4.5:1 and large text meets 3:1 under WCAG 2.2 SC 1.4.3.
- Baseline actual: one serious `color-contrast` rule affected 20 nodes. Examples included 2.40:1 info-band numbers, 3.37:1 Experiencias overline, 2.56:1 card descriptions, 1.81:1 Ceremonias label, 1.65:1 Naturaleza dark-section label, 1.58:1 coordinate accent, and 2.65:1 footer text.
- Evidence/severity/confidence: real Chromium/axe diagnostics across desktop, phone, and tablet; **serious accessibility impact**, **high confidence** for rendered tested states.
- Likely cause: translucent/theme accent colors were reused across incompatible light, white-card, and near-black backgrounds.
- Remedy/effort/dependencies: retain brand accents for borders/actions while adding explicit readable foregrounds and per-theme dark-background accents; small, no new design system.
- Acceptance: default theme passes on three device classes; all four themes pass on desktop; screenshots retain recognizable Domo identity.
- Status: **implemented and automated/browser-verified**. This is not a full-page WCAG conformance claim.

### DOMO-MEDIA-01 — hero video autoloaded/autoplayed without visitor control

- Journey/role: any direct visitor, especially motion-sensitive or data-constrained mobile visitors.
- Reproduction: load the route at baseline and inspect the hero video attributes/network behavior.
- Expected: useful poster content appears first; motion and transfer begin only after an explicit action; the visitor can pause it.
- Baseline actual: `autoPlay`, `loop`, and `preload="auto"` were set with no pause control. The selected nature files are 1,090,031 bytes desktop and 1,092,300 bytes mobile on disk.
- Evidence/severity/confidence: source/runtime red test (play control absent), local asset sizes, and final request observation; **medium-high impact**, **high confidence**. Actual transferred bytes and user impact in the field are not measured.
- Likely cause: the immersive hero was implemented as unconditional background media rather than progressive enhancement.
- Remedy/effort/dependencies: no autoplay, `preload="none"`, poster-first rendering, accessible 44px play/pause button, and state reset when changing experiences; small, existing MUI/icons only.
- Acceptance: no hero video request before opt-in; play/pause labels and `aria-pressed` state transition; three device screenshots show no overlap; axe remains clear of serious/critical findings.
- Status: **implemented and UI-event/browser-verified**. Real media decoding and failure feedback remain unverified.

## Onboarding and product rationale

For this public acquisition route, the useful first action is not account creation; it is understanding whether the venue fits the visitor's intent and submitting a supported request. The implementation therefore preserves account-free discovery and the `#cotizar` destination. It does not introduce a generic welcome wizard or demand profile fields unrelated to the request.

When authoritative quote checkout is unavailable, the UI uses the published event-production service, makes the no-hold/no-payment boundary explicit, and retains the same safe idempotency identity after an ambiguous failure. Product intent remains request context only; no privilege or role is assigned. The existing authoritative quote flow remains unchanged and continues to use the storefront's returned timezone and versioned commercial terms.

The visual changes are deliberately route-scoped: brand colors remain in navigation, borders, images, and actions, while semantic foreground variants handle white and near-black surfaces. No parallel component library or rebrand was introduced.

## Performance evidence

Comparable production builds used the same host/toolchain:

| Measure | Baseline `8ccc3b6d…` | Implementation `4ca0bdeb…` | Interpretation |
|---|---:|---:|---|
| Initial JS gzip budget | 413,040 bytes | 413,021 bytes | Essentially unchanged; 19-byte variation is not treated as a meaningful user improvement |
| Lazy Domo page chunk | 33.91 kB / 10.94 kB gzip | 34.64 kB / 11.24 kB gzip | About +0.73 kB raw / +0.30 kB gzip for media control and safeguards |
| Hero video request before opt-in | Baseline source forced autoplay/preload; request count not retained as a comparable artifact | 0 in each tested Chromium device project | The final route does not request a 1.09 MB nature video before consent; transferred-byte savings are not claimed without a measured baseline waterfall |

The build budget passed with five preloads. No Lighthouse/WebPageTest run, CPU/network throttle profile, p75 field LCP/INP/CLS, cache analysis, or production waterfall was available, so the targets LCP ≤2.5 s, INP ≤200 ms, and CLS ≤0.1 remain **not yet measured** for this route.

## Verification performed

- Controlled autocomplete regression: **failed as intended** before the fix (`expected "name", received null`).
- Controlled timezone regression with the old UTC fallback: **failed as intended** (`expected 2030-01-15T15:00:00Z`, received `2030-01-15T10:00:00Z`).
- Initial Domo Playwright accessibility run: **failed on all three Chromium device projects** with one serious contrast rule affecting 20 nodes; detailed target/HTML/failure summaries were used to fix the route.
- Media-control regression before implementation: **failed as intended** because `Reproducir fondo` did not exist.
- `npm test --workspace=tdf-hq-ui -- --runTestsByPath src/pages/DomoVenuePage.test.tsx`: **passed**, 1 suite / 3 tests.
- Focused Domo browser matrix: **passed**, 3/3 on Chromium desktop/phone/tablet in 33.5 seconds after the final media change.
- `npm run test:e2e:web`: **passed**, 38 tests / 4 configured skips in 3.0 minutes after final code. This includes all Chromium public/persona and internship tests plus the configured critical Firefox/WebKit subset.
- `npm run lint --workspace=tdf-hq-ui`: **passed with 0 errors and 102 pre-existing warnings**; no warning is in the changed Domo page.
- `npm run build --workspace=tdf-hq-ui`: **passed**, TypeScript no-emit check plus Vite production build, 12,415 modules, bundle budget 413,021 gzip bytes initial JS. Existing >500 kB chunk advisory remains.
- `npm run quality:repo`: **passed** after final code: generated-audit diff guard; 7 internship tests; formal verification (0 critical/errors, 314 warnings); 42 improvement-loop tests; 4 formal-audit tests; 49 production-release tests; 18 CI-pipeline tests; 2 visual-artifact tests; and 3 persona-program tests.
- `npm run test:catalog-list-audit`: **passed**, 1/1.
- `npm run audit:catalog-lists`: **passed**, no unreviewed candidate.
- `git diff --check` and staged diff check: **passed** before the implementation commit.

### Explicitly failed/limited checks

`npm run quality:ui` is **not green and is not reported as passed**. Its lint and typecheck stages passed; Jest then reported 185/186 suites passing and 1,596/1,764 tests passing. The baseline-identical `CourseRegistrationsAdminPage.test.tsx` hit its hard five-second timeout in `strips common form-provider descriptors from first-run cohort copy`, after which 167 related assertions cascaded; the Domo suite passed inside the full collection. Running that first course test alone with 525 siblings skipped reproduced the same timeout. Neither the course test nor its product page differs from baseline in this branch. Because Jest failed, the wrapper did not reach build; the production build was run separately and passed.

No test was disabled, assertion weakened, timeout raised, skipped collection counted as a pass, or mocked response described as backend/provider verification.

## Screenshots and artifacts

Final Playwright screenshots, all from local Vite plus synthetic intercepted APIs and manually inspected:

- Desktop: `artifacts/persona-playwright/test-results/persona-public-PW-PER-01-D-46221-ual-request-in-Ecuador-time-chromium-desktop/domo-manual-request-safe.png`
- Phone: `artifacts/persona-playwright/test-results/persona-public-PW-PER-01-D-46221-ual-request-in-Ecuador-time-chromium-phone/domo-manual-request-safe.png`
- Tablet: `artifacts/persona-playwright/test-results/persona-public-PW-PER-01-D-46221-ual-request-in-Ecuador-time-chromium-tablet/domo-manual-request-safe.png`

Playwright attachments also contain the final axe JSON. These runtime artifacts are ignored local evidence and are not presented as production captures. Comparable pre-change screenshots were not retained; pre-change evidence is the controlled failing assertions and axe diagnostics, not a reconstructed image.

## Incremental usability-testing script

No participant was contacted and no session is claimed. Use synthetic data and a local/staging environment:

1. **Customer/prospective visitor:** open a shared `#cotizar` URL, explain in one sentence what can be requested, choose an experience, complete the request, encounter one simulated transport failure, and retry. Observe whether the person understands that no date or payment is confirmed.
2. **Artist:** arrive from a shared Domo link, compare two experiences, pause/play the hero media, and identify how to request a performance or production date without creating an unrelated account.
3. **Stuart:** inspect the resulting tentative request in the corresponding staff workflow and state what is known versus still unverified; do not use payment/availability assumptions.
4. **Staff/operations:** distinguish an authoritative Domo quote from a manual fallback booking, verify Ecuador time, and process a replay without duplicate outreach.

For every task, check keyboard-only operation, visible/unobscured focus, 200% text zoom, 320px reflow, one portrait/landscape mobile pass, screen reader announcements for error/success, and interruption/refresh behavior. Record observed issues; do not infer conversion results from completion in a moderated test.

## Changed files

- `tdf-hq-ui/src/pages/DomoVenuePage.tsx` — Ecuador fallback, user-controlled media, accessible form metadata, and contrast-safe route theme values.
- `tdf-hq-ui/src/pages/DomoVenuePage.test.tsx` — exact timezone, retry/key, truthful-copy, autocomplete, and media-loading regression.
- `e2e/web/persona-public.spec.mjs` — desktop/phone/tablet Domo task, response retry, request capture, network/media observation, reflow, touch target, screenshots, and richer axe diagnostics.
- this report and `reports/domo-fallback-browser-pr-description-2026-09-10.md`.

## Deferred work and acceptance criteria

1. **Combined browser/backend/PostgreSQL test** — impact: UI and backend guarantees are currently complementary rather than one process graph; dependency: deterministic local stack fixture; acceptance: the browser sends the two retries to the compiled handler and disposable PostgreSQL, receives one booking, and database cardinality remains one.
2. **Real media playback/failure** — impact: UI event logic is tested with a media shim but codec/network failure feedback is not; dependency: deterministic small valid media fixture; acceptance: opt-in starts actual playback, pause stops it, source change resets it, and decode failure leaves a clear retry/static-poster state.
3. **Manual accessibility** — impact: automated scans cannot prove WCAG conformance; dependency: human/device access; acceptance: screen-reader, focus order/visibility, 200% zoom, orientation, virtual keyboard, and physical touch checks complete the task without obstruction.
4. **Domo cross-browser expansion** — impact: Firefox/WebKit do not run this non-critical-tagged path; dependency: test budget; acceptance: Domo journey and axe assertions pass in those engines or documented engine defects have fallbacks.
5. **Offline/refresh/back continuity** — impact: interrupted anonymous requests may still confuse customers; dependency: product decision on durable non-sensitive draft/receipt recovery; acceptance: no duplicate booking, cross-account leakage, or false success after interruption.
6. **Authoritative quote provider/staging** — impact: only fallback and mocked quote failure are verified in this batch; dependency: approved sandbox provider and fixtures; acceptance: quote, hold expiry, cancel/interruption, and checkout states use versioned server truth without real charge.
7. **Measurement** — impact: booking abandonment, retry frequency, and field performance remain unknown; dependency: privacy/consent and analytics access; acceptance: existing taxonomy records unique funnel milestones (not clicks), excludes contact/free text, deduplicates completion, and reports consented field p75 separately from lab data.
8. **Course-registration test timing debt** — impact: the complete UI wrapper remains red despite 185 passing suites; dependency: focused test-suite refactor, not a timeout increase; acceptance: isolate provider-label normalization as fast unit/property tests and keep page integration coverage within the existing limit without hiding failures.

## Handoff status

- Branch: `feature/domo-fallback-browser-runtime-20260910`.
- Stacked base: `feature/public-booking-http-concurrency-20260909` at `8ccc3b6d1deedf6547994c2601bbbc438ec54be7`.
- Implementation commit: `4ca0bdeb4fabbcdb7a280f462ae9ffea87f424f6`.
- Initial evidence/report commit: `09b205d820e82f0a1b855aea9547eaad5108dc47`.
- Draft PR: https://github.com/diegueins680/tdf-app/pull/327.
- Hosted exact-head CI: triggered by the final handoff update; final status is recorded in the execution handoff.

No merge, deployment, production mutation, real transaction, or customer communication was performed.
