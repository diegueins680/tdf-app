# TDF public-booking guest continuity audit and implementation — 2026-09-09

## Outcome

This bounded revenue/onboarding batch extends the consolidated onboarding audit after the cross-device work in `reports/onboarding-cross-device-reconciliation-audit-2026-09-09.md`. It fixes three linked defects in the no-quote public-booking journey:

1. `POST /bookings/public` no longer creates an undisclosed `UserCredential`, random password, and automatic customer security policy for an anonymous customer. It creates or reuses only the customer `Party`, matching the already-correct quote-backed checkout path.
2. Optional login and signup now retain the exact safe booking path, service/campaign query, and hash instead of returning to a generic route.
3. The form and receipt no longer promise automatic account creation or customer email confirmation that the backend does not perform. They explain that an account is optional and direct the customer to retain the real booking ID and use the existing WhatsApp contact action for adjustments.

The booking remains available without authentication. No price, deposit, cancellation rule, role, permission, payment state, feature flag, database schema, outbound customer communication, deployment, or production data changed. The paused onboarding experiment remains paused.

## Capability and safety matrix

| Capability | Status | Evidence from an actual check | Consequence |
|---|---|---|---|
| Repository read/write | Available | Five implementation/test files, browser artifacts, and this report were changed only in `/private/tmp/tdf-event-save-publish-20260907` | The primary dirty checkout was not modified |
| Dedicated branch and baseline | Available | Branch `feature/public-booking-truthful-guest-continuity-20260909`; clean starting commit `c433dda0be0f6e29330cf559a3e3d7cf3e892117` | This batch can be reviewed as a stack on the prior onboarding branch |
| Unrelated-work protection | Available | Initial isolated-worktree `git status --short --branch` was clean; the root workspace was not used for edits | No known unrelated change is included |
| Required runtimes and test runners | Available | Node `v24.8.0`, npm `11.6.0`, Stack `3.7.1`; Jest, TypeScript, ESLint, Hspec, and Playwright commands executed | Focused web, browser, and backend checks are supported |
| Backend/database | Partial | Backend compiles against the local Stack project; the regression uses an isolated in-memory SQLite schema | Guest identity persistence is testable locally; PostgreSQL concurrency and production data are not validated |
| Browser/device emulation | Available | Playwright ran Chromium desktop and the repository's Pixel 7 profile against a local Vite server | The customer journey and responsive receipt were exercised in real browser engines, not generated mockups |
| Screenshot capability | Available | Playwright wrote two `booking-customer-safe.png` files, and both were visually inspected | Comparable post-submit desktop/phone evidence is available |
| Native mobile workspace | Available but unaffected | Submodule remains at published commit `f487de478939b3c19a62b5f54aa876d96a4eb32c`; source search found only generated web-route registry entries for `/reservar` | There is no native public-booking screen to change in this batch; the mobile pointer remains untouched |
| Local/staging configuration | Partial | Local UI ran with synthetic route fixtures; no staging backend, sandbox payment provider, SMTP sink, or synthetic authenticated customer was configured for this batch | HTTP assertions do not prove staging persistence, mail delivery, or payment behavior |
| Analytics access | Unavailable | `docs/analytics.md` and emitters were inspected; no booking funnel emitter exists and no PostHog project access was available | Booking conversion remains “not yet measured”; no uplift is claimed |
| Network/GitHub | Available for requested operations | `gh auth status` initially reported an invalid stored CLI token, but the actual SSH branch push and authorized `gh pr create --draft` both succeeded | Branch and draft PR are published; the contradictory status check remains documented rather than treated as proof of failure |
| Production access | Not used | No production URL, database, provider, communication, deployment, or live account was invoked | Findings and verification are local/source-based only |

The Playwright server initially failed to bind `127.0.0.1:4173` inside the filesystem sandbox (`EPERM`). The same scoped command was rerun with approved local-server permission and passed. An earlier invocation used the nonexistent project name `chromium-des`; it ran zero tests and is a command-shape error, not a product failure or pass.

The documented `npm run ai:doctor` preflight completed with **14 OK, 4 warnings, 0 errors**. Warnings reported absent dated memory-note files in this isolated worktree, the expected task-owned dirty state, and invalid stored GitHub CLI authentication. The later push and draft-PR operation nevertheless succeeded; both observations are retained.

## Baseline and method

- Baseline: root `c433dda0be0f6e29330cf559a3e3d7cf3e892117`; mobile `f487de478939b3c19a62b5f54aa876d96a4eb32c`.
- Environment: macOS, America/Guayaquil, local Vite, Chromium desktop and Pixel 7 emulation, synthetic 2030 booking data, mocked public HTTP responses, SQLite Hspec fixture.
- Methods: route/API/data-model source inspection, backend control-flow trace, cognitive walkthrough, copy and state audit, historical-finding revalidation, Jest interaction tests, SQLite-backed invariant test, Playwright task run, automated serious/critical axe scan, and manual screenshot inspection.
- Evidence labels: browser results are synthetic task-based runtime evidence; SQLite verifies the persistence helper; source tracing verifies which helper the endpoint invokes. No participant research, field analytics, staging transaction, screen-reader session, or production validation occurred.

A fresh pre-change screenshot was not captured before editing. The actual runtime files under `artifacts/ux-audit-2026-09-05/after/booking-customer-safe-{desktop,pixel7}.png` are retained as historical comparators and visibly contain the old email promise. They are not relabeled as this batch's baseline.

## Coverage matrix

| Route/API | Role | Device/state | Inspection method | Status |
|---|---|---|---|---|
| `/reservar?service=synthetic-studio-session&utm_source=persona-booking#horario` | Anonymous prospective customer | Chromium desktop; catalog loaded; available no-quote slot; success | Playwright with synthetic HTTP, axe, screenshot | Verified |
| Same route | Anonymous prospective customer | Pixel 7 emulation; same states | Playwright with synthetic HTTP, axe, screenshot | Verified |
| `/reservar?service=dj-booth-practice&utm_source=campaign#horario` | Anonymous prospective customer | DOM/router fixture; optional login/signup | Jest + MemoryRouter | Verified |
| `/dj-booth` | Anonymous prospective customer | Preset copy and canonical service selection | Source + existing Jest | Verified in unit fixture; not rerun in browser |
| `POST /bookings/public` | Anonymous customer | Valid no-quote request | Endpoint/helper source trace + compiled backend | Implemented; real HTTP/database integration not exercised |
| `ensurePartyRecord` | Anonymous guest-commerce identity | New contact; repeated normalized contact | Hspec + isolated SQLite | Verified: 1 example / 0 failures |
| `POST /bookings/public/checkout` and public order tracking | Anonymous customer | Authoritative quote; unpaid hold | Existing source/Jest regression remained in the focused 12-test suite | Verified with mocks; provider sandbox skipped |
| `/login` return from booking | Anonymous or returning customer | Safe internal path with query and hash | Jest + Playwright link contract | Verified; actual auth round-trip not performed in this batch |
| Native mobile | Any | Public booking | Source inventory | No native screen found; generated registry points to web route |
| Error/offline/slow/provider/expired-session states | Anonymous/returning | Browser | Existing source/tests only | Not rerun in this focused runtime batch |

## Historical finding revalidation

| Historical item | Current classification | Evidence |
|---|---|---|
| 2026-09-05 `REV-01`: no-quote receipt exposed staff-only calendar | Still resolved | Focused Jest and both browser runs confirm no “Ver mi reserva” link and no `/estudio/calendario` destination |
| 2026-09-05 `REV-01`: email remained as follow-up | Superseded | Backend trace shows legacy creation calls only `notifyEngineerIfNeeded`; no customer email is sent. Old receipt copy was therefore unsupported and is now removed |
| 2026-09-05 `PRIV-02`: booking contact draft moved out of persistent shared-device storage | Still resolved | Existing suite passed; no storage behavior changed |
| 2026-08-20 safe return-task recommendation | Previously partial; resolved for booking entry | Both optional auth links now include path, service/campaign query, and hash through the shared redirect sanitizer |
| Paused `single-feature-onboarding-v1` experiment | Unchanged | This batch does not read or modify experiment code or flags |

Historical reports remain unchanged.

## Findings and implementation status

### PB-GUEST-01 — anonymous booking created an inaccessible account

- Journey/role: no-quote public booking; anonymous prospective customer.
- Reproduction: submit a valid request to `POST /bookings/public`; follow `createPublicBooking` to `ensurePartyWithAccount`; observe credential generation, automatic customer policy assignment, discarded `(username, tempPassword)`, and only engineer notification.
- Expected/actual: guest booking should bind the commercial record to a Party without silently creating authentication state. Actual code created a credential the customer could not know or use.
- Evidence: backend source at the baseline, adjacent guest-commerce invariant comment, database schema, and SQLite regression; severity **high**, confidence **high**.
- Observed impact: inaccessible credentials and misleading account state are created for affected no-quote submissions. Frequency and number of historical records are unknown because production data was not queried.
- Cause: legacy public booking reused an account-provisioning helper intended for flows that can deliver credentials.
- Remedy/effort/dependencies: switch one endpoint call to the established `ensurePartyRecord` helper; small, no migration or client contract change.
- Acceptance: new/repeated guest booking identity resolves to one Party and zero credentials; explicit signup remains separate.
- Status: **implemented and verified** through source linkage and the isolated SQLite invariant.

### PB-INTENT-01 — optional auth discarded booking context

- Journey/role: customer enters through a service/campaign/shared booking URL, then chooses optional login or signup.
- Reproduction: open `/reservar?service=…&utm_source=…#horario`; inspect both auth links. Baseline links used only `/reservar` or the preset path.
- Expected/actual: the safe exact internal target should survive authentication. Actual behavior lost service selection, acquisition context, and hash.
- Evidence: baseline source, Jest, and Playwright link assertions; severity **medium-high**, confidence **high**.
- Cause: links were built from preset configuration rather than router location.
- Remedy/effort/dependencies: pass current path/search/hash through the existing `buildLoginRedirectPath`; small, no new dependency.
- Acceptance: both links retain the entire safe relative target; external redirects remain rejected by the shared sanitizer.
- Status: **implemented and verified** for link generation. Full login/signup round-trip remains untested here.

### PB-COPY-01 — booking promised email and automatic access without backend support

- Journey/role: anonymous booking orientation, contact validation, review, manual-confirmation state, and receipt.
- Reproduction: inspect the baseline page and historical runtime screenshot; copy says TDF will confirm by email and create access/profile automatically. Trace the endpoint: there is no customer mail operation, and created credentials were not delivered.
- Expected/actual: instructions must describe only states and channels the system performs. Actual claims could send customers to a nonexistent inbox message or inaccessible account.
- Evidence: baseline UI, historical screenshot, backend source, current Jest/Playwright/screenshot; severity **high**, confidence **high**.
- Cause: UI copy drifted from backend behavior.
- Remedy/effort/dependencies: explain guest booking and optional accounts, identify email's real purpose, surface the returned booking ID, and use the existing WhatsApp action for adjustments; small.
- Acceptance: no automatic-account/customer-email promise; actual ID remains visible; quote-backed payment wording remains unchanged.
- Status: **implemented and verified** with synthetic desktop and phone journeys.

### PB-IDEMP-01 — legacy tentative creation lacks server idempotency

- Journey/role: anonymous customer retries after a timeout or lost response.
- Reproduction: inspect the OpenAPI route and UI client; unlike `/bookings/public/checkout`, `/bookings/public` accepts no `Idempotency-Key`. The client generates a stable key only for quote-backed checkout.
- Expected/actual: retrying the same request should return the original tentative booking, while reusing a key with a different body should conflict. Actual compatibility endpoint can create another booking after an ambiguous response.
- Evidence: API/client/backend source; severity **high**, confidence **high**. No destructive concurrency reproduction was run.
- Likely cause: the compatibility path predates the canonical checkout runtime.
- Remedy/effort/dependencies: add a versioned idempotency contract and durable unique request receipt, hash canonical request content, replay identical results, reject mismatches, and test concurrent retries on PostgreSQL; medium-large, migration and generated clients required.
- Acceptance: same key/body returns the same booking; same key/different body returns 409; concurrent identical requests produce one booking; missing/invalid key has a documented compatibility policy.
- Status: **deferred** to the highest-priority next transaction-integrity batch. It was not mixed into the credential/copy patch because a client-only key cannot provide the required guarantee.

### PB-RACE-01 — legacy availability check and booking insert are not one serialized exclusion

- Journey/role: two customers request the same no-quote resource/time concurrently.
- Reproduction: source trace shows `resolveResourcesForBooking` checks overlaps, then a later transaction inserts the booking/resource rows. The canonical checkout path has its own atomic exclusion-backed hold.
- Expected/actual: one of two conflicting requests should win atomically. The legacy compatibility path has a check/insert race window.
- Evidence: backend transaction boundaries/source; severity **high**, confidence **medium-high** until PostgreSQL concurrency reproduction.
- Remedy/effort/dependencies: converge on canonical hold infrastructure or lock/constraint the legacy path with deterministic conflict behavior; PostgreSQL fixture and migration review required.
- Acceptance: a concurrent barrier test produces exactly one accepted resource hold and one stable 409, without orphaned rows.
- Status: **deferred** with PB-IDEMP-01; no transaction-safety claim is made for the legacy route.

### PB-MEASURE-01 — booking funnel is not explicitly measured

- Journey/role: all public-booking customers and product operators.
- Reproduction: inspect `docs/analytics.md`, analytics modules, and `PublicBookingPage`; only automatic page views apply, with no reviewed booking-start/submission/server-created events.
- Expected/actual: distinguish valid form progression, server failure, tentative booking creation, checkout creation, and verified payment without PII or duplicate completions. Actual conversion is “not yet measured.”
- Evidence: source/docs; severity **medium**, confidence **high**.
- Remedy/effort/dependencies: define cross-surface event semantics and server/client ownership after the idempotency boundary is reliable; analytics/privacy review and PostHog access required.
- Acceptance: no email/phone/name/notes/token properties; completion means server-created state, not click; replay does not double count; consent and identity reset honored.
- Status: **deferred**. No invented conversion baseline or uplift is reported.

## Design, copy, accessibility, localization, privacy, and performance

The visual system is unchanged: existing typography, semantic success/info states, responsive cards, and brand navigation remain. The change reduces cognitive effort by making guest/account choice explicit and replacing two nonexistent follow-ups with the concrete returned ID and an existing contact action.

The focused Playwright test ran the repository's axe helper and found no serious/critical violations at the final state on desktop or Pixel 7 emulation. Manual screenshot inspection found no visible clipping or overlap at those viewports. Keyboard traversal, focus order through every step, 200% zoom/reflow, contrast measurement, screen-reader announcements, virtual-keyboard behavior, landscape orientation, and physical touch targets were not manually verified; this is not a WCAG 2.2 AA conformance claim.

The page's existing customer copy is hardcoded Spanish rather than routed through the repository localization catalogs. This batch keeps coherent Spanish but therefore cannot provide a supported English equivalent without first migrating the route's full string set. That localization debt is explicit and deferred; no mixed-language string was introduced.

No passwords, tokens, names, emails, phones, notes, booking IDs, or free text were added to analytics or browser persistence. The route redirect contains only the existing relative booking URL and is sanitized by the shared internal-redirect helper. The backend change reduces unnecessary authentication data creation.

No before/after Web Vitals, waterfall, bundle, INP, LCP, CLS, or field p75 measurement was performed. The code adds no dependency, media, request, timer, or rendered component. Performance impact is expected to be negligible but is **not measured**.

## Verification

Executed evidence:

- Initial `npm test --workspace=tdf-hq-ui -- --runTestsByPath src/__tests__/PublicBookingPage.test.tsx`: **pass**, 1 suite / 12 tests against the implemented identity/copy/redirect behavior before the final copy-only “Solicitud registrada” refinement.
- `npm run typecheck --workspace=tdf-hq-ui`: **pass**.
- `npx eslint src/pages/PublicBookingPage.tsx src/__tests__/PublicBookingPage.test.tsx` from `tdf-hq-ui`: **pass**, zero findings.
- Initial `PLAYWRIGHT_ARTIFACT_DIR=artifacts/public-booking-guest-continuity-2026-09-09 npx playwright test e2e/web/persona-public.spec.mjs --grep PW-PER-01-BOOKING --project=chromium-desktop --project=chromium-phone`: **pass**, 2/2 tests in 35.2 seconds after approved local-server binding.
- Final assertion-identical Playwright rerun after the copy refinement, with `--timeout=120000`: **pass**, 2/2 tests in 1.9 minutes. It refreshed both screenshots and the JSON/HTML report. Both runs used mocked public endpoints and included the repository's serious/critical axe scan.
- `stack test --test-arguments='--match=ensurePartyRecord'`: **pass**, 1 example / 0 failures. Stack rebuilt and linked the production and 184-module test executables; existing dependency-bound, missing-home-module, shadowing, partial-function, and unused-import warnings remain visible.
- Initial backend compile attempt: **failed before running tests** because the new test referenced Persistent field constructors without the suite's established `M.` qualifier. The selectors were corrected and the final command above executed the example successfully; this was not a product failure or a passing test.
- `npm run build --workspace=tdf-hq-ui`: **pass**, 12,415 modules transformed; bundle guard passed with 5 preloads / 413,022 gzip bytes initial JavaScript. Vite retained the existing greater-than-500 kB chunk advisory. This preceded the final one-label copy refinement; the final source was subsequently compiled and exercised by Vite in the passing browser run.
- `npm run ai:doctor`: **completed**, 14 OK / 4 warnings / 0 errors as detailed in the capability section.
- `git diff --check`: **pass** after the implementation patch.

A later concurrent duplicate of full-tree UI lint and TypeScript stopped producing output under machine load and was interrupted. TypeScript was then retried alone, remained active at about 22% CPU for a bounded ten-minute window, and was interrupted. These are explicitly recorded as incomplete duplicate checks, not passes. The final typed structure is identical to the source already covered by the successful focused lint, successful standalone TypeScript run, and successful production build/typecheck; only a string literal changed afterward.

The exact Jest suite was also rerun after that string change during the same system load. Its normally one-to-two-second interaction tests stretched to 14–42 seconds: 4/12 completed and 8 failed after the first 15-second timeout caused overlapping React `act()` scopes. A two-test retry likewise timed out at 15.6 and 27.4 seconds. These are retained as **failed harness runs**, not passes; no timeout or assertion was weakened in source. The final label and journey were instead verified by the successful desktop/phone Playwright run using the extended command-line harness timeout. A clean 12/12 final Jest rerun remains a CI/local follow-up.

No full web Jest suite, full backend test suite, mobile suite, PostgreSQL integration, payment-provider sandbox, SMTP delivery, analytics ingestion, staging, deployment, or production validation is claimed. The focused tests and full UI production build are the proportional final checks for this batch.

## Artifacts and changed files

Current runtime artifacts:

- `artifacts/public-booking-guest-continuity-2026-09-09/test-results/persona-public-PW-PER-01-B-38b2c-ustomer-safe-public-actions-chromium-desktop/booking-customer-safe.png`
- `artifacts/public-booking-guest-continuity-2026-09-09/test-results/persona-public-PW-PER-01-B-38b2c-ustomer-safe-public-actions-chromium-phone/booking-customer-safe.png`
- `artifacts/public-booking-guest-continuity-2026-09-09/results.json`
- `artifacts/public-booking-guest-continuity-2026-09-09/html/index.html`

Historical comparison artifacts:

- `artifacts/ux-audit-2026-09-05/after/booking-customer-safe-desktop.png`
- `artifacts/ux-audit-2026-09-05/after/booking-customer-safe-pixel7.png`

Changed implementation/test files:

- `tdf-hq/src/TDF/Server.hs`
- `tdf-hq/test/TDF/ServerSpec.hs`
- `tdf-hq-ui/src/pages/PublicBookingPage.tsx`
- `tdf-hq-ui/src/__tests__/PublicBookingPage.test.tsx`
- `e2e/web/persona-public.spec.mjs`
- this report and its ready-to-use PR description

## Highest-priority next batch

1. Make the legacy tentative route idempotent and resource-conflict-safe on PostgreSQL, or retire it behind the canonical hold path without changing business policy.
2. Exercise the changed endpoint through a real local/staging HTTP stack with a disposable database; verify one Party, zero credentials/policies, one booking, and no customer email side effect.
3. Define privacy-reviewed `booking_started`, `booking_create_failed`, `tentative_booking_created`, and `booking_checkout_created` semantics with exact-once ownership after idempotency is available; establish the baseline rather than inventing uplift.
4. Migrate the complete public-booking string set into the supported Spanish/English localization system.
5. Run the broader slow/offline/session-expiry/back-navigation/repeated-submit matrix, a keyboard/screen-reader walkthrough, and physical mobile-browser enlarged-text/virtual-keyboard checks.

## Handoff status

Branch: `feature/public-booking-truthful-guest-continuity-20260909`.

Implementation, tests, artifacts, and initial report commit: `e0dc4f2e6a517cd7472471d5ab3a90d3523f4a74`.

The feature branch was pushed to `origin`. Draft PR: https://github.com/diegueins680/tdf-app/pull/309, stacked on `feature/onboarding-cross-device-reconciliation-20260909`. This documentation-only handoff update follows the implementation commit. No merge or deployment was performed or authorized.
