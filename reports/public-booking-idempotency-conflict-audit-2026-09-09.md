# TDF public-booking idempotency and conflict audit — 2026-09-09

## Outcome

This bounded transaction-integrity batch follows `reports/public-booking-guest-continuity-audit-2026-09-09.md` and completes its highest-priority deferred booking work. The unauthenticated no-price compatibility route now:

1. requires a validated `Idempotency-Key`;
2. stores only that key, a SHA-256 normalized-request fingerprint, and the resulting booking reference;
3. serializes equal keys, returns the original booking for an identical replay, and returns `409` when the key is reused with different input;
4. creates the guest Party, booking, resource relations, and replay receipt in one transaction;
5. converts the existing PostgreSQL resource-exclusion violation (`23P01`) into the documented `409` response; and
6. keeps the same payload-scoped key through ordinary retries and same-tab reload/back attempts without storing name, email, phone, or notes in browser storage.

Both first-party web callers and the generated web/native contracts were updated. No price, deposit, cancellation policy, permission, role assignment, payment state, feature flag, account-creation behavior, analytics payload, outbound communication, or production data changed. No merge or deployment occurred.

## Capability and safety matrix

| Capability | Status | Evidence from an actual check | Consequence |
|---|---|---|---|
| Repository read/write | Available | Implementation, SQL, tests, generated contracts, artifacts, and this report were changed only in `/private/tmp/tdf-event-save-publish-20260907` | The dirty primary checkout remained untouched |
| Dedicated branch and baseline | Available | Root branch `feature/public-booking-idempotency-conflicts-20260909` began at clean commit `8229ff8ad3bf6cce46e7107d873c4137fd7de36d` | This is a focused stack on the truthful guest-booking batch |
| Unrelated-work protection | Available | Isolated-worktree baseline was clean; explicit path staging was used | No known unrelated change is included |
| Required runtimes/package managers | Available | Node `v24.8.0`, npm `11.6.0`, Stack `3.7.1`, Docker, PostgreSQL 17 image, Jest, ESLint, TypeScript, Hspec, Vite, and Playwright commands executed | Web, contract, backend compile, and disposable-database validation are supported |
| Backend/database | Partial | The production/test Haskell executables compiled; real PostgreSQL migration rehearsals ran in disposable Docker containers | Database invariants are verified; a configured full HTTP server/database fixture was not available |
| Browser/device tooling | Available | Playwright ran Chromium desktop and the repository phone project against local Vite | The synthetic failure/retry/success journey was exercised at both viewports |
| Screenshot capability | Available | Two real post-submit screenshots were captured and visually inspected | Current responsive evidence is retained; no fictitious baseline image was created |
| Native mobile submodule | Available | Required API generation, lint, and typecheck ran; mobile commit `57abd23c0522133274e5bc2aa1a7e524d94dd62b` was pushed and verified with `git ls-remote` | Parent pointer references a remotely available contract commit; no native booking screen was found |
| Network/GitHub | Available for requested operations | Root/mobile branch pushes and authorized draft-PR creation succeeded; `npm run ai:doctor` still reports the stored `gh` token as invalid | Both review paths are published; the contradictory status diagnostic remains documented rather than treated as stronger evidence than successful operations |
| Local/staging configuration | Partial | Local UI used synthetic HTTP fixtures; README-referenced full backend default configuration was absent from this worktree | No staging persistence, external provider, SMTP, or end-to-end HTTP claim is made |
| Synthetic accounts | Unavailable for this batch | The route is anonymous and used synthetic contact data; no configured authenticated/staging account was found | Account/session authorization was not part of runtime validation |
| Analytics access | Unavailable | No PostHog project or representative field dataset was available | Funnel baselines and conversion impact remain “not yet measured” |
| Production access | Not used | Only the local release planner's non-mutating mode was invoked; no production URL, database, transaction, message, deployment, or customer account was invoked | All evidence is local, synthetic, source-based, or disposable-database evidence |

The documented `npm run ai:doctor` preflight completed with **14 OK, 4 warnings, 0 errors**. Its warnings are missing isolated-worktree daily memory files, expected task-owned dirty state, and invalid stored GitHub CLI authentication. No background supervisor or indefinite loop was started.

## Baseline and methodology

- Root baseline: `8229ff8ad3bf6cce46e7107d873c4137fd7de36d` (`docs: record public booking review handoff`).
- Mobile baseline: `f487de478939b3c19a62b5f54aa876d96a4eb32c`.
- Environment: macOS, America/Guayaquil; local Vite; Chromium desktop and phone emulation; synthetic 2030 booking; mocked browser HTTP; disposable PostgreSQL 17.
- Methods: historical finding revalidation, API/backend/client control-flow trace, database constraint/trigger inspection, task-oriented cognitive walkthrough, generated-contract comparison, focused Jest/Hspec checks, real PostgreSQL migration rehearsals, Playwright retry exercise, automated serious/critical axe scan inherited by the persona test, and manual screenshot inspection.
- Evidence boundary: expert assessment and synthetic runtime testing are not user research. No interview, participant quote, real conversion metric, field percentile, staging transaction, payment, mail delivery, or production validation is claimed.

## Coverage matrix

| Route/surface | Role | Device and important states | Inspection method | Status |
|---|---|---|---|---|
| `/reservar?service=synthetic-studio-session…` | Anonymous prospective customer | Desktop; catalog/availability loaded; first POST `503`; retry; success | Playwright, request inspection, axe, screenshot | **Verified**: 2 POSTs reused one key and success followed the successful response |
| Same `/reservar` journey | Anonymous prospective customer | Phone project; same failure/retry/success sequence | Playwright, request inspection, axe, screenshot | **Verified** |
| Public booking key continuity | Anonymous prospective customer | Same payload across independent helper calls representing reload/back; storage available | Jest with deterministic Web Crypto digest and sessionStorage inspection | **Verified**: same key; stored key name is a hash and contains no contact text |
| `POST /bookings/public` | Anonymous customer | Missing/invalid key; identical replay; changed-payload key reuse; resource conflict | OpenAPI/client/backend source, compiled handler, PostgreSQL schema invariants | **Implemented; HTTP integration unverified** |
| Guest identity creation | Anonymous customer | New/repeated contact under booking transaction | Refactored shared helper plus focused Hspec | **Verified** for helper: 1 example / 0 failures; PostgreSQL handler transaction not invoked through HTTP |
| `service_booking_tentative_request` | System/database | First/repeated migration; key uniqueness; one-key-per-booking; malformed key; rollback | Disposable PostgreSQL 17 rehearsal | **Verified** |
| Existing resource allocation | Anonymous/customer/staff booking writers | Overlapping legacy `booking_resource` insert | Existing disposable PostgreSQL rehearsal | **Verified**: second overlapping insert rejected |
| `/domo-del-pululahua` legacy fallback | Anonymous prospective customer | No authoritative quote; tentative submission | Source/typecheck and existing page suite compile | **Implemented; submission not runtime-exercised in this batch** |
| Quote-backed `/bookings/public/checkout` | Anonymous customer | Existing idempotent paid-hold path | Regression source/build and focused public-booking tests | Unchanged; mocked unit coverage only in this batch |
| Native mobile | Any | Generated OpenAPI surface | Required generation, lint, typecheck | **Verified contract only**; no native public-booking screen exists |
| Full HTTP process, concurrent handler calls, slow/offline, cross-device guest retry | Anonymous customer | Runtime | No configured fixture | **Blocked/unverified** |
| Payment provider, SMTP, analytics ingestion, staging/production | Customer/operators | External states | Not invoked | **Skipped by scope/safety** |

## Historical finding revalidation

Historical reports remain unchanged.

| Finding | Current classification | Evidence |
|---|---|---|
| `PB-IDEMP-01`: tentative route lacked server idempotency | **Still present at baseline; now implemented** | Baseline API/client/backend accepted no key. Current migration, handler, clients, generated contract, Jest, and browser retry cover the new contract within stated limits |
| `PB-RACE-01`: legacy availability and insert lacked serialized exclusion | **Superseded; earlier cause statement was incomplete** | `2026-08-16_service_booking_checkout_runtime.sql` already installs `service_booking_resource_allocation`, a GiST exclusion constraint, and an `AFTER INSERT` trigger on every `booking_resource`; its PostgreSQL rehearsal rejects an overlapping legacy insert |
| Documented `409` for overlapping tentative booking | **Still defective at baseline; now implemented** | The existing canonical checkout caught SQLSTATE `23P01`; the tentative handler did not. It now rolls back and maps that constraint failure to `409` |
| `PB-GUEST-01`: anonymous route created inaccessible credentials | **Still resolved** | The transaction calls the Party-only helper introduced by the preceding batch; focused helper Hspec passes |
| `PB-COPY-01`: unsupported email/account promises | **Still resolved** | Current desktop/phone screenshots retain the truthful “Solicitud registrada”/booking-ID state |
| `REV-01`: receipt linked to staff calendar | **Still resolved** | Both browser runs confirm the customer-safe receipt and no staff-calendar action |
| Directory-event migration manifest ancestry | **Pre-existing release blocker found and corrected** | Manifest pointed to non-ancestor `dfea3d0…`; current history introduced byte-identical SQL at `7e2a82d…`. SHA-256 comparison matched and a full manifest ancestry scan found no second mismatch |
| Paused `single-feature-onboarding-v1` experiment | **Unchanged** | This batch does not read, expose, or modify the experiment or flags |

## Findings and implementation status

### PB-IDEMP-01 — duplicate tentative booking after an ambiguous response

- Journey/role: anonymous customer submitting a no-price booking.
- Reproduction at baseline: send `POST /bookings/public`, lose the response, then retry. The endpoint accepted no idempotency header and could create another Party-linked booking/resource row.
- Expected/actual: equal key and normalized payload must return one booking; changed input under that key must conflict. Baseline could create multiple bookings.
- Evidence/severity/confidence: API/client/backend source; **high**, **high**. Observed impact is duplicate-write risk; production frequency is unknown.
- Cause: compatibility endpoint predated the canonical checkout replay receipt.
- Remedy/effort/dependencies: required key, SHA-256 normalized fingerprint, advisory lock, durable receipt table, generated clients, first-party key continuity; medium, additive migration.
- Acceptance: one durable key maps to one booking; identical replay returns it; changed payload produces `409`; retry does not resend engineer notification; no PII is stored in the replay table/browser key name.
- Status: **implemented**. Client retry and database invariants are verified; full HTTP/concurrent handler verification remains outstanding.

### PB-CONFLICT-01 — database exclusion could surface as `500`

- Journey/role: two customers choose an overlapping resource/time.
- Reproduction at baseline: the trigger-backed allocation insert raises PostgreSQL SQLSTATE `23P01`; unlike the canonical checkout function, the tentative handler had no exception mapping.
- Expected/actual: losing request receives the documented conflict response and no partial writes. Baseline could expose an internal error despite the database correctly rejecting the overlap.
- Evidence/severity/confidence: source comparison plus real PostgreSQL rejection; **high**, **high** for cause. The exact old HTTP status was not reproduced through a configured server.
- Remedy/effort/dependencies: keep the existing exclusion invariant; run Party/booking/resource/receipt writes transactionally; catch `23P01` after rollback and return `409`.
- Acceptance: overlapping resource allocation leaves no new booking, Party update, resource relation, or replay receipt and returns stable conflict semantics.
- Status: **implemented**; rollback behavior follows the transaction exception boundary and PostgreSQL invariant, but exact HTTP response remains unverified end to end.

### PB-RETRY-01 — client key did not survive reload/back

- Journey/role: customer retries after a dropped/ambiguous response or reloads the form.
- Reproduction at baseline: tentative path had no key; the existing checkout key lived only in a React ref.
- Expected/actual: identical session/payload should reuse its key without persisting contact data. Baseline generated a new key after remount.
- Evidence/severity/confidence: client source and Jest storage test; **medium-high**, **high**.
- Remedy/effort/dependencies: SHA-256 the serialized payload with Web Crypto, use only that digest as the sessionStorage lookup suffix, and degrade to in-memory/new-key behavior if Web Crypto or storage is unavailable. Server persistence remains authoritative.
- Acceptance: repeat helper calls use one key; different checkout/tentative scopes do not collide; storage contains no raw contact payload; storage failure does not block booking.
- Status: **implemented and unit-verified**. Same-tab persistence is not cross-device continuity and is not described as such.

### PB-COMPAT-01 — required header affects ungenerated external clients

- Journey/role: any external caller using the legacy route directly.
- Reproduction: call the updated endpoint without `Idempotency-Key`.
- Expected/actual: transaction-integrity guarantee requires a stable caller key; older callers sent none.
- Evidence/severity/confidence: OpenAPI/client inventory; **medium**, **high**. No external consumer inventory or traffic data is available.
- Remedy/effort/dependencies: publish the required OpenAPI parameter and update generated clients/known first-party callers; communicate before rollout.
- Acceptance: known callers send 16–128 visible ASCII characters; unknown consumers receive documented `400`, not duplicate writes.
- Status: **implemented for known first-party/generated clients; rollout communication deferred**.

### PB-MEASURE-01 — booking funnel remains not yet measured

- Journey/role: customers and product operators.
- Evidence/severity/confidence: current analytics source/docs; **medium**, **high**.
- Impact: progression, retry, conflict, failure, and created-booking rates cannot be established. No uplift percentage is available.
- Remedy/dependencies: define privacy-reviewed events with server-created completion ownership and replay deduplication; analytics access and consent review required.
- Acceptance: no contact/free-text/token fields; click is not completion; replay cannot double-count; anonymous/auth identity boundaries remain correct.
- Status: **deferred**. No new event was added without a valid measurement contract.

## Engineering and data rationale

The implementation reuses the existing Servant route, request type, booking model, guest Party helper, resource allocation trigger, SHA-256 helper, OpenAPI generator, and client request infrastructure. It adds no package and does not fork the design system or checkout architecture.

The additive table intentionally contains no request JSON or contact field. `booking_id` is unique and `ON DELETE RESTRICT`; silently deleting the receipt would allow an old retry to create a second booking. For the same reason, the supplied rollback is non-destructive: older application code ignores the table safely, while preserving it maintains replay evidence. Operational rollback should revert application traffic first and retain the table until its records age beyond a separately approved policy; no retention policy is invented here.

The browser cache is only a continuity aid. The server table and advisory transaction are the correctness boundary. Browser storage failure degrades safely to a new key rather than blocking a booking, and only a scoped SHA-256 lookup name plus random idempotency value is stored.

## Design, copy, localization, accessibility, privacy, and performance

This is a behavioral/API batch; no layout, visual token, navigation, price copy, or localized string changed. Existing Spanish-first copy and current English-localization debt remain as recorded in the preceding audit.

The browser test executed the repository’s serious/critical axe scan and found no such violation at the success state on desktop or phone. Both screenshots were manually inspected for clipping and overlap. No screen-reader, complete keyboard/focus, 200% zoom/reflow, reduced-motion, virtual-keyboard, orientation, or physical-touch test was performed; this is not a WCAG 2.2 AA conformance claim.

No name, email, phone, note, password, token, request body, or booking ID was added to analytics. The server receipt stores only key/hash/reference; sessionStorage uses only a scope/hash name and the generated key. Production data was not inspected.

No Web Vitals or field p75 dataset was available. The batch adds no dependency, media, render path, or additional normal-path HTTP request. The production build passed its existing initial-bundle guard at **5 preloads / 413,015 gzip bytes initial JavaScript**. Vite retained the repository’s existing greater-than-500 kB chunk advisory. LCP, INP, CLS, and conversion impact are **not measured**.

## Verification performed

- `npm run ai:doctor`: **completed**, 14 OK / 4 warnings / 0 errors.
- `REQUIRE_MOBILE_WORKSPACE=1 npm run generate:api`: **passed**, web and mobile generated from `tdf-hq/docs/openapi/api.yaml`.
- Focused ESLint for the six affected web source/test files: **passed**, zero findings.
- `npm test --workspace=tdf-hq-ui -- --runTestsByPath src/api/queryValidation.test.ts src/__tests__/PublicBookingPage.test.tsx src/pages/DomoVenuePage.test.tsx`: **passed**, 3 suites / 28 tests. An earlier run passed 27 existing tests but the new storage test failed before execution because jsdom exposes no `crypto.subtle`; the test was corrected with a deterministic Web Crypto stub, without weakening product assertions.
- `npm run typecheck:ui`: **passed**.
- `REQUIRE_MOBILE_WORKSPACE=1 npm run lint:mobile`: **passed**.
- `REQUIRE_MOBILE_WORKSPACE=1 npm run typecheck:mobile`: **passed**.
- `npm run build --workspace=tdf-hq-ui`: **passed**, 12,415 modules transformed; initial-bundle guard passed at 413,015 gzip bytes.
- `stack test --test-arguments='--match=PublicBookingReq'`: **passed**, 3 examples / 0 failures after compiling/linking production and the 184-module test executable.
- Final `stack test --test-arguments='--match=ensurePartyRecord'`: **passed**, 1 example / 0 failures after the transactional helper refactor. Existing Cabal missing-home-module, shadowing, unused-import, and linker warnings remain.
- `npm run test:public-booking-tentative-migration`: **passed** on disposable PostgreSQL 17. It applied the migration twice, verified key/hash/booking constraints, and verified the intentionally non-destructive rollback.
- `npm run test:service-booking-migration`: **passed** on disposable PostgreSQL 17. Its deliberate overlapping legacy insert was rejected by the existing allocation exclusion.
- `npm run test:production-release`: **passed**, 49/49 tests, after registering the migration with its immutable introducing commit SHA.
- `npm run test:ci-pipeline`: **passed**, 16/16 tests, including OpenAPI/generated-client and migration change-scope behavior.
- `npm run test:catalog-list-audit && npm run audit:catalog-lists`: **passed** after the first PR run exposed two new candidate fingerprints and one stale fingerprint. The fix preserves the reviewed deployment-registry decision under its new ID and classifies booking request keys as retained API mechanics; no catalog authority moved into code.
- `npm run release:backend:plan -- --sha 2b7fb46a122710ac342fd973683ba32ff22f5d33`: **passed** in non-mutating dry-run mode after correcting the pre-existing directory-migration ancestry metadata. It returned an empty command list and marked every planned release step `mutating: false`.
- `PLAYWRIGHT_ARTIFACT_DIR=artifacts/public-booking-idempotency-conflict-2026-09-09 npx playwright test e2e/web/persona-public.spec.mjs --grep PW-PER-01-BOOKING --project=chromium-desktop --project=chromium-phone`: **passed**, 2/2 in 14.6 seconds. Each fixture returned one synthetic `503`, then success, and asserted exact key reuse across both requests.
- `git diff --check` / staged diff check: **passed** at implementation commit.
- Mobile `git ls-remote`: **passed**, confirming `57abd23c0522133274e5bc2aa1a7e524d94dd62b` on `origin/feature/public-booking-idempotency-contract-20260909`.

No test was disabled, timeout raised in source, assertion weakened, or empty collection reported as a pass. The first focused lint invocation used a nonexistent workspace-local binary and exited `127`; it ran no lint. The corrected root-workspace invocation passed.

A later duplicate `npm run lint:ui && npm test --workspace=tdf-hq-ui` was stopped after the full-tree ESLint phase had run for more than eleven minutes under shared-machine contention. A process check showed ESLint active at roughly 30–44% CPU, but the bounded duplicate check had not exited and Jest had not started. It is recorded as **interrupted/incomplete**, not as a lint pass, test failure, or empty collection; the focused lint, focused 28-test collection, TypeScript check, production build, and browser tests above did complete.

Unverified/skipped: full web/mobile/backend suites; actual Servant HTTP request/replay/concurrent barrier; configured full local backend; staging; real payment; SMTP; analytics ingestion; screen reader; physical device; field performance; deployment and production.

## Artifacts and changed files

Current synthetic runtime artifacts:

- `artifacts/public-booking-idempotency-conflict-2026-09-09/test-results/persona-public-PW-PER-01-B-38b2c-ustomer-safe-public-actions-chromium-desktop/booking-customer-safe.png`
- `artifacts/public-booking-idempotency-conflict-2026-09-09/test-results/persona-public-PW-PER-01-B-38b2c-ustomer-safe-public-actions-chromium-phone/booking-customer-safe.png`
- `artifacts/public-booking-idempotency-conflict-2026-09-09/results.json`
- `artifacts/public-booking-idempotency-conflict-2026-09-09/html/index.html`

Implementation and contract:

- `tdf-hq/src/TDF/API.hs`
- `tdf-hq/src/TDF/Server.hs`
- `tdf-hq/docs/openapi/api.yaml`
- `tdf-hq/sql/2026-09-09_public_booking_tentative_idempotency.sql`
- `tdf-hq/sql/2026-09-09_public_booking_tentative_idempotency_rollback.sql`
- `tdf-hq-ui/src/api/bookings.ts`
- `tdf-hq-ui/src/api/generated/types.ts`
- `tdf-hq-ui/src/pages/PublicBookingPage.tsx`
- `tdf-hq-ui/src/pages/DomoVenuePage.tsx`
- mobile pointer to generated-contract commit above

Regression/release evidence:

- `tdf-hq-ui/src/api/queryValidation.test.ts`
- `tdf-hq-ui/src/__tests__/PublicBookingPage.test.tsx`
- `tdf-hq-ui/src/pages/DomoVenuePage.test.tsx`
- `e2e/web/persona-public.spec.mjs`
- `scripts/test-public-booking-tentative-idempotency-migration.sh`
- `scripts/production-migrations.json`
- `docs/catalog-persistence/catalog-list-decisions.json`
- `.github/workflows/ci.yml`
- `package.json`
- this report and its ready-to-use draft-PR description

## Deferred work and next batch

1. **Real HTTP/concurrency test** — impact: strongest remaining transaction-integrity uncertainty; dependency: disposable configured PostgreSQL-backed server fixture; reason deferred: the documented local server configuration is incomplete in this worktree; acceptance: two barrier-synchronized equal requests yield one booking and identical response, changed payload yields `409`, overlapping resources yield one success/one `409`, and no orphan Party/resource/receipt remains.
2. **External-client rollout inventory** — impact: older consumers without the newly required header receive `400`; dependency: API consumer/traffic ownership; acceptance: every active caller is identified, migrated, or explicitly retired before rollout.
3. **Booking funnel measurement** — impact: abandonment/retry/conflict rates remain unknown; dependency: privacy/consent review and analytics access; acceptance as `PB-MEASURE-01` above.
4. **Domo fallback runtime case** — impact: a less common first-party compatibility caller is source/type verified only; dependency: synthetic no-authoritative-quote fixture; acceptance: failed/retried tentative submission reuses one key and never claims a hold/payment.
5. **Broader accessibility/resilience** — impact: screen-reader, full keyboard/focus, zoom, offline, and physical-device gaps remain; acceptance: task script covers those states without duplicate writes or obscured controls.

## Handoff status

- Root branch: `feature/public-booking-idempotency-conflicts-20260909`.
- Root implementation commit: `2da5c939baca7cdeec8aba623a83e9b42440f2e3`.
- Root evidence/manifest commit: `2b7fb46a122710ac342fd973683ba32ff22f5d33`.
- Root catalog-audit CI fix: `2425821ce` (`docs: classify booking contract constants`).
- Mobile branch: `feature/public-booking-idempotency-contract-20260909`.
- Mobile commit: `57abd23c0522133274e5bc2aa1a7e524d94dd62b` (pushed and remotely verified).
- Root draft PR: https://github.com/diegueins680/tdf-app/pull/318, stacked on `feature/public-booking-truthful-guest-continuity-20260909`.
- Mobile draft PR: https://github.com/diegueins680/TDF-mobile/pull/71.
- A documentation-only handoff update follows the evidence commit above.

No merge, deployment, production mutation, real transaction, or customer communication was performed.
