# Consolidated defect and UX report

No finding below is based on human testimony. “Hypothesis” language denotes synthetic-persona heuristic evaluation. Evidence is sanitized and either committed as text/JSON or retained as an ignored/CI artifact.

## PT-001 — Mobile registry promised seven nonexistent native destinations

- **Classification / severity / confidence:** Inconsistent web/mobile behavior; documentation problem; confirmed functional defect — **High** — high confidence.
- **Affected:** PER-01, PER-04, PER-06, PER-17, PER-19 and all mobile discovery users; EP-02, EP-03, EP-04, EP-16; home, search, profile/classified/event/venue detail and classified management/quick-create; iOS/Android.
- **Preconditions:** Generate the mobile registry from baseline `560ac9954`; run the feature audit with the submodule initialized.
- **Reproduction:** `npm run generate:features && npm run audit:features` on the baseline; inspect the reported native destinations under `tdf-mobile/app/`.
- **Expected:** Every `native` destination resolves to an Expo screen; unavailable native functionality has an explicit web fallback or documented exception.
- **Actual / evidence:** The baseline audit rejected seven missing destinations. `RUN-005` independently failed the mobile registry test. No user data was involved.
- **Impact:** A user can be promised a native path that cannot resolve, creating a dead end and undermining parity/feature metrics. The affected flows are high-reach discovery and profile/community entry points.
- **Suspected root cause (inference):** Feature metadata advanced ahead of the Expo route tree and generated-registry validation was not consistently run with the submodule present.
- **Improvement / acceptance:** Initially fixed by assigning truthful `external-web` treatment while the screens were absent. The completed directory work now restores six implemented search/detail/management families to native routes while home remains a truthful web continuation. `RUN-008` must remain green with 137 features/156 web/44 mobile routes.
- **Regression requirement:** `npm run audit:features` and mobile `featureRegistry.test.ts` in CI.
- **Effort / related work:** Small, implemented. Related prior parity discussion: `docs/feature-discoverability-audit/2026-08-06/experimental-and-incomplete-features.md`. No GitHub issue created.

## PT-002 — Malformed catalog response blanked the public app shell

- **Classification / severity / confidence:** Confirmed functional defect; error-recovery problem — **High** — high confidence.
- **Affected:** PER-01, PER-03, PER-15, PER-24, PER-26 and any public/authenticated web user; EP-01, EP-03, EP-16; web browsers.
- **Preconditions:** Return HTTP 200 with `{}` rather than an array for `/catalogs/batch` while loading a public page.
- **Reproduction:** The isolated Playwright mock supplies `{}`; open `/buscar` or a protected intent redirecting to `/login`.
- **Expected:** Locale/theme preferences use safe fallbacks and the page remains usable; optional catalog corruption is observable but not fatal.
- **Actual / evidence:** Direct exploratory run produced a blank app and `.catalogs.find is not a function` in `AppThemeProvider`. The committed E2E test preserves the malformed response; `RUN-010` now passes with no unexpected console error.
- **Impact:** A proxy/backend shape error in optional catalog data can suppress every primary action, including login and public discovery; high reach and severe conversion loss.
- **Suspected root cause (confirmed in source):** Theme and locale contexts assumed catalog arrays without runtime normalization.
- **Improvement / acceptance:** Implemented `Array.isArray` guards and safe emergency theme/locale fallbacks. App shell, heading and interactions must render; axe/console/request assertions must pass.
- **Regression requirement:** `AppThemeProvider.test.tsx` malformed-payload test plus `PW-PER-01-AUTH` and `PW-PER-01-DISCOVERY`.
- **Effort / related work:** Small, implemented. No matching open issue found; no issue created.

## PT-003 — Dark-theme primary actions failed serious contrast checks

- **Classification / severity / confidence:** Accessibility problem; UX problem — **High** — high confidence.
- **Affected:** PER-03 (high contrast/color vision), PER-24 (screen reader/keyboard) and users selecting or inheriting dark mode; EP-01, EP-03, EP-16; login and directory web surfaces.
- **Preconditions:** Playwright `colorScheme: dark`; navigate to login/directory; run axe.
- **Reproduction:** Execute `npm run test:e2e:web` against the pre-fix theme.
- **Expected:** Text and controls have WCAG-suitable contrast in both theme modes and do not rely on color alone.
- **Actual / evidence:** Axe reported serious `color-contrast` violations on primary/secondary controls. After the theme fix, attached `axe-serious-critical.json` files contain `[]`; `RUN-010` passes.
- **Impact:** Low-vision and color-vision users may not perceive essential auth/search calls to action; this affects acquisition and independent task completion.
- **Suspected root cause (confirmed in source):** A dark foreground was not selected consistently for the lightened dark-mode brand colors and contained-button overrides.
- **Improvement / acceptance:** Implemented mode-aware main colors, contrast text and contained overrides. Zero critical/serious axe violations on the covered pages/viewports.
- **Regression requirement:** Playwright axe scan in Chromium desktop/phone/tablet and critical Firefox/WebKit flow.
- **Effort / related work:** Small, implemented. Prior 2026-08-06 audit included accessibility scans but did not cover this malformed-catalog/dark-mode journey.

## PT-004 — Forty backend endpoints lack explicit interface and action disposition

- **Classification / severity / confidence:** Permission/security problem; documentation problem; test-coverage gap — **High** — high confidence about the gap, not a claim of exploitable authorization.
- **Affected:** PER-08, PER-15, PER-16, PER-21, PER-22 and public consumers; EP-03, EP-12, EP-14, EP-15, EP-16; catalog/security revisions, DDEX references, favorites, label notes, Domo, event tickets, records feed, reviews and event research.
- **Preconditions:** Generate the expanded endpoint inventory from current source.
- **Reproduction:** Run `ALLOW_PENDING_FEATURE_DISPOSITIONS=1 npm run generate:feature-audit-reports`; inspect `pending-backend-capabilities.csv`. Run without the opt-in to exercise the failing quality gate.
- **Expected:** Each concrete endpoint has an explicit feature/action or justified API-only/concealed/technical disposition and source-backed handler/record-scope authorization evidence.
- **Actual / evidence:** 40 of 548 endpoints remain “API-only pending explicit product/security disposition”; [current packet](../feature-discoverability-audit/2026-08-21/README.md).
- **Impact:** Coverage and discoverability claims cannot demonstrate backend enforcement for these endpoints. Public routes need validation/rate-limit review; protected catalog/security mutations deserve exact-action and record-scope review.
- **Suspected root cause (inference):** API surface growth after the earlier packet plus a historical parser limitation for parameterized Servant aliases.
- **Improvement / acceptance:** Owners review all 40, map or deliberately classify each, add positive/negative tests, and make the default generator pass without `ALLOW_PENDING_FEATURE_DISPOSITIONS`.
- **Regression requirement:** Default feature-audit generation in repository quality plus handler-level permission tests.
- **Effort / related work:** Medium (2–5 days depending on handler gaps), not implemented because disposition requires product/security ownership. Related: 2026-08-06 architecture/authorization report.

## PT-005 — Historical feature audit no longer described the implemented surface

- **Classification / severity / confidence:** Documentation problem; test-coverage gap — **Medium** — high confidence.
- **Affected:** Release managers, admins, mobile/web owners; every epic relying on inventory and parity.
- **Preconditions / reproduction:** Compare 2026-08-06 generated counts to current route/API/registry source.
- **Expected:** The auditable inventory is regenerable into a current dated packet and reports parser/coverage gaps.
- **Actual / evidence:** Prior packet: 115 features, 125 web routes, 35 mobile routes, 408 endpoints, 2,530 matrix rows. Current packet: 137 features, 156 actual web routes, 44 mobile routes, 548 endpoints, 3,014 rows.
- **Impact:** Release and permission decisions can omit newly implemented functionality or overstate endpoint disposition completeness.
- **Suspected root cause (confirmed/inferred):** Output was pinned to a historical directory, and `WorkItemFilters api` was not expanded correctly.
- **Improvement / acceptance:** Implemented date-configurable output and generic type-parameter substitution; generated the 2026-08-21 packet and preserved pending endpoints as a gate.
- **Regression requirement:** Parser tests should be added if more higher-kinded aliases appear; current generation must retain exact summary counts or produce an reviewed diff.
- **Effort / related work:** Small, implemented. Explicitly updates—not supersedes without acknowledgement—the 2026-08-06 report.

## PT-006 — One mobile checkout test exceeded timeout once

- **Classification / severity / confidence:** Reliability problem; test-coverage problem — **Low** — medium confidence that the initial event was environmental; low confidence in a product defect.
- **Affected:** MOB-PER-02-TICKET-IDEMPOTENCY; EP-09; mobile CI.
- **Preconditions / reproduction:** Initial full serial mobile Jest run after dependency install.
- **Expected:** Ticket checkout component suite completes consistently within configured limits.
- **Actual / evidence:** One test exceeded 5 seconds in `RUN-005`; isolated run passed 14/14 and the complete post-fix run passed 49 suites/256 tests in 33.153 seconds (`RUN-006`, `RUN-011`).
- **Impact:** If recurring, CI noise could hide ticket regressions. No user-facing failure is established.
- **Suspected root cause (inference):** Cold transform/resource pressure during the first full run.
- **Improvement / acceptance:** No retry or timeout inflation added. Retain zero CI retries, track recurrence, and investigate scheduling/open handles if it reappears twice.
- **Regression requirement:** Stable `MOB-PER-02-TICKET-IDEMPOTENCY` identifiers and full serial mobile quality run.
- **Effort / related work:** Tiny monitoring task; no code fix warranted from one non-reproducing event.

## PT-007 — Dependency audit signals remain untriaged

- **Classification / severity / confidence:** Permission/security problem; documentation problem — **Medium** — high confidence in npm's counts, no claim that a production exploit is reachable.
- **Affected:** Web/tooling and mobile dependency graphs; all personas indirectly.
- **Preconditions / reproduction:** Clean `npm ci` and `npm --prefix tdf-mobile ci`.
- **Expected:** Every high advisory is triaged for runtime reachability, patch availability and regression impact, with an upgrade or accepted-risk owner/date.
- **Actual / evidence:** Root install reported 6 moderate/6 high; mobile reported 4 moderate/35 high. No secret or advisory payload is committed.
- **Impact:** Unreviewed transitive vulnerabilities weaken release assurance; indiscriminate upgrades could also break React Native/build tooling.
- **Suspected root cause (inference):** Mature JavaScript/React Native graphs and deferred dependency maintenance.
- **Improvement / acceptance:** Run scoped `npm audit --omit=dev`/SBOM review in CI-capable networking, separate runtime from development exposure, patch safely, and record residual risk.
- **Regression requirement:** Dependency review gate with documented exceptions rather than blind `--force` upgrades.
- **Effort / related work:** Medium and not implemented; requires advisory-level triage.

## PT-008 — DDEX lifecycle is intentionally incomplete

- **Classification / severity / confidence:** Missing functionality; product/revenue opportunity; documentation problem — **High** — high confidence; previously known.
- **Affected:** PER-08 and PER-21; EP-12/EP-14; distribution clients, A&R, LabelRep and administrators; web/mobile/backend.
- **Preconditions / reproduction:** Inspect DDEX handlers and the prior experimental/incomplete report; invoke only in a disposable environment.
- **Expected:** Private storage, safe preview/download, validation, conflict resolution, atomic/idempotent import, partner-profiled export/delivery, acknowledgement/status and rollback/audit.
- **Actual / evidence:** Multiple storage/import/export/read-through handlers return `501`; partner certification and real delivery are absent. See prior `docs/feature-discoverability-audit/2026-08-06/experimental-and-incomplete-features.md` and `docs/revenue-platform/architecture-audit-2026-08-13.md`.
- **Impact:** A label client cannot complete distribution; exposing actions could imply a release was delivered when it was not, with rights/revenue consequences.
- **Suspected root cause:** Explicit staged implementation and missing external partner/storage contracts—not a newly discovered regression.
- **Improvement / acceptance:** Keep incomplete actions concealed; implement private storage, exact permissions, idempotency/rollback, fake-partner contract tests, then certify per partner before production.
- **Regression requirement:** Negative `501`/concealment tests now; full import/delivery/acknowledgement lifecycle tests before enablement.
- **Effort / related work:** Large, not implemented. ADR-0106 and prior audits are authoritative.

## PT-009 — Public pages created duplicate/nested main landmarks

- **Classification / severity / confidence:** Accessibility problem; UX problem — **Medium** — high confidence.
- **Affected:** PER-01, PER-24 and screen-reader/keyboard users; EP-01, EP-03, EP-08, EP-10, EP-16; login, directory detail/search, Domo quote and public tickets.
- **Preconditions / reproduction:** Render a public route inside `PublicBranding`; inspect landmarks or count `main#main-content`.
- **Expected:** One main landmark and one skip-link destination per document.
- **Actual / evidence:** The shell owned `main#main-content`, while child pages declared another `main` or duplicate id. Source inspection and browser snapshot confirmed nesting.
- **Impact:** Landmark navigation can announce ambiguous structure and skip links may target duplicate IDs.
- **Suspected root cause (confirmed):** Page components and their shared public shell both assumed landmark ownership.
- **Improvement / acceptance:** Implemented shell-owned landmark; child pages are neutral containers. E2E asserts exactly one `main#main-content` for auth and directory.
- **Regression requirement:** `PW-PER-01-AUTH`, `PW-PER-01-DISCOVERY`, plus future semantic tests for Domo/tickets/detail.
- **Effort / related work:** Small, implemented. No matching issue found.

## PT-010 — Public critical journeys had no browser-level CI gate

- **Classification / severity / confidence:** Test-coverage gap — **Medium** — high confidence.
- **Affected:** All web personas; EP-01, EP-03, EP-16 initially, with expansion needed across commerce.
- **Preconditions / reproduction:** Inspect baseline package scripts and CI.
- **Expected:** Deterministic browser regression coverage with isolated fixtures, cross-browser critical paths, accessible viewports and retained failure artifacts.
- **Actual / evidence:** Baseline had broad Jest/component coverage but no repository Playwright suite or browser artifact gate.
- **Impact:** Route composition, responsive layout, landmark/contrast problems and runtime API-shape failures can pass unit tests.
- **Suspected root cause (inference):** Earlier investment prioritized component, backend and mobile tests.
- **Improvement / acceptance:** Implemented Playwright fixtures/config, Chromium breadth, Firefox/WebKit critical paths, axe checks, zero retries, failure artifacts and a required CI dependency. `RUN-010` passes.
- **Regression requirement:** `persona-web-e2e` must be selected for repo/UI changes and included in the aggregate `quality` job.
- **Effort / related work:** Medium, implemented for two story slices; commerce expansion remains in backlog.

## PT-011 — Logout cleared the browser cookie without revoking its server token

- **Classification / severity / confidence:** Permission/security problem; confirmed functional defect — **High** — high confidence.
- **Affected:** Every authenticated persona, with direct execution as PER-02; EP-01/ST-004; Customer/Fan and all other roles; web, mobile and API clients using session cookies or bearer tokens.
- **Preconditions:** Start the backend against the isolated PostgreSQL persona database, sign in as the reserved fictional PER-02 account, and retain a copy of the pre-logout cookie.
- **Reproduction:** (1) `POST /login`; (2) confirm `GET /session` resolves PER-02; (3) preserve the cookie value; (4) `POST /session/logout`; (5) resend the preserved cookie to `GET /session`; (6) inspect the matching `api_token.active` state without printing the token.
- **Expected:** Logout expires the browser cookie and invalidates every valid session credential presented with the request; a copied pre-logout cookie cannot resolve a session. Repeated or anonymous logout remains safe and idempotent.
- **Actual / sanitized evidence:** Before the fix, logout returned 200 and the client cookie was cleared, but the copied cookie still resolved PER-02 and its database token remained active. After the fix, the same isolated flow returned `null` for the copied cookie and zero active PER-02 tokens. Raw tokens and the runtime-only password are redacted and the disposable database is destroyed after testing (`RUN-019`, `RUN-020`).
- **User and business impact:** A token copied before logout could continue accessing the account until another mechanism invalidated it, defeating shared-device logout expectations and increasing account/privacy risk for every authenticated workflow, including orders and staff modules.
- **Root cause (confirmed in source):** The logout route accepted neither authentication header nor cookie header, and its handler only emitted an expired-cookie response; it never updated `ApiToken.active`.
- **Proposed improvement / implemented fix:** The logout endpoint now accepts bearer and cookie credentials, parses each independently, deduplicates them, deactivates every matching active token, and always clears the browser cookie. The OpenAPI contract documents bearer/cookie/anonymous idempotent behavior and web/mobile generated types were refreshed.
- **Acceptance criteria:** A request presenting a cookie token, bearer token, both distinct valid tokens, malformed one plus valid other, already-revoked credentials, or no credentials returns 200; every valid presented token is inactive afterward; stale-cookie/session replay returns `null`; no unrelated token is modified.
- **Regression requirement:** `TDF.Server helpers/sessionServer/revokes every valid token presented during logout`, full backend quality, OpenAPI generation, and the disposable PostgreSQL stale-cookie replay check must remain green.
- **Effort / related work:** Small, implemented with no schema migration. No matching issue was created; GitHub mutation still requires separate authorization.

## Consolidated priorities

- **Immediate release gates:** Keep the PT-011 regression green; resolve PT-004 endpoint disposition; keep PT-008 DDEX disabled; complete the remaining provider execution and certification gates in the release report.
- **Short term:** PT-007 advisory triage; broaden PT-010 into auth registration, public ticket, booking, checkout and payment recovery; monitor PT-006.
- **Larger work:** DDEX storage/partner delivery and complete native equivalents for high-demand web fallbacks.
- **Future research:** Run the separate human protocol; validate terminology, trust and conversion hypotheses with real participants before any participant-derived score.
