# Draft PR: make the Domo fallback accurate, accessible, and browser-verified

## Problem

The public Domo route had no runtime test for its no-authoritative-quote booking fallback. Current browser inspection found that it defaulted a local Ecuador date-time to UTC, lacked contact autocomplete metadata, failed minimum contrast at 20 rendered nodes, and autoplayed/preloaded its hero video without a control.

These defects sit on a public revenue-onboarding path: a visitor could send a request five hours early, face unnecessary data entry effort, struggle to read route content, and incur motion/media transfer before choosing it.

## Changes

- Default the fixed-location fallback to `America/Guayaquil` when no Domo-specific timezone is configured.
- Preserve the same manual-booking payload and idempotency key across an ambiguous retry.
- Add `name`, `email`, and `tel` autocomplete semantics.
- Introduce contrast-safe light/dark foreground variants while retaining Domo brand accents.
- Make hero media poster-first: no autoplay, `preload="none"`, and a visible 44px play/pause control.
- Add a real-browser synthetic Domo journey on desktop, phone, and tablet covering:
  - `503` then successful retry;
  - exact Ecuador-to-UTC conversion;
  - identical request/key reuse;
  - truthful no-hold/no-payment states;
  - keyboard submit and pointer retry;
  - autocomplete, target size, and 320px reflow;
  - zero video request before opt-in and media-control transitions;
  - axe serious/critical scans across all four route themes;
  - final screenshots.
- Add focused unit coverage and richer axe-node diagnostics.

## Evidence

- Pre-fix timezone regression: expected `2030-01-15T15:00:00Z`, received `2030-01-15T10:00:00Z`.
- Pre-fix autocomplete regression: expected `name`, received `null`.
- Pre-fix axe run: one serious contrast rule affecting 20 nodes, with ratios as low as 1.58:1.
- Pre-fix media regression: `Reproducir fondo` control absent.
- Final focused Jest: 1 suite / 3 tests passed.
- Final focused Domo Playwright: 3/3 passed on Chromium desktop/phone/tablet.
- Final complete Playwright: 38 passed / 4 configured skips.
- UI lint: 0 errors / 102 existing warnings.
- TypeScript + production build: passed; 12,415 modules; 413,021 gzip bytes initial JS.
- Repository quality, catalog unit test, and strict catalog audit: passed.
- Full `quality:ui`: not green—185/186 suites passed, but one baseline-identical 24,000-line course-registration suite hit its five-second timeout and cascaded to 168 failures. Domo passed inside the run; isolated reproduction confirms the timing defect.

Detailed findings, coverage, evidence boundaries, screenshots, and deferred acceptance criteria: `reports/domo-fallback-browser-audit-2026-09-10.md`.

## Risks and compatibility

- No business rule, price, payment, availability, API, schema, backend, generated client, role, permission, mobile pointer, analytics, or dependency change.
- The venue remains fully understandable from the poster; visitors must now opt in to motion. If media playback fails, the poster remains but dedicated failure feedback is deferred.
- Contrast values are route-scoped; brand accents remain for identity and actions.
- Browser responses are synthetic interceptions. PR #324 separately verifies the compiled booking handler and PostgreSQL integrity; this PR does not claim one combined end-to-end stack.
- Domo runtime coverage is Chromium-only; the broader critical Firefox/WebKit subset passed but does not include this route.

## Rollback

Revert this branch's commits. No database or generated-client rollback is required. Reverting would restore the five-hour fallback risk, inaccessible contrast, and unsolicited hero media, so keep the new regression tests if the implementation is replaced.

## Remaining gaps

- Connect the browser journey to the real compiled handler and disposable PostgreSQL.
- Verify actual media decoding/failure, screen readers, 200% zoom, physical touch, orientation, and virtual-keyboard behavior.
- Add Domo Firefox/WebKit coverage and interruption/offline continuity.
- Exercise an approved sandbox authoritative-quote provider.
- Establish consented funnel and field Web Vitals measurement; no uplift is claimed.
- Refactor the baseline course-registration timing defect so `quality:ui` is fully green without increasing timeouts.

No merge or production deployment is requested.
