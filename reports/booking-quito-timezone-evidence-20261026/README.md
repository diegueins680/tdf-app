# Quito studio booking verification

The owner confirmed 08:00–22:00 America/Guayaquil on 26 October 2026.
Source fingerprints, exact baseline and limitations are in the adjacent JSON report.
All executions here used a base63 checkout with explicitly fingerprinted source
changes; they are not relabeled as a future committed build or production release.

From a checkout with the repository dependencies and pinned browsers installed:

```sh
npm run test:e2e:web -- e2e/web/booking-timezone.spec.mjs --project=chromium-desktop --project=chromium-phone
npm test --workspace=tdf-hq-ui -- --runTestsByPath src/__tests__/PublicBookingPage.test.tsx
```

The canonical Playwright config builds and serves the application. The browser
fixtures intercept every API call and abort external traffic. Each scenario
selects UTC or Europe/Madrid itself, including a July booking with GMT+2 while
the synthetic clock is in December. A synthetic booking409 intentionally leaves
a visible actionable error; no real reservation or payment is created.

`browser-publication-final.json` and publication-prefixed screenshots show the
final four passing Chromium desktop/Pixel7-emulated cases. Earlier results are
retained: locator mistakes, an incorrect error expectation instead of existing
normalization, an implicit duration assumption, and unavailable WebKit. The final
assertions check explicit30-minute input, exact opening/closing values, UTC
availability and submission, correct dated offset labels, zero page errors, and
no false success after409. No timeout, tolerance or automatic retry was increased.

Pinned WebKit cannot install on this macOS12 machine. Linux CI remains required;
no local WebKit/physical-device/human outcome is claimed. The original component
negative control predates the additional dated-offset correction; the final unit
suite was rerun after that correction. Existing booking timestamps are unchanged.
