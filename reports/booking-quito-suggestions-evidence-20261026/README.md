# Quito quick-suggestion regression

The original75 rendered component fails the new test with selectedJanuary15
becomingJanuary14 after one quick-suggestion click. The fixed component passes
all16 form tests. The portable regression also passes under America/Guayaquil.
Eight local browser cases pass with repeated suggestion clicks both at opening
and after visitor-local midnight, preserving every previous boundary and UTC
submission assertion. Every API response is synthetic; no real booking/payment.

Run from the corrected candidate with dependencies/browsers installed:

```sh
TZ=UTC npm test --workspace=tdf-hq-ui -- --runTestsByPath src/__tests__/PublicBookingPage.test.tsx
npm run test:e2e:web -- e2e/web/booking-timezone.spec.mjs --project=chromium-desktop --project=chromium-phone --project=android-chrome-small --project=firefox-critical
```

The recorded local run used fingerprinted working files over source75 before
normalmain4b composition; later Git and hosted-CI identities remain separate.
The two browser-negative starts failed from disk pressure before product execution.
The actual negative control is the rendered-form test, not an inferred browser
failure. Previous75 full-CI success does not prove this correction.
