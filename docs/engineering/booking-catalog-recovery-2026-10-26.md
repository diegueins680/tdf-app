# Recover the public booking catalog without losing the form

An anonymous visitor can explore the studio and reach `/reservar` without an account. If `GET /services/catalog/public` fails after its normal retries, or returns an empty catalog, the form is disabled. The original notice asks the visitor to retry later but has no action. Reloading the page is the only explicit recovery described by the previous contract and may discard contact edits.

## Evidence and priority

- Role/journey: guest or signed-in customer, studio discovery → service selection → schedule.
- Reproduction: open the studio preview, select its reservation link, and simulate HTTP 503 for the public catalog. After automatic retries the contact fields are disabled and the notice has zero buttons. Reproduced in Chromium desktop and Pixel 7 emulation on the actual 7fc9d97 static preview; API responses were synthetic, with no business writes. An empty-catalog component case exercises the same disabled state.
- Severity: blocks service selection for affected visits. Real occurrence frequency and abandonment are unknown; the synthetic reproduction is not production outage evidence.
- Priority model: task impact 3/3 × confidence 1.0 / effort 1 = 3. Effort is one small frontend correction; no comparison with measured conversion or unrelated safety work is claimed.
- Dependencies/risk: existing TanStack Query public catalog and canonical booking guards. Backend/API/SQL and payment authority are unchanged. This does not repair an unavailable server or guarantee that any returned service can be booked.

## Expected behavior

The notice includes **Reintentar catálogo**. Activating it performs the existing catalog read again and shows **Cargando catálogo…** with a disabled busy button. An unavailable form remains disabled until a nonempty successful response arrives. Repeated errors or empty results remain actionable. Contact edits, route presets and the current step remain in the mounted form. Recovery never creates or confirms a reservation, checkout or payment.

The existing `BOOK-SERVICE-PICKER-001` failure/recovery contract is extended explicitly; unrelated requirements keep their complete objects. The change does not need a new API, database schema, dependency or analytics event.

## Acceptance and verification

1. Error and empty responses expose the retry; keyboard activation makes a fresh catalog request.
2. The busy state prevents duplicate activation and retains the unavailable gate.
3. Another empty result keeps the retry; successful canonical data restores the selector and preserves edited contact fields and the route preset.
4. The visitor can continue to the scheduling step without reload after recovery.
5. Booking and checkout mutations remain absent in these recovery tests; existing canonical offering, quote, idempotency and stale-response regression tests remain intact.
6. The new warning and button pass scoped axe serious/critical checks; the scheduling page fits 320 CSS pixels. Browser coverage is synthetic and does not establish human usability, provider correctness or production task success.

Test and delivery receipts are recorded separately with source identities. Implemented, tested, merged, deployed and production-verified remain distinct states.

## Safari resize verification

The first full CI reported 593 passes, 22 skips and one WebKit desktop overflow failure. Its trace and screenshot still show the desktop stepper immediately after resizing to 320px. The test now waits for the existing mobile step label before applying the unchanged zero-overflow assertion. No application code, tolerance, timeout, retry policy or other assertion changed. The original failure and local requalification retain separate source identities in reports/booking-catalog-safari-2026-10-26.json; fresh Linux WebKit CI remains required.
