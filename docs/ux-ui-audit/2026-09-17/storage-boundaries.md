# Optional browser caches and calendar diagnostics — 2026-09-18

Continues stable findings UX-260917-007 and UX-260917-044. Baseline root2ce7a1a0d.
An actual isolated Admin signup/session and PostgreSQL17 role fixture reproduced nine
failed arrivals across diagnostics, trial lessons, private service tracking and Datafast
return when browser storage getters/reads/writes were denied. Route error boundaries
caught these errors, so a zero `pageerror` count alone did not establish usable arrival.

## Causes and final changes

- Trial lesson preferences are optional. Guarded reads retain the existing default date
  range; failed writes leave active filters in memory. A component test changes a date
  under each denial and verifies the resulting API query.
- Private service pages recover from missing/unreadable cached lookup tokens through
  their existing recovery UI. They never generate substitute authority or confirm a
  payment without a supplied token/reference. Tracking fragment precedence is retained.
- Diagnostics no longer treats global localStorage calendar IDs/timestamps as connection
  or synchronization facts. Its action opens the existing calendar page, which owns the
  authoritative configuration/last-sync display after PR445/444. The diagnostic page
  itself makes no provider-health, connection or disconnection assertion.
- The first rebuilt application still failed eight authenticated arrivals despite the
  page fixes. The captured component stack identified RadioWidget, which is mounted
  after authentication and had additional unguarded preference reads. All radio cache
  reads/writes now use the same best-effort boundary; volume/mute and other preferences
  remain in memory. Radio presence/broadcast APIs and media permissions are unchanged.

## Executed evidence

- Forty focused tests pass across six suites; the seven trial tests were additionally
  rerun after adding actual filter-edit assertions. Includes radio mute interaction under
  getter/getItem/setItem denial, diagnostics with old markers, service no-token refusal,
  valid fragment precedence, and existing radio routing/API behavior.
- Final production bundle:48 actual isolated Admin/cookie/API arrival cases pass, four
  routes × four storage conditions × Chromium/Firefox/WebKit,768CSSpx. No route fallback,
  page error or Datafast mutation. This is real isolated auth/PostgreSQL evidence with
  empty business data, not production payment/provider or populated-calendar evidence.
- Twenty anonymous production-bundle browser cases pass across five profiles/three engines
  with controlled APIs. These ran on the page-fix bundle before the subsequent authenticated
  radio fix; the final48 cover the additional shared widget. The source spec remains in CI.
- TypeScript, lint, production build pass; final initial JS365673gzip bytes/fivepreloads.
  Strict catalog gate passes1138reviewed candidates with pinned mobile4122bb75.
- An intermediate rebuilt bundle still failed on RadioWidget; it is explicitly excluded
  from passing evidence. A filter test initially clicked before the empty-state button
  rendered; it was corrected to wait for the visible state and rerun successfully.

## Executable formal contract

`OptionalTokenRecovery.tla`, TLC1.7.2:32initial combinations of tracking/return, two
fragment/cache values, denied/available cache and required reference presence.96generated
and96distinct states,depth3. `NoRequestWithoutToken`, `NoInventedToken`, fragment precedence
and conditional `RecoverySettles` pass. Weak fairness assumes the synchronous resolve and
dispatch actions execute; browser storage is assumed to return or throw, not hang forever.
Terminal quiescence is permitted. The unguarded-storage negative control explores84states
and violates liveness; cache-first behavior violates fragment precedence at41states. Both
must fail their named checks. The full pinned TLC/Alloy runner also passes.

Conformance: helper/storage lookup → Resolve; fragment-first tracking lookup → precedence;
existing query/effect guards → Dispatch. Component and browser regressions reproduce denied
storage and missing/private fragment cases. Token presence is not proof of server permission
or token validity; this model certifies no cryptography, provider settlement, session ownership,
order persistence or unrelated handlers. Existing server checks remain authoritative.

## Release and remaining scope

No migration, payment setting, actual charge or external provider operation was changed.
This focused branch is published for integration with the other pending UX increments.
Independent review, combined gates, merge and production acceptance remain pending.
Calendar details rely on444/445's corrected authoritative display. Other storage hypotheses
in checkout/idempotency and RSVP-intent code remain unverified and must be reproduced before
being promoted to mandatory findings; no claim of exhaustive application coverage is made.
