# Product analytics activation — 5 October 2026

## Verified control plane

The owner explicitly authorized creating TDF Records with `info@tdfrecords.net`
after neither existing Google nor GitHub SSO exposed a project in EU or US.
Email verification is complete. The organization uses the free, capped plan
without a payment card. No teammates were invited.

- Organization: `01a10c55-fb7d-0000-df46-527fb2bf55ad`.
- Project: [TDF Production, EU 294698](https://eu.posthog.com/project/294698).
- Timezone: America/Guayaquil.
- Ingestion: `https://eu.i.posthog.com`.
- Dashboard: [TDF Mobile: testers and feedback](https://eu.posthog.com/project/294698/dashboard/998858).

Project API readback confirms IP anonymization; session recording, heatmaps,
automatic interaction capture, console capture, automatic exceptions and
automatic web-vitals capture are disabled. TDF's existing explicit `web_vital`
event is independent of the disabled PostHog automatic feature.
The dashboard is private and has no public share token. All five saved queries
execute successfully with HTTP200 and uncached results.

## Production web receipt — 18:24 UTC

The earlier disclosure changes reached reviewed main through PR468 alongside
PR485's privacy protection. All five legal/support pages on both canonical and
legacy domains returned HTTP200 and matched `c0a154cd4cbfcd3a5dc5167634be30b7691803cc`.
Cloudflare production variables were saved and read back; retry deployment
`c255006c-7aa2-45e6-a852-aaf96a3695dd` served the same current main. `/app` and the
deployment preview serve matching assets; entry `index-BOFwi28I.js` has SHA256
`9879bd7f1ba91e307fff83419306692b0ce0da7cb55d94903a5ff22c26add679`
and embeds the intended EU public ingestion configuration.

Normal Chrome navigation `/tdf` → invitation → `/app` → iOS → feedback opened
→ TestFlight produced provider-confirmed production events:
`mobile_promo_viewed`, `mobile_testing_interest_clicked`,
`mobile_platform_selected`, `mobile_feedback_opened`, and
`mobile_testing_join_clicked`, with campaign `mobile_activation_20261005`.
The aggregate query found no email, name, password, feedback text, attachment,
message or unexpected token in these QA events. PostHog's `token` property is
its public project ingestion key. No feedback was submitted in this verification.
Clicks do not prove enrollment or installation, and no native receipt is claimed.

## Configuration and verification

| Surface | Public configuration | Release boundary |
| --- | --- | --- |
| Web | Cloudflare production `VITE_POSTHOG_KEY`, `VITE_POSTHOG_HOST` | New production build after privacy protection and disclosure are deployed |
| Signed native lanes | GitHub repository variables `EXPO_PUBLIC_POSTHOG_KEY`, `EXPO_PUBLIC_POSTHOG_HOST` | New signed build |
| EAS production | Same public names and values, project `218aca4d-c096-4892-a353-c1dd7df23448` | EAS production build; no implicit OTA update |

Only the public ingestion key belongs in client configuration. Passwords,
personal API keys and signing credentials are never frontend variables.
Mobile PR135 merged as `6ebe6c5fee3efb8712c673f102c44ec4913710e2`; its 91 suites /
591 tests, release checks and signing checks pass. Jest explicitly removes
the analytics environment variables.

A normal Chrome session on PR485's deployed preview, with an explicit QA
runtime key, produced provider-confirmed `mobile_platform_selected`,
`mobile_feedback_opened` and `mobile_testing_join_clicked` events. This verifies
the transport and provider, not production configuration or an app install.
Playwright is intentionally ignored by the SDK's bot detection. No bot filter
was disabled. Provider verification queries use `refresh: force_blocking` to
avoid mistaking cached results for current ingestion.

The single separately authorized web feedback submission returned HTTP200 and
was confirmed exactly once in production PostgreSQL: consent true, zero
attachments/contact emails/authenticated creator, existing category `idea`
with `kind: general` metadata. No additional test feedback was submitted.
Email inbox delivery is not proven by the API response or database record.

Both signed artifacts completed successfully from `6ebe6c5`:

| Platform | Build | Qualified workflow | Artifact SHA256 |
| --- | --- | --- | --- |
| iOS | 1.0.1 (32) | [37325937942](https://github.com/diegueins680/TDF-mobile/actions/runs/37325937942) | `9819ff6be9b1897c0ec7eeec4f2f5b92f55867b05ec3c880de785ab9d905dd82` |
| Android | 1.0.1 (24) | [37325943662](https://github.com/diegueins680/TDF-mobile/actions/runs/37325943662) | `62836a384ae866297149ffff546e467edce893238203c5b21927f01898409483` |

Downloaded files match their signing receipts and contain the expected public
project key and EU host. This is artifact verification, not store availability.
The web pin additionally preserves generated ticket/deletion contracts and the
shared discovery registry. These generated changes do not alter the exact
source of the already signed artifacts. Mobile PR141 merged the authenticated
receipt contract while preserving current mobile main; discovery synchronization
is reviewed separately in PR142 at `3a6a84ee741ae58a4b6431a8c33fc822a42ab093`.
Its generated-only compatible ancestor is `e6c935e5d717c0e700fede2ec6803294ad232295`;
The preceding discovery head passed 95 suites / 611 tests and Expo Doctor 17/17.
The new audit/CSRF-header contract synchronization passes local typechecking;
its hosted checks must qualify the updated head before merge.
This qualification is separate from the signed-artifact source above.

## Store disclosure audit and account deletion

The 5 October authenticated Play Console audit found the previously saved declaration
claims that the app does not allow account creation, despite native password
signup and Google authentication. App activity and device identifiers were not
declared; User IDs had only the app-functionality purpose. Those answers were corrected and saved for review before distributing the
newly configured analytics build.
A build finishing successfully does not resolve this release boundary.

The owner confirmed that `info@tdfrecords.net` will handle account-deletion
requests. All five public mobile legal/support pages now use that real inbox
and the canonical `www.tdfrecords.net` URLs. Legacy Pages URLs remain served.
The existing native About screen links to the deletion page. This PR changes
that page to lead to `/cuenta/eliminar`: an authenticated, bilingual request for the entire
account without composing an email. The owner processes the request manually;
submission is not completed erasure. See the [operator procedure and identity
checks](account-deletion-operations.md). No actual deletion is claimed by QA. The strict backend endpoint and queue
must be deployed before this new flow is considered operational.
Authenticated App Store Connect readback confirms User ID, Device ID and Product
Interaction are already declared for analytics linked to the user's identity.
The native party selector also emits latency/error classifications: Performance
Data (analytics, linked to identity, not cross-company advertising tracking)
and the analytics purpose for Other Diagnostic Data were published in Apple
App Privacy. Play Diagnostics was saved for review as non-ephemeral collection
for analytics. This does not publish an App Store app version.
Its privacy and privacy-choices URLs now reference the canonical domain; Apple
says these metadata changes accompany the next app version.

The current production rejection is now directly verified: Apple reviewed
1.0.1 (25) on 28 September on iPhone 17 Pro Max / iOS27 and reported a login
failure (2.1) and absence of an equivalent privacy-preserving login option (4.8).
Current native authentication implements password/Google, not a verified Sign in
with Apple flow. Physical iPhone login proof and resolution of 4.8 remain public
App Store release gates; Beta App Review is a separate process. No App Review
resubmission or message to Apple was sent.

Play's corrected account-creation, analytics/feedback data and canonical deletion
answers, plus the canonical privacy URL, are saved for review. The changes do not
mean Google has approved them. Submit them after the updated disclosure deploys.
EAS remote counters were read back as iOS32 / Android24 to avoid reusing numbers.
iOS32 was uploaded through EAS submission `85446118-dba6-457d-93bd-018c39d1d674`
and is VALID/internal, awaiting Beta App Review, with ES/EN test notes saved.
An Android24 edit validated, but Google rejected withholding
review via `changesNotSentForReview=true`; the failed edit was deleted. Retry the
normal existing closed-testing submission only after the disclosure is deployed.

## Dashboard semantics

Five saved views cover web invitation → interest → testing click within seven
days; events by surface; testing clicks by platform; campaign response; and
native first observed launch / feedback. Web views require
`$host = www.tdfrecords.net`, excluding previews. The native view uses
`surface = native_app | profile | about`.

An access request is not admission. A testing link click is not installation.
`mobile_first_open` is the first launch observed by the integration, not a
store-confirmed or web-attributed install; a prior local first-open marker can
exist even when telemetry was unconfigured. Do not join anonymous web/native
identities by inference. The current project does not backfill missing events
from previously released iOS31 / Android23.

## Primary references

- [PostHog project configuration API](https://posthog.com/docs/api/projects).
- [PostHog query API](https://posthog.com/docs/api/query).
- [PostHog dashboard API](https://posthog.com/docs/api/dashboards).
- [PostHog free plan and usage limits](https://posthog.com/pricing).

See the current delivery evidence for deployment receipts and exact signed
artifact sources. Configuration alone is not a successful deployment, store
submission, installation or real native analytics receipt.

## Android review submission — 20:06 UTC

Google's [official account-deletion FAQ](https://support.google.com/googleplay/android-developer/answer/13327111?hl=en) permits an in-app link to a branded web resource with a customer-service email. The currently deployed page and owner-confirmed inbox satisfy that request path independently of Apple's no-email initiation requirement. All ten canonical/legacy legal pages were rechecked against main `c0a154cd4`.

Play received the corrected Data safety and privacy URL declarations plus signed Android24 in the existing closed alpha track. The API validated and committed edit `04449850881532326500` and returned the expected artifact hash. Console shows the three changes in review, initially running quick checks. API `completed` does not establish availability: build23 remains the last verified tester release until Console confirms24 is available. No countries, tester lists or public/open tracks changed. iOS32 still waits for the authenticated deletion flow to be deployed and Beta App Review.

## Android available to selected testers — 21:13 UTC

Authenticated Play Console now shows `1.0.1 (24) - TDF tester analytics` as **Available to selected testers**, released October5 at3:57PM in Console. The channel remains closed alpha in178 countries/regions with the same three admission lists (68/1/11 entries, not unique or opted-in counts). The publishing overview has no pending changes and reports publication. The manifest now identifies24 using that observation; its existing October12 validity deadline is preserved. This establishes closed-track availability, not public production, installation or native event reception. Signed source/hash remain the24 receipt above. A fresh21:10UTC ASC read confirms31 BETA_APPROVED,32 READY_FOR_BETA_SUBMISSION and AppStore REJECTED/MANUAL.
