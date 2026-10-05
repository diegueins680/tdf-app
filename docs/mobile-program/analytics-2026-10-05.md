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
Root pins `432b321`, the reviewed mobile main that additionally preserves the
two ticket-contract fields already published in web main; those type-only fields
are the only difference from the runtime source of the signed artifacts.

## Store disclosure audit and account deletion

The 5 October authenticated Play Console audit found the saved declaration still
claims that the app does not allow account creation, despite native password
signup and Google authentication. App activity and device identifiers were not
declared; User IDs had only the app-functionality purpose. These saved answers
must be corrected before distributing the newly configured analytics build.
A build finishing successfully does not resolve this release boundary.

The owner confirmed that `info@tdfrecords.net` will handle account-deletion
requests. All five public mobile legal/support pages now use that real inbox
and the canonical `www.tdfrecords.net` URLs. Legacy Pages URLs remain served.
The existing native About screen links to the deletion page; it now provides
Spanish and English instructions and asks only for the account email and
requested deletion scope, not unnecessary identity or credential data.
Apple App Privacy must also be checked against the new native collection before
the new build is made available. Physical iPhone OAuth evidence remains absent.

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
