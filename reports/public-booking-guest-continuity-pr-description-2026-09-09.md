# Draft PR: truthful guest booking identity and return continuity

## Problem

The no-quote public-booking compatibility endpoint silently created a credential with a random password, assigned automatic customer access, discarded the generated credentials, and sent no customer message. The public form nevertheless promised automatic account access and email confirmation. Optional login/signup links also discarded the selected service, campaign query, and hash.

## Changes

- Reuse the established guest-commerce `ensurePartyRecord` path for legacy tentative bookings, leaving explicit account creation to signup.
- Preserve the exact safe public-booking path, query, and hash in optional login and signup links through the shared redirect sanitizer.
- Replace unsupported email/account promises with truthful guest-booking, returned-ID, and existing WhatsApp recovery guidance.
- Add a SQLite guest-identity regression, focused router/copy assertions, and desktop/Pixel 7 Playwright coverage with screenshots and axe checks.
- Add the dated audit with capability, coverage, historical revalidation, evidence limits, and prioritized transaction-integrity/measurement backlog.

## Evidence

- Audit: `reports/public-booking-guest-continuity-audit-2026-09-09.md`
- Focused public-booking Jest initially passed 1 suite / 12 tests; after the final one-label refinement, resource-starved retries timed out and cascaded through React `act()` scopes. No source timeout/assertion was weakened; a clean final Jest rerun remains required in CI.
- UI TypeScript and focused ESLint passed.
- UI production build passed; bundle guard reported 5 preloads / 413,022 gzip bytes initial JavaScript.
- Final public-booking Playwright: 2/2 passed on Chromium desktop and Pixel 7 emulation using synthetic HTTP fixtures and an extended command-line timeout under machine load; assertions were unchanged and real final screenshots were captured.
- Focused backend Hspec: 1 example / 0 failures using isolated SQLite.

## Risks and boundaries

- No schema, OpenAPI response, price, policy, role, permission, payment, feature flag, dependency, mobile pointer, or production state changes.
- The browser run mocks the backend and does not prove PostgreSQL, SMTP, provider, staging, or production behavior.
- The legacy route still lacks durable server idempotency and an atomic exclusion-backed hold. Those require a reviewed contract/migration and are the highest-priority next batch.
- Booking funnel analytics are not yet implemented; no conversion uplift is claimed.
- Public-booking copy remains in the route's existing hardcoded Spanish structure; full Spanish/English catalog migration is deferred.

## Rollback

Revert this focused commit. Do not restore undisclosed credential creation as a long-term behavior; if compatibility requires temporary UI rollback, retain the guest-only backend identity invariant.

## Remaining gaps

- PostgreSQL HTTP/integration and concurrent retry/conflict coverage.
- Durable legacy-route idempotency and serialized resource exclusion.
- Privacy-reviewed booking funnel events and real baseline.
- Real auth round-trip, SMTP/provider sandbox, keyboard/screen-reader, enlarged-text, and physical-device validation.

No merge or deployment is requested by this draft.
